library(tidyverse)
library(rstatix)
library(lmtest)
library(sandwich)
library(xtable)

alpha  <- 0.05
n_boot <- 5000
seed   <- 123

decisions <- read.csv(file.path("analysis", "decisions.csv"), sep = ",", header = TRUE)
method <- decisions$method
names(method) <- decisions$analysis
method

dir_tab <- file.path("report", "tables")
dir_fig <- file.path("report", "figures")
for (d in c(dir_tab, dir_fig)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

fmt   <- function(x, d = 2) formatC(x, format = "f", digits = d)
fmt_p <- function(p) ifelse(p < 0.001, "$<$.001", gsub("^0", "", formatC(p, format = "f", digits = 3)))
ci_tx <- function(lo, hi, d = 2) paste0("[", fmt(lo, d), ", ", fmt(hi, d), "]")

save_table <- function(tab, name) {
  print(xtable(tab), file = file.path(dir_tab, paste0(name, "_table.tex")),
        include.rownames = FALSE, booktabs = TRUE, size = "footnotesize")
}
save_fig <- function(p, name, w = 6, h = 4) {
  ggsave(file.path(dir_fig, paste0(name, "_figure.pdf")), p, width = w, height = h)
}

if (!exists("summary_rows")) summary_rows <- list()
add_summary <- function(id, question, estimate, test, p) {
  summary_rows[[id]] <<- data.frame(Analysis = id, Question = question, Estimate = estimate,
                                    Test = test, p = fmt_p(p))
}

boot_ci <- function(x, stat = mean) {
  x <- x[!is.na(x)]
  set.seed(seed)
  b <- replicate(n_boot, stat(sample(x, length(x), replace = TRUE)))
  quantile(b, c(0.025, 0.975))
}
boot_diff_ci <- function(x, y, stat = mean) {
  set.seed(seed)
  b <- replicate(n_boot, stat(sample(x, length(x), replace = TRUE)) -
                         stat(sample(y, length(y), replace = TRUE)))
  quantile(b, c(0.025, 0.975))
}

group_ci <- function(d, y, g) {
  d$grp <- d[[g]]
  d$val <- d[[y]]
  d %>% group_by(grp) %>%
    summarise(n = n(), m = mean(val), se = sd(val) / sqrt(n),
              lower = m - qt(0.975, n - 1) * se, upper = m + qt(0.975, n - 1) * se, .groups = "drop")
}

choose_vcov <- function(fit) {
  bp <- bptest(fit)
  robust <- bp$p.value < alpha
  list(v = if (robust) vcovHC(fit, type = "HC3") else vcov(fit), robust = robust, bp_p = bp$p.value)
}

reg_table <- function(fit, vc, label = "") {
  ct <- coeftest(fit, vcov. = vc$v)
  ci <- coefci(fit, vcov. = vc$v)
  tab <- data.frame(Model = label, Term = rownames(ct), Estimate = fmt(ct[, 1], 3), SE = fmt(ct[, 2], 3),
                    CI = ci_tx(ci[, 1], ci[, 2], 3), p = fmt_p(ct[, 4]))
  rbind(tab, data.frame(Model = label, Term = c("n", "R2"),
                        Estimate = c(as.character(nobs(fit)), fmt(summary(fit)$r.squared, 3)),
                        SE = "", CI = "", p = ""))
}

diag_plots <- function(fit, d, name) {
  dd <- data.frame(fitted = fitted(fit), resid = residuals(fit), std = rstandard(fit))
  p1 <- ggplot(dd, aes(x = fitted, y = resid)) + geom_point(alpha = 0.5) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "#e34948") +
    geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "#2a78d6") +
    labs(x = "fitted rating (rating points)", y = "residual (rating points)", title = "Residuals vs fitted") +
    theme_minimal()
  p2 <- ggplot(dd, aes(sample = std)) + stat_qq(alpha = 0.5) + stat_qq_line(color = "#e34948") +
    labs(x = "theoretical quantile", y = "standardized residual", title = "Normal Q-Q plot") + theme_minimal()
  save_fig(p1, paste0(name, "_resid"))
  save_fig(p2, paste0(name, "_qq"))
  top <- sort(cooks.distance(fit), decreasing = TRUE)[1:5]
  print(data.frame(cafe = d$cafe_name[as.integer(names(top))], cooks = round(top, 3)))
}

df <- read.csv(file.path("analysis", "dataset.csv"), sep = ",", header = TRUE)

df$cafe_type     <- factor(df$cafe_type, levels = c("Independent", "Local Chain", "International Chain"))
df$ratings_group <- factor(df$ratings_group, levels = c("few (<100)", "medium (100-1000)", "many (>1000)"),
                           labels = c("few (under 100)", "medium (100-1000)", "many (over 1000)"))
df$ratings_group2 <- factor(df$ratings_group2, levels = c("many (>=100)", "few (<100)"),
                            labels = c("many (100 or more)", "few (under 100)"))
df$nratings_cmp  <- factor(df$nratings_cmp, levels = c("above median", "below median"))
df$n1000         <- df$n_ratings / 1000

nrow(df)
c(rating_available = sum(!is.na(df$rating)), ratings_count_available = sum(!is.na(df$n_ratings)))

x1 <- df$rating[!is.na(df$rating)]
n1 <- length(x1)

a1_ci_t    <- t.test(x1, mu = 4.5)$conf.int
a1_ci_boot <- boot_ci(x1)
a1_d       <- (mean(x1) - 4.5) / sd(x1)

if (method["A1"] == "param") {
  a1_test <- t.test(x1, mu = 4.5)
  a1_stat <- paste0("t(", a1_test$parameter, ") = ", fmt(a1_test$statistic))
  a1_p    <- a1_test$p.value
  a1_name <- "One-sample t-test"
} else {
  a1_test <- wilcox.test(x1, mu = 4.5, exact = FALSE)
  a1_stat <- paste0("V = ", fmt(a1_test$statistic, 0))
  a1_p    <- a1_test$p.value
  a1_name <- "One-sample Wilcoxon test"
}

A1 <- data.frame(Quantity = c("n", "Mean rating (rating points)", "95 percent CI (t)", "95 percent CI (bootstrap)",
                              "Median", "IQR", "SD", "Cohen's d vs 4.5", a1_name, "p"),
                 Value = c(n1, fmt(mean(x1)), ci_tx(a1_ci_t[1], a1_ci_t[2]), ci_tx(a1_ci_boot[1], a1_ci_boot[2]),
                           fmt(median(x1)), fmt(IQR(x1)), fmt(sd(x1)), fmt(a1_d), a1_stat, fmt_p(a1_p)))
save_table(A1, "A1")

A1_fig <- ggplot(data.frame(rating = x1), aes(x = rating)) +
  geom_histogram(bins = 25, fill = "grey70", color = "white") +
  annotate("rect", xmin = a1_ci_t[1], xmax = a1_ci_t[2], ymin = -Inf, ymax = Inf, fill = "#2a78d6", alpha = 0.3) +
  geom_vline(xintercept = mean(x1), color = "#2a78d6", linewidth = 1) +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "#e34948") +
  labs(x = "Google rating (rating points)", y = "number of cafes",
       title = "Google ratings: mean with 95% CI (blue) vs 4.5 (dashed red)") +
  theme_minimal()
save_fig(A1_fig, "A1")

add_summary("A1", "Mean rating vs 4.5", paste0("M = ", fmt(mean(x1)), " ", ci_tx(a1_ci_t[1], a1_ci_t[2])),
            paste0(a1_name, ", ", a1_stat), a1_p)

d2 <- df %>% filter(!is.na(rating), !is.na(nratings_cmp))
table(d2$nratings_cmp)

a2_two <- t.test(rating ~ nratings_cmp, data = d2)
a2_d   <- (d2 %>% cohens_d(rating ~ nratings_cmp))$effsize

if (method["A2"] == "param") {
  a2_test <- t.test(rating ~ nratings_cmp, data = d2, alternative = "less")
  a2_stat <- paste0("t(", fmt(a2_test$parameter), ") = ", fmt(a2_test$statistic))
  a2_name <- "Welch t-test (one-tailed)"
  a2_est  <- unname(a2_two$estimate[1] - a2_two$estimate[2])
  a2_ci   <- a2_two$conf.int
} else {
  a2_test <- wilcox.test(rating ~ nratings_cmp, data = d2, alternative = "less", conf.int = TRUE, exact = FALSE)
  a2_stat <- paste0("W = ", fmt(a2_test$statistic, 0))
  a2_name <- "Wilcoxon rank-sum test (one-tailed)"
  a2_two_w <- wilcox.test(rating ~ nratings_cmp, data = d2, conf.int = TRUE, exact = FALSE)
  a2_est  <- unname(a2_two_w$estimate)
  a2_ci   <- a2_two_w$conf.int
}
a2_p <- a2_test$p.value

A2 <- data.frame(Quantity = c("n above median", "n below median", "Mean above median", "Mean below median",
                              "Difference: above minus below (rating points)", "95 percent CI (two-sided)",
                              "Cohen's d (above minus below)", a2_name, "p (one-tailed, fixed in advance)"),
                 Value = c(sum(d2$nratings_cmp == "above median"), sum(d2$nratings_cmp == "below median"),
                           fmt(a2_two$estimate[1]), fmt(a2_two$estimate[2]),
                           fmt(a2_est), ci_tx(a2_ci[1], a2_ci[2]), fmt(a2_d), a2_stat, fmt_p(a2_p)))
save_table(A2, "A2")

a2_g <- group_ci(d2, "rating", "nratings_cmp")
A2_fig <- ggplot(d2, aes(x = nratings_cmp, y = rating)) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  geom_errorbar(data = a2_g, aes(x = grp, ymin = lower, ymax = upper), width = 0.1, color = "#e34948", linewidth = 1, inherit.aes = FALSE) +
  geom_point(data = a2_g, aes(x = grp, y = m), color = "#e34948", size = 3, inherit.aes = FALSE) +
  labs(x = "number of Google ratings", y = "Google rating (rating points)",
       title = "Rating by number of ratings (red: mean with 95% CI)") + theme_minimal()
save_fig(A2_fig, "A2")

add_summary("A2", "Above- vs below-median number of ratings", paste0(fmt(a2_est), " ", ci_tx(a2_ci[1], a2_ci[2]), " rating points"),
            paste0(a2_name, ", ", a2_stat), a2_p)

d3 <- df %>% filter(!is.na(rating), !is.na(adj_rating))
diff3 <- d3$adj_rating - d3$rating
n3 <- length(diff3)

a3_d <- mean(diff3) / sd(diff3)

if (method["A3"] == "param") {
  a3_test <- t.test(d3$adj_rating, d3$rating, paired = TRUE)
  a3_stat <- paste0("t(", a3_test$parameter, ") = ", fmt(a3_test$statistic))
  a3_name <- "Paired t-test"
  a3_est  <- unname(a3_test$estimate)
  a3_ci   <- a3_test$conf.int
} else {
  a3_test <- wilcox.test(d3$adj_rating, d3$rating, paired = TRUE, conf.int = TRUE, exact = FALSE)
  a3_stat <- paste0("V = ", fmt(a3_test$statistic, 0))
  a3_name <- "Wilcoxon signed-rank test"
  a3_est  <- unname(a3_test$estimate)
  a3_ci   <- a3_test$conf.int
}
a3_p <- a3_test$p.value

A3 <- data.frame(Quantity = c("n pairs", "Mean raw rating", "Mean adjusted rating",
                              "Mean difference: adjusted minus raw (rating points)", "95 percent CI", "d z",
                              "Median difference", a3_name, "p"),
                 Value = c(n3, fmt(mean(d3$rating), 3), fmt(mean(d3$adj_rating), 3), fmt(a3_est, 3),
                           ci_tx(a3_ci[1], a3_ci[2], 3), fmt(a3_d), fmt(median(diff3), 3), a3_stat, fmt_p(a3_p)))
save_table(A3, "A3")

A3_fig <- ggplot(data.frame(diff = diff3), aes(x = diff)) +
  geom_histogram(bins = 30, fill = "grey70", color = "white") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "#e34948") +
  annotate("rect", xmin = a3_ci[1], xmax = a3_ci[2], ymin = -Inf, ymax = Inf, fill = "#2a78d6", alpha = 0.3) +
  geom_vline(xintercept = a3_est, color = "#2a78d6", linewidth = 1) +
  labs(x = "adjusted minus raw rating (rating points)", y = "number of cafes",
       title = "Adjustment of the rating: estimate with 95% CI (blue), 0 (dashed red)") + theme_minimal()
save_fig(A3_fig, "A3")

add_summary("A3", "Adjusted minus raw rating", paste0(fmt(a3_est, 3), " ", ci_tx(a3_ci[1], a3_ci[2], 3), " rating points"),
            paste0(a3_name, ", ", a3_stat), a3_p)

d4 <- df %>% filter(!is.na(rating), !is.na(ratings_group)) %>% droplevels()
table(d4$ratings_group)

levene <- d4 %>% levene_test(rating ~ ratings_group)
levene

if (method["A4"] == "param") {
  a4_test <- oneway.test(rating ~ ratings_group, data = d4)
  a4_stat <- paste0("Welch F(", fmt(a4_test$parameter[1]), ", ", fmt(a4_test$parameter[2]), ") = ", fmt(a4_test$statistic))
  a4_name <- "Welch ANOVA"
} else {
  a4_test <- kruskal.test(rating ~ ratings_group, data = d4)
  a4_stat <- paste0("H(", a4_test$parameter, ") = ", fmt(a4_test$statistic))
  a4_name <- "Kruskal-Wallis test"
}
a4_p <- a4_test$p.value

if (method["A4"] == "param") {
  a4_g <- group_ci(d4, "rating", "ratings_group") %>% rename(center = m)
} else {
  a4_g <- bind_rows(lapply(levels(d4$ratings_group), function(g) {
    x <- d4$rating[d4$ratings_group == g]
    ci <- boot_ci(x, stat = median)
    data.frame(grp = g, n = length(x), center = median(x), lower = unname(ci[1]), upper = unname(ci[2]))
  }))
}
a4_g$grp <- factor(a4_g$grp, levels = levels(d4$ratings_group))

A4 <- data.frame(Group = as.character(a4_g$grp), n = a4_g$n,
                 Center = fmt(a4_g$center), CI = ci_tx(a4_g$lower, a4_g$upper))
A4 <- rbind(A4, data.frame(Group = c(a4_name, "p"), n = "", Center = c(a4_stat, fmt_p(a4_p)), CI = ""))
save_table(A4, "A4")

A4_fig <- ggplot(d4, aes(x = ratings_group, y = rating)) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  geom_errorbar(data = a4_g, aes(x = grp, ymin = lower, ymax = upper), width = 0.1, color = "#e34948", linewidth = 1, inherit.aes = FALSE) +
  geom_point(data = a4_g, aes(x = grp, y = center), color = "#e34948", size = 3, inherit.aes = FALSE) +
  labs(x = "number of Google ratings", y = "Google rating (rating points)",
       title = ifelse(method["A4"] == "param", "Rating by group (red: mean, 95% CI)", "Rating by group (red: median, bootstrap 95% CI)")) +
  theme_minimal()
save_fig(A4_fig, "A4")

if (a4_p < alpha) {
  if (method["A4"] == "param") {
    ph <- d4 %>% games_howell_test(rating ~ ratings_group)
    for (k in seq_len(nrow(ph))) {
      mine <- mean(d4$rating[d4$ratings_group == ph$group2[k]]) - mean(d4$rating[d4$ratings_group == ph$group1[k]])
      if (sign(mine) != sign(ph$estimate[k])) {
        ph$estimate[k] <- -ph$estimate[k]
        tmp <- ph$conf.low[k]; ph$conf.low[k] <- -ph$conf.high[k]; ph$conf.high[k] <- -tmp
      }
    }
    A4_post <- data.frame(Comparison = paste(ph$group2, "minus", ph$group1), Difference = fmt(ph$estimate),
                          CI = ci_tx(ph$conf.low, ph$conf.high), p.adj = fmt_p(ph$p.adj))
  } else {
    ph <- d4 %>% dunn_test(rating ~ ratings_group, p.adjust.method = "holm")
    diffs <- sapply(seq_len(nrow(ph)), function(k) {
      x <- d4$rating[d4$ratings_group == ph$group2[k]]
      y <- d4$rating[d4$ratings_group == ph$group1[k]]
      c(est = median(x) - median(y), boot_diff_ci(x, y, stat = median))
    })
    A4_post <- data.frame(Comparison = paste(ph$group2, "minus", ph$group1),
                          Difference = fmt(diffs[1, ]), CI = ci_tx(diffs[2, ], diffs[3, ]), p.adj = fmt_p(ph$p.adj))
  }
  A4_post
  save_table(A4_post, "A4_posthoc")
}

add_summary("A4", "Rating by number-of-ratings group", paste0("see table A4 (", nlevels(d4$ratings_group), " groups)"),
            paste0(a4_name, ", ", a4_stat), a4_p)

d5 <- df %>% filter(!is.na(top_rated), !is.na(nratings_below_median))
d5$grp <- factor(ifelse(d5$nratings_below_median == 1, "below median", "other cafes"), levels = c("below median", "other cafes"))
d5$top <- factor(ifelse(d5$top_rated == 1, "top-rated", "not top-rated"), levels = c("top-rated", "not top-rated"))

tab5 <- table(d5$grp, d5$top)
tab5
round(prop.table(tab5, margin = 1) * 100, 1)

chi5   <- chisq.test(tab5, correct = FALSE)
min_e5 <- min(chi5$expected)
cv5    <- sqrt(unname(chi5$statistic) / sum(tab5))
pt5_one <- prop.test(tab5[, "top-rated"], rowSums(tab5), alternative = "greater", correct = FALSE)
pt5_two <- prop.test(tab5[, "top-rated"], rowSums(tab5), correct = FALSE)
diff5   <- unname(pt5_two$estimate[1] - pt5_two$estimate[2]) * 100
ci5     <- pt5_two$conf.int * 100

if (min_e5 >= 5) {
  a5_name <- "Chi-square test (difference in proportions, one-tailed)"
  a5_stat <- paste0("$\\chi^2$(1, N = ", sum(tab5), ") = ", fmt(chi5$statistic))
  a5_p    <- pt5_one$p.value
} else {
  f5 <- fisher.test(tab5, alternative = "greater")
  a5_name <- "Fisher's exact test (one-tailed)"
  a5_stat <- paste0("OR = ", fmt(f5$estimate))
  a5_p    <- f5$p.value
}

A5 <- data.frame(Quantity = c("Top-rated, below median (n top / n)", "Top-rated, other cafes (n top / n)",
                              "Share top-rated, below median (pp)", "Share top-rated, other cafes (pp)",
                              "Difference: below minus other (percentage points)", "95 percent CI (two-sided)",
                              "Smallest expected count", "Cramer's V", a5_name, a5_stat, "p (one-tailed, fixed in advance)"),
                 Value = c(paste0(tab5[1, 1], " / ", sum(tab5[1, ])), paste0(tab5[2, 1], " / ", sum(tab5[2, ])),
                           fmt(pt5_two$estimate[1] * 100, 1), fmt(pt5_two$estimate[2] * 100, 1),
                           fmt(diff5, 1), ci_tx(ci5[1], ci5[2], 1), fmt(min_e5), fmt(cv5), "", "", fmt_p(a5_p)))
save_table(A5, "A5")
round(chi5$stdres, 2)

a5_g <- data.frame(grp = levels(d5$grp), p = as.numeric(prop.table(tab5, margin = 1)[, "top-rated"]),
                   lower = sapply(1:2, function(k) prop.test(tab5[k, 1], sum(tab5[k, ]))$conf.int[1]),
                   upper = sapply(1:2, function(k) prop.test(tab5[k, 1], sum(tab5[k, ]))$conf.int[2]))
A5_fig <- ggplot(a5_g, aes(x = grp, y = p * 100)) +
  geom_col(fill = "grey75", width = 0.5) +
  geom_errorbar(aes(ymin = lower * 100, ymax = upper * 100), width = 0.1, color = "#e34948", linewidth = 1) +
  geom_point(color = "#e34948", size = 3) +
  labs(x = "number of Google ratings", y = "share of top-rated cafes (percentage points)",
       title = "Top-rated (4.7 or higher) cafes, with 95% CI") + theme_minimal()
save_fig(A5_fig, "A5")

add_summary("A5", "Top-rated: below-median vs other cafes", paste0(fmt(diff5, 1), " pp ", ci_tx(ci5[1], ci5[2], 1)),
            paste0(a5_name, ", ", a5_stat), a5_p)

d6 <- df %>% filter(!is.na(rating), !is.na(n_ratings))
n6 <- nrow(d6)

if (method["A6"] == "param") {
  a6_test <- cor.test(d6$n_ratings, d6$rating)
  a6_r    <- unname(a6_test$estimate)
  a6_ci   <- a6_test$conf.int
  a6_name <- "Pearson's r"
  a6_stat <- paste0("t(", a6_test$parameter, ") = ", fmt(a6_test$statistic))
} else {
  a6_test <- cor.test(d6$n_ratings, d6$rating, method = "spearman", exact = FALSE)
  a6_r    <- unname(a6_test$estimate)
  set.seed(seed)
  a6_b <- replicate(n_boot, { i <- sample(n6, replace = TRUE); cor(d6$n_ratings[i], d6$rating[i], method = "spearman") })
  a6_ci   <- quantile(a6_b, c(0.025, 0.975))
  a6_name <- "Spearman's rho (bootstrap CI)"
  a6_stat <- paste0("S = ", fmt(a6_test$statistic, 0))
}
a6_p <- a6_test$p.value

A6 <- data.frame(Quantity = c("n", a6_name, "95 percent CI", "Squared coefficient (shared variance)", "Test statistic", "p"),
                 Value = c(n6, fmt(a6_r), ci_tx(a6_ci[1], a6_ci[2]), fmt(a6_r^2), a6_stat, fmt_p(a6_p)))
save_table(A6, "A6")

A6_fig <- ggplot(d6, aes(x = n_ratings, y = rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "#e34948") +
  scale_x_log10() +
  labs(x = "number of Google ratings (log scale)", y = "Google rating (rating points)",
       title = "Rating vs number of ratings (smoother for orientation only)") + theme_minimal()
save_fig(A6_fig, "A6")

add_summary("A6", "Rating vs number of ratings", paste0(ifelse(method["A6"] == "param", "r = ", "rho = "), fmt(a6_r), " ", ci_tx(a6_ci[1], a6_ci[2])),
            paste0(a6_name, ", ", a6_stat), a6_p)

d7 <- df %>% filter(!is.na(rating), !is.na(n1000))
fit7 <- lm(rating ~ n1000, data = d7)
vc7  <- choose_vcov(fit7)
c(breusch_pagan_p = vc7$bp_p, robust_se_used = vc7$robust)

A7 <- reg_table(fit7, vc7)
A7 <- A7[, c("Term", "Estimate", "SE", "CI", "p")]
save_table(A7, "A7")
diag_plots(fit7, d7, "A7")

A7_fig <- ggplot(d7, aes(x = n1000, y = rating)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", formula = y ~ x, color = "#e34948") +
  labs(x = "number of Google ratings (thousands)", y = "Google rating (rating points)",
       title = "Rating vs number of ratings: least-squares line with 95% CI") + theme_minimal()
save_fig(A7_fig, "A7")

ct7 <- coeftest(fit7, vcov. = vc7$v); ci7 <- coefci(fit7, vcov. = vc7$v)
add_summary("A7", "Rating per 1,000 ratings", paste0(fmt(ct7["n1000", 1], 3), " ", ci_tx(ci7["n1000", 1], ci7["n1000", 2], 3), " points"),
            paste0("t(", fit7$df.residual, ") = ", fmt(ct7["n1000", 3])), ct7["n1000", 4])

d8 <- df %>% filter(!is.na(rating), !is.na(cafe_type), !is.na(n1000))
table(d8$cafe_type)

fit8a <- lm(rating ~ cafe_type, data = d8)
fit8b <- lm(rating ~ cafe_type + n1000, data = d8)
fit8n <- lm(rating ~ n1000, data = d8)
vc8a <- choose_vcov(fit8a)
vc8b <- choose_vcov(fit8b)

A8 <- rbind(reg_table(fit8a, vc8a, "Cafe type only"), reg_table(fit8b, vc8b, "Cafe type + ratings"))
save_table(A8, "A8")
diag_plots(fit8b, d8, "A8")

joint8 <- waldtest(fit8b, fit8n, vcov = function(x) {
  if (vc8b$robust) vcovHC(x, type = "HC3") else vcov(x)
})
joint8
joint_p <- joint8$"Pr(>F)"[2]
joint_F <- joint8$F[2]

a8_ct <- coeftest(fit8b, vcov. = vc8b$v)
a8_ci <- coefci(fit8b, vcov. = vc8b$v)
cmp8 <- rbind(without_n1000 = coef(fit8a)[-1], with_n1000 = coef(fit8b)[2:3])
cmp8

a8_ad <- data.frame(cafe_type = levels(d8$cafe_type), n1000 = mean(d8$n1000))
a8_pr <- predict(fit8b, a8_ad, interval = "confidence")
a8_pr <- data.frame(cafe_type = factor(levels(d8$cafe_type), levels = levels(d8$cafe_type)), a8_pr)
A8_fig <- ggplot(d8, aes(x = cafe_type, y = rating)) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  geom_errorbar(data = a8_pr, aes(x = cafe_type, ymin = lwr, ymax = upr), width = 0.1, color = "#e34948", linewidth = 1, inherit.aes = FALSE) +
  geom_point(data = a8_pr, aes(x = cafe_type, y = fit), color = "#e34948", size = 3, inherit.aes = FALSE) +
  labs(x = "cafe type", y = "Google rating (rating points)",
       title = "Rating by cafe type (red: adjusted mean at the average number of ratings, 95% CI)") +
  theme_minimal()
save_fig(A8_fig, "A8", w = 7)

add_summary("A8", "Rating by cafe type, adjusted for number of ratings",
            paste0("Local chain: ", fmt(a8_ct[2, 1], 3), " ", ci_tx(a8_ci[2, 1], a8_ci[2, 2], 3)),
            paste0("Joint F-test of cafe type, F = ", fmt(joint_F)), joint_p)

d9 <- df %>% filter(!is.na(rating), !is.na(adj_rating), !is.na(ratings_group2))
d9$few <- as.integer(d9$ratings_group2 == "few (under 100)")
table(d9$ratings_group2)

fit9 <- lm(adj_rating ~ rating * few, data = d9)
vc9  <- choose_vcov(fit9)
c(breusch_pagan_p = vc9$bp_p, robust_se_used = vc9$robust)

A9 <- reg_table(fit9, vc9)
A9 <- A9[, c("Term", "Estimate", "SE", "CI", "p")]

b9 <- coef(fit9); V9 <- vc9$v
tcrit <- qt(0.975, fit9$df.residual)
slope_many <- b9["rating"]
se_many    <- sqrt(V9["rating", "rating"])
slope_few  <- b9["rating"] + b9["rating:few"]
se_few     <- sqrt(V9["rating", "rating"] + V9["rating:few", "rating:few"] + 2 * V9["rating", "rating:few"])
A9 <- rbind(A9, data.frame(Term = c("Slope, many ratings", "Slope, few ratings"),
                           Estimate = fmt(c(slope_many, slope_few), 3), SE = fmt(c(se_many, se_few), 3),
                           CI = c(ci_tx(slope_many - tcrit * se_many, slope_many + tcrit * se_many, 3),
                                  ci_tx(slope_few - tcrit * se_few, slope_few + tcrit * se_few, 3)), p = ""))
save_table(A9, "A9")
diag_plots(fit9, d9, "A9")

A9_fig <- ggplot(d9, aes(x = rating, y = adj_rating, color = ratings_group2)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ x, se = TRUE) +
  labs(x = "raw Google rating (rating points)", y = "Adjusted Rating (rating points)", color = NULL,
       title = "Raw vs adjusted rating, by number of ratings") + theme_minimal() +
  theme(legend.position = "bottom")
save_fig(A9_fig, "A9")

ct9 <- coeftest(fit9, vcov. = V9); ci9 <- coefci(fit9, vcov. = V9)
add_summary("A9", "Raw vs adjusted rating: few vs many ratings",
            paste0("Interaction: ", fmt(ct9["rating:few", 1], 3), " ", ci_tx(ci9["rating:few", 1], ci9["rating:few", 2], 3)),
            paste0("t(", fit9$df.residual, ") = ", fmt(ct9["rating:few", 3])), ct9["rating:few", 4])

d10 <- df %>% filter(!is.na(top_rated), !is.na(n1000))
fit10 <- glm(top_rated ~ n1000, data = d10, family = binomial)
summary(fit10)

ame <- function(fit) mean(coef(fit)["n1000"] * fitted(fit) * (1 - fitted(fit))) * 100
ame10 <- ame(fit10)

set.seed(seed)
ame_boot <- replicate(n_boot, {
  i <- sample(nrow(d10), replace = TRUE)
  ame(suppressWarnings(glm(top_rated ~ n1000, data = d10[i, ], family = binomial)))
})
ame_ci <- quantile(ame_boot, c(0.025, 0.975))

ct10 <- summary(fit10)$coefficients
or_ci <- exp(confint.default(fit10)["n1000", ])
A10 <- data.frame(Quantity = c("n", "Top-rated cafes", "Coefficient (log-odds per 1,000 ratings)", "SE",
                               "Odds ratio per 1,000 ratings", "95 percent CI of the odds ratio (Wald)",
                               "Average marginal effect (percentage points per 1,000 ratings)", "95 percent CI of the AME (bootstrap)",
                               "z", "p"),
                  Value = c(nrow(d10), sum(d10$top_rated), fmt(ct10["n1000", 1], 3), fmt(ct10["n1000", 2], 3),
                            fmt(exp(ct10["n1000", 1])), ci_tx(or_ci[1], or_ci[2]), fmt(ame10, 1),
                            ci_tx(ame_ci[1], ame_ci[2], 1), fmt(ct10["n1000", 3]), fmt_p(ct10["n1000", 4])))
save_table(A10, "A10")

grid10 <- data.frame(n1000 = seq(min(d10$n1000), max(d10$n1000), length.out = 200))
pr10 <- predict(fit10, grid10, type = "link", se.fit = TRUE)
grid10$p     <- plogis(pr10$fit)
grid10$lower <- plogis(pr10$fit - 1.96 * pr10$se.fit)
grid10$upper <- plogis(pr10$fit + 1.96 * pr10$se.fit)
A10_fig <- ggplot(grid10, aes(x = n1000)) +
  geom_ribbon(aes(ymin = lower, ymax = upper), fill = "#e34948", alpha = 0.2) +
  geom_line(aes(y = p), color = "#e34948", linewidth = 1) +
  geom_jitter(data = d10, aes(x = n1000, y = top_rated), height = 0.03, alpha = 0.4, inherit.aes = FALSE) +
  labs(x = "number of Google ratings (thousands)", y = "probability of a rating of 4.7 or higher",
       title = "Probability of being top-rated (95% CI)") + theme_minimal()
save_fig(A10_fig, "A10")

add_summary("A10", "Top-rated per 1,000 ratings", paste0("AME = ", fmt(ame10, 1), " pp ", ci_tx(ame_ci[1], ame_ci[2], 1)),
            paste0("Logistic regression, z = ", fmt(ct10["n1000", 3])), ct10["n1000", 4])

summary_table <- do.call(rbind, summary_rows)
summary_table
save_table(summary_table, "A_summary")

cat("Analysis finished: tables in", dir_tab, ", figures in", dir_fig, "\n")
