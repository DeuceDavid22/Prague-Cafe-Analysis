library(tidyverse)
library(xtable)

alpha  <- 0.05
n_boot <- 5000
seed   <- 123

dir_tab <- file.path("report", "tables")
dir_fig <- file.path("report", "figures")
for (d in c(dir_tab, dir_fig)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

fmt <- function(x, d = 2) formatC(x, format = "f", digits = d)

save_table <- function(tab, name) {
  print(xtable(tab), file = file.path(dir_tab, paste0(name, ".tex")),
        include.rownames = FALSE, booktabs = TRUE, size = "footnotesize")
}
save_fig <- function(p, name, w = 8, h = 6) {
  ggsave(file.path(dir_fig, paste0(name, ".pdf")), p, width = w, height = h)
}

central_moment <- function(x, k) mean((x - mean(x))^k)

safe_shapiro <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) < 3 || length(unique(x)) < 3 || length(x) > 5000) return(NA_real_)
  shapiro.test(x)$p.value
}

bootstrap_means <- function(x) {
  x <- x[!is.na(x)]
  set.seed(seed)
  replicate(n_boot, mean(sample(x, length(x), replace = TRUE)))
}

df <- read.csv(file.path("analysis", "dataset.csv"), sep = ",", header = TRUE)

df$cafe_type     <- factor(df$cafe_type, levels = c("Independent", "Local Chain", "International Chain"))
df$ratings_group <- factor(df$ratings_group, levels = c("few (<100)", "medium (100-1000)", "many (>1000)"),
                           labels = c("few (under 100)", "medium (100-1000)", "many (over 1000)"))
df$nratings_cmp  <- factor(df$nratings_cmp, levels = c("above median", "below median"))

df$rating_diff <- df$adj_rating - df$rating

glimpse(df)
nrow(df)
colSums(is.na(df[c("rating", "n_ratings", "adj_rating", "top_rated", "cafe_type")]))

mk <- function(d, a, v, g, col) {
  data.frame(analysis = a, variable = v, group = g, value = d[[col]])
}

desc_long <- bind_rows(
  mk(df, "A1", "Google rating", "all cafes", "rating"),
  mk(df %>% filter(nratings_cmp == "above median"), "A2", "Google rating", "above median ratings", "rating"),
  mk(df %>% filter(nratings_cmp == "below median"), "A2", "Google rating", "below median ratings", "rating"),
  mk(df, "A3", "Adjusted minus raw rating", "all cafes", "rating_diff"),
  bind_rows(lapply(levels(df$ratings_group), function(g) {
    mk(df %>% filter(ratings_group == g), "A4", "Google rating", g, "rating")
  })),
  mk(df, "A6", "Google rating", "all cafes", "rating"),
  mk(df, "A6", "Number of ratings", "all cafes", "n_ratings")
)

desc_long$label <- paste0(desc_long$analysis, ": ", desc_long$variable, " - ", desc_long$group)
desc_long$label <- factor(desc_long$label, levels = unique(desc_long$label))
desc_long$panel <- paste(desc_long$analysis, desc_long$variable)

stats_rows <- list()
dec_rows   <- list()
boot_list  <- list()

for (lab in levels(desc_long$label)) {
  d_g <- desc_long %>% filter(label == lab)
  x   <- d_g$value[!is.na(d_g$value)]
  n   <- length(x)

  sk  <- central_moment(x, 3) / sd(x)^3
  ek  <- central_moment(x, 4) / sd(x)^4 - 3
  sh  <- safe_shapiro(x)
  normal <- abs(sk) < 0.5 & abs(ek) < 1

  bm  <- bootstrap_means(x)
  bsk <- central_moment(bm, 3) / sd(bm)^3
  bek <- central_moment(bm, 4) / sd(bm)^4 - 3
  bsh <- safe_shapiro(bm)
  clt_boot <- abs(bsk) < 0.5 & abs(bek) < 1
  clt      <- n >= 30 & clt_boot
  boot_list[[lab]] <- data.frame(label = lab, panel = d_g$panel[1], value = bm)

  param_ok <- normal | clt

  stats_rows[[lab]] <- data.frame(
    Analysis = d_g$analysis[1], Variable = d_g$variable[1], Group = d_g$group[1],
    n = n, Missing = sum(is.na(d_g$value)),
    Mean = fmt(mean(x)), SD = fmt(sd(x)), Median = fmt(median(x)), IQR = fmt(IQR(x)),
    Min = fmt(min(x)), Max = fmt(max(x)), check.names = FALSE)

  dec_rows[[lab]] <- data.frame(
    Analysis = d_g$analysis[1], Variable = d_g$variable[1], Group = d_g$group[1],
    n = n, Skewness = fmt(sk), `Excess kurtosis` = fmt(ek),
    Normal = ifelse(normal, "yes", "no"),
    `n at least 30` = ifelse(n >= 30, "yes", "no"),
    `Bootstrap skewness` = fmt(bsk),
    `CLT applies` = ifelse(clt, "yes", "no"),
    Conclusion = ifelse(param_ok, "parametric", "non-parametric"),
    `Shapiro p` = ifelse(is.na(sh), "NA", ifelse(sh < 0.001, "$<$.001", gsub("^0", "", fmt(sh, 3)))),
    param_ok = param_ok, check.names = FALSE)
}

stats_table <- do.call(rbind, stats_rows)
dec_table   <- do.call(rbind, dec_rows)
stats_table
dec_table

dec_table[c("Analysis", "Group", "n", "CONCLUSION" = "Conclusion")]

analyses <- c("A1", "A2", "A3", "A4", "A6")
decisions <- data.frame(analysis = analyses,
                        method = sapply(analyses, function(a) {
                          ok <- dec_table$param_ok[dec_table$Analysis == a]
                          ifelse(all(ok), "param", "nonparam")
                        }))

decisions
write.csv(decisions, file.path("analysis", "decisions.csv"), row.names = FALSE)

save_table(dec_table[, names(dec_table) != "param_ok"], "D_table")
save_table(stats_table, "D_stats_table")

decisions_tab <- data.frame(Analysis = decisions$analysis,
                            Method = ifelse(decisions$method == "param", "parametric", "non-parametric"))
save_table(decisions_tab, "D_decisions_table")

cat_table <- function(x, name) {
  tb <- table(x, useNA = "ifany")
  data.frame(Variable = name, Category = ifelse(is.na(names(tb)), "missing", names(tb)),
             Count = as.integer(tb), Percent = fmt(as.numeric(prop.table(tb)) * 100, 1))
}

df$top_label <- ifelse(df$top_rated == 1, "top-rated (4.7 or higher)", "not top-rated")
categorical <- rbind(cat_table(df$top_label, "Top-rated (A5, A10)"),
                     cat_table(df$cafe_type, "Cafe type (A8)"),
                     cat_table(df$ratings_group, "Number of ratings, 3 groups (A4)"),
                     cat_table(df$nratings_cmp, "Number of ratings vs median (A2, A5)"))
categorical
save_table(categorical, "D_categorical_table")

tab5 <- table(ifelse(df$nratings_below_median == 1, "below median", "other cafes"), df$top_rated)
tab5
chisq.test(tab5, correct = FALSE)$expected

plot_df <- desc_long %>% filter(!is.na(value))

p_hist <- ggplot(plot_df, aes(x = value)) +
  geom_histogram(bins = 25, fill = "grey70", color = "white") +
  facet_wrap(~ label, scales = "free", labeller = label_wrap_gen(28)) +
  labs(x = "value (rating points, or number of ratings)", y = "number of cafes",
       title = "Histograms of the variables used in the analyses") + theme_minimal()
save_fig(p_hist, "D_histograms", w = 10, h = 8)

p_box <- ggplot(plot_df, aes(x = group, y = value)) +
  geom_boxplot(outlier.alpha = 0.4, fill = "grey90") +
  facet_wrap(~ panel, scales = "free") +
  labs(x = NULL, y = "value (rating points, or number of ratings)",
       title = "Boxplots of the variables used in the analyses") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 20, hjust = 1))
save_fig(p_box, "D_boxplots", w = 10, h = 8)

p_qq <- ggplot(plot_df, aes(sample = value)) +
  stat_qq(alpha = 0.5) + stat_qq_line(color = "#e34948") +
  facet_wrap(~ label, scales = "free", labeller = label_wrap_gen(28)) +
  labs(x = "theoretical quantile", y = "sample quantile", title = "Normal Q-Q plots") + theme_minimal()
save_fig(p_qq, "D_qqplots", w = 10, h = 8)

boot_df <- bind_rows(boot_list)
boot_df$label <- factor(boot_df$label, levels = levels(desc_long$label))
p_boot <- ggplot(boot_df, aes(x = value)) +
  geom_histogram(bins = 40, fill = "#2a78d6", alpha = 0.6, color = "white") +
  facet_wrap(~ label, scales = "free", labeller = label_wrap_gen(28)) +
  labs(x = "bootstrapped mean", y = "count",
       title = paste0("Distribution of ", n_boot, " bootstrapped means (seed ", seed, ")")) + theme_minimal()
save_fig(p_boot, "D_bootstrap_means", w = 10, h = 8)

d_a24 <- plot_df %>% filter(analysis %in% c("A2", "A4"))
p_groups <- ggplot(d_a24, aes(x = group, y = value)) +
  geom_boxplot(fill = "grey90", outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4) +
  facet_wrap(~ analysis, scales = "free_x") +
  labs(x = "group", y = "Google rating (rating points)",
       title = "Google rating in the compared groups (A2: median split, A4: three groups)") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
save_fig(p_groups, "D_groups", w = 10, h = 5)

cat("Descriptive analysis finished. Decisions saved to analysis/decisions.csv\n")
