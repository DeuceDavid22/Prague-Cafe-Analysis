library(tidyverse)

file_raw <- file.path("analysis", "raw_data.csv")
file_out <- file.path("analysis", "dataset.csv")

Cafedata <- read.csv(file_raw, colClasses = "character", na.strings = "", fileEncoding = "UTF-8")

dim(Cafedata)
names(Cafedata)
View(Cafedata)
str(Cafedata)
glimpse(Cafedata)

Cafedata_raw <- Cafedata

names(Cafedata) <- c("student", "visit_raw", "district_raw", "district_name_raw", "cafe_name",
                     "address_raw", "coordinates_raw", "maps_link", "espresso_raw",
                     "cappuccino_raw", "flat_white_raw", "filter_raw", "payment_raw",
                     "seats_in_raw", "seats_out_raw", "rating_raw", "n_ratings_raw",
                     "cafe_type_raw", "roastery_raw", "photo_link", "note", "automatic",
                     "distance_raw", "latitude_sheet", "longitude_sheet", "adj_rating_sheet")

Cafedata[1, c("student", "cafe_name", "espresso_raw", "distance_raw")]

Cafedata <- Cafedata %>%
  select(student, visit_raw, district_raw, district_name_raw, cafe_name, address_raw,
         coordinates_raw, maps_link, espresso_raw, cappuccino_raw, flat_white_raw, filter_raw,
         payment_raw, seats_in_raw, seats_out_raw, rating_raw, n_ratings_raw, cafe_type_raw,
         roastery_raw, photo_link, note, distance_raw)

colSums(is.na(Cafedata))
sum(duplicated(Cafedata))

n_raw <- nrow(Cafedata)

sum(is.na(Cafedata$student) & is.na(Cafedata$cafe_name))

Cafedata <- Cafedata %>% filter(!is.na(student) | !is.na(cafe_name))

c(rows_in_file = n_raw, real_rows = nrow(Cafedata))

for (col in names(Cafedata)) {
  Cafedata[[col]] <- gsub("\u00a0", " ", Cafedata[[col]])
  Cafedata[[col]] <- gsub("^ +| +$", "", Cafedata[[col]])
}

length(unique(Cafedata$student))

table(Cafedata$district_raw, useNA = "ifany")
table(Cafedata$cafe_type_raw, useNA = "ifany")
table(Cafedata$payment_raw, useNA = "ifany")
table(Cafedata$roastery_raw, useNA = "ifany")
sort(table(Cafedata$district_name_raw, useNA = "ifany"), decreasing = TRUE)

Cafedata$district <- ifelse(is.na(Cafedata$district_raw), NA,
                      paste("Prague", gsub("[^0-9]", "", Cafedata$district_raw)))

table(Cafedata$district, useNA = "ifany")

Cafedata <- Cafedata %>%
  mutate(district_name = case_when(
    district_name_raw %in% c("Old Town", "Old town", "Staré Město", "stare mesto")        ~ "Old Town",
    district_name_raw %in% c("New Town", "Nové Město", "Vodickova, New Town",
                             "Charles Square, New Town")                                  ~ "New Town",
    district_name_raw %in% c("Karlín", "Karlin")                                          ~ "Karlin",
    district_name_raw %in% c("Mala Strana", "Malá Strana")                                ~ "Mala Strana",
    district_name_raw %in% c("Smíchov", "Smichov")                                        ~ "Smichov",
    district_name_raw %in% c("Libeň")                                                     ~ "Liben",
    district_name_raw %in% c("Holesovice")                                                ~ "Holesovice",
    TRUE                                                                                  ~ district_name_raw
  ))
table(Cafedata$district_name, useNA = "ifany")

Cafedata <- Cafedata %>%
  mutate(cafe_type = case_when(
    cafe_type_raw == "Independent"         ~ "Independent",
    cafe_type_raw == "Local Chain"         ~ "Local Chain",
    cafe_type_raw == "International Chain" ~ "International Chain",
    TRUE                                   ~ NA),
    payment = case_when(
    payment_raw == "Cash & Card" ~ "Cash & Card",
    payment_raw == "Cash Only"   ~ "Cash only",
    payment_raw == "Card Only"   ~ "Card only",
    TRUE                         ~ NA))

Cafedata$cafe_type <- factor(Cafedata$cafe_type, levels = c("Independent", "Local Chain", "International Chain"))
levels(Cafedata$cafe_type)
table(Cafedata$cafe_type, useNA = "ifany")
table(Cafedata$payment, useNA = "ifany")

to_num <- function(x) suppressWarnings(as.numeric(x))

show_nonnumeric <- function(x) sort(unique(x[is.na(to_num(x)) & !is.na(x)]))

show_nonnumeric(Cafedata$espresso_raw)
show_nonnumeric(Cafedata$cappuccino_raw)
show_nonnumeric(Cafedata$flat_white_raw)
show_nonnumeric(Cafedata$filter_raw)

Cafedata %>% filter(to_num(cappuccino_raw) > 1000) %>% select(cafe_name, cappuccino_raw)

first_price <- function(x) {
  x <- gsub("^([0-9]{3})[0-9]{3,}$", "\\1", x)
  x <- gsub("^[^0-9]*([0-9]+).*$", "\\1", x)
  to_num(x)
}

Cafedata$espresso_czk   <- first_price(Cafedata$espresso_raw)
Cafedata$cappuccino_czk <- first_price(Cafedata$cappuccino_raw)
Cafedata$flat_white_czk <- first_price(Cafedata$flat_white_raw)
Cafedata$filter_czk     <- first_price(Cafedata$filter_raw)

Cafedata %>% filter(is.na(to_num(espresso_raw)) & !is.na(espresso_raw))     %>% select(cafe_name, espresso_raw, espresso_czk)
Cafedata %>% filter(is.na(to_num(cappuccino_raw)) & !is.na(cappuccino_raw)) %>% select(cafe_name, cappuccino_raw, cappuccino_czk)
Cafedata %>% filter(is.na(to_num(flat_white_raw)) & !is.na(flat_white_raw)) %>% select(cafe_name, flat_white_raw, flat_white_czk)
Cafedata %>% filter(to_num(cappuccino_raw) > 1000)                          %>% select(cafe_name, cappuccino_raw, cappuccino_czk)

Cafedata$offers_filter <- ifelse(is.na(Cafedata$filter_czk), 0L, 1L)
table(Cafedata$offers_filter)

summary(Cafedata[c("espresso_czk", "cappuccino_czk", "flat_white_czk", "filter_czk")])

show_nonnumeric(Cafedata$n_ratings_raw)
show_nonnumeric(Cafedata$rating_raw)
show_nonnumeric(Cafedata$seats_in_raw)
show_nonnumeric(Cafedata$seats_out_raw)
show_nonnumeric(Cafedata$roastery_raw)
show_nonnumeric(Cafedata$distance_raw)

Cafedata$n_ratings   <- to_num(gsub("[, ]", "", Cafedata$n_ratings_raw))
Cafedata$rating      <- to_num(gsub(",", ".", Cafedata$rating_raw))
Cafedata$seats_in    <- to_num(Cafedata$seats_in_raw)
Cafedata$seats_out   <- to_num(Cafedata$seats_out_raw)
Cafedata$is_roastery <- to_num(Cafedata$roastery_raw)
Cafedata$distance_km <- to_num(Cafedata$distance_raw)

c(n_ratings   = sum(is.na(Cafedata$n_ratings)   & !is.na(Cafedata$n_ratings_raw)),
  rating      = sum(is.na(Cafedata$rating)      & !is.na(Cafedata$rating_raw)),
  seats_in    = sum(is.na(Cafedata$seats_in)    & !is.na(Cafedata$seats_in_raw)),
  seats_out   = sum(is.na(Cafedata$seats_out)   & !is.na(Cafedata$seats_out_raw)),
  is_roastery = sum(is.na(Cafedata$is_roastery) & !is.na(Cafedata$roastery_raw)),
  distance_km = sum(is.na(Cafedata$distance_km) & !is.na(Cafedata$distance_raw)))

sum(Cafedata$n_ratings %% 1 != 0, na.rm = TRUE)

Cafedata %>% filter(!is.na(n_ratings_raw) & is.na(to_num(n_ratings_raw))) %>% select(cafe_name, n_ratings_raw, n_ratings)

Cafedata %>% filter(is.na(rating) | is.na(n_ratings)) %>% select(cafe_name, rating, n_ratings)

Cafedata$latitude  <- to_num(gsub(",.*$", "", Cafedata$coordinates_raw))
Cafedata$longitude <- to_num(gsub("^.*, *", "", Cafedata$coordinates_raw))

summary(Cafedata[c("latitude", "longitude")])
Cafedata %>% filter(latitude < 49.9 | latitude > 50.2 | longitude < 14.2 | longitude > 14.8) %>%
  select(cafe_name, latitude, longitude)

Cafedata$ns_km <- (Cafedata$latitude  - 50.0871) * 111
Cafedata$ew_km <- (Cafedata$longitude - 14.4207) * 71

candidates <- NULL
for (i in seq_len(nrow(Cafedata) - 1)) {
  for (j in (i + 1):nrow(Cafedata)) {
    dist_m <- sqrt(((Cafedata$latitude[i]  - Cafedata$latitude[j])  * 111000)^2 +
                   ((Cafedata$longitude[i] - Cafedata$longitude[j]) * 71000)^2)
    a <- Cafedata$n_ratings[i]
    b <- Cafedata$n_ratings[j]
    if (dist_m < 40 & (is.na(a) | is.na(b) | abs(a - b) / max(a, b) < 0.10)) {
      candidates <- rbind(candidates,
                          data.frame(row_i = i, row_j = j, distance_m = round(dist_m),
                                     cafe_i = Cafedata$cafe_name[i], cafe_j = Cafedata$cafe_name[j],
                                     ratings_i = a, ratings_j = b))
    }
  }
}
candidates

not_duplicates <- c()

rows_to_drop <- unique(candidates$row_j)
rows_to_drop <- rows_to_drop[!(rows_to_drop %in% not_duplicates)]

n_before <- nrow(Cafedata)
if (length(rows_to_drop) > 0) {
  Cafedata <- Cafedata[-rows_to_drop, ]
}
c(before = n_before, after = nrow(Cafedata), removed = length(rows_to_drop))

mean_rating <- mean(Cafedata$rating, na.rm = TRUE)
median_nrat <- median(Cafedata$n_ratings, na.rm = TRUE)
median_fw   <- median(Cafedata$flat_white_czk, na.rm = TRUE)

Cafedata$adj_rating <- (Cafedata$n_ratings * Cafedata$rating + 50 * mean_rating) / (Cafedata$n_ratings + 50)

Cafedata$price_level <- ifelse(!is.na(Cafedata$espresso_czk) & !is.na(Cafedata$cappuccino_czk) & !is.na(Cafedata$flat_white_czk),
                         (Cafedata$espresso_czk + Cafedata$cappuccino_czk + Cafedata$flat_white_czk) / 3, NA)
median_pl <- median(Cafedata$price_level, na.rm = TRUE)

Cafedata$prague1         <- ifelse(Cafedata$district == "Prague 1", 1L, 0L)
Cafedata$outdoor_seating <- ifelse(Cafedata$seats_out > 0, 1L, 0L)
Cafedata$top_rated       <- ifelse(Cafedata$rating >= 4.7, 1L, 0L)
Cafedata$small_cafe      <- ifelse(Cafedata$seats_in <= 15, 1L, 0L)
Cafedata$chain           <- ifelse(Cafedata$cafe_type != "Independent", 1L, 0L)
Cafedata$intl_chain      <- ifelse(Cafedata$cafe_type == "International Chain", 1L, 0L)
Cafedata$east_of_orloj   <- ifelse(Cafedata$ew_km > 0, 1L, 0L)
Cafedata$nratings_above_median    <- ifelse(Cafedata$n_ratings > median_nrat, 1L, 0L)
Cafedata$nratings_below_median    <- ifelse(Cafedata$n_ratings < median_nrat, 1L, 0L)
Cafedata$fw_above_median          <- ifelse(Cafedata$flat_white_czk > median_fw, 1L, 0L)
Cafedata$price_level_above_median <- ifelse(Cafedata$price_level > median_pl, 1L, 0L)

Cafedata$abs_ns_km <- abs(Cafedata$ns_km)
Cafedata$abs_ew_km <- abs(Cafedata$ew_km)

Cafedata$ratings_group <- cut(Cafedata$n_ratings, breaks = c(-Inf, 99, 1000, Inf),
                        labels = c("few (<100)", "medium (100-1000)", "many (>1000)"), right = TRUE)
Cafedata$ratings_group2 <- factor(ifelse(Cafedata$n_ratings < 100, "few (<100)", "many (>=100)"))

Cafedata <- Cafedata %>%
  mutate(nratings_cmp = case_when(n_ratings > median_nrat ~ "above median",
                                  n_ratings < median_nrat ~ "below median",
                                  TRUE                    ~ NA))

Cafedata$n_specialty     <- Cafedata$is_roastery + Cafedata$offers_filter
Cafedata$specialty_group <- factor(Cafedata$n_specialty, levels = 0:2, labels = c("none", "one", "both"))

c(mean_rating = mean_rating, median_nratings = median_nrat,
  median_flat_white = median_fw, median_price_level = median_pl)

Cafedata_clean <- Cafedata %>%
  select(student, cafe_name, district, district_name,
         latitude, longitude, distance_km, ns_km, ew_km, abs_ns_km, abs_ew_km, east_of_orloj,
         espresso_czk, cappuccino_czk, flat_white_czk, filter_czk, offers_filter, price_level,
         payment, seats_in, seats_out, outdoor_seating, small_cafe,
         rating, n_ratings, adj_rating, top_rated, nratings_above_median, nratings_below_median, nratings_cmp,
         ratings_group, ratings_group2,
         cafe_type, chain, intl_chain, is_roastery, n_specialty, specialty_group,
         prague1, fw_above_median, price_level_above_median, note)

c(rows_in_file = n_raw, final_rows = nrow(Cafedata_clean))
colSums(is.na(Cafedata_clean))
summary(Cafedata_clean)
str(Cafedata_clean)

range(Cafedata_clean$rating, na.rm = TRUE)
table(Cafedata_clean$nratings_cmp, useNA = "ifany")
table(Cafedata_clean$ratings_group, useNA = "ifany")
table(Cafedata_clean$top_rated, useNA = "ifany")

Cafedata[order(Cafedata$espresso_czk), c("cafe_name", "espresso_raw", "espresso_czk", "photo_link")][1:5, ]
Cafedata[order(-Cafedata$espresso_czk), c("cafe_name", "espresso_raw", "espresso_czk", "photo_link")][1:5, ]
Cafedata[order(-Cafedata$filter_czk), c("cafe_name", "filter_raw", "filter_czk", "photo_link")][1:5, ]

write.csv(Cafedata_clean, file_out, row.names = FALSE)
cat("Saved:", file_out, "\n")
