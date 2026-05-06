
# NYC AIRBNB ANALYSIS - TABLE 1
# Summary Statistics by Borough


# LOAD PACKAGES 
library(tidyverse)
library(knitr)
library(kableExtra)

# LOAD DATA
setwd("/Users/oyindamolasanni/Downloads")
listings <- read_csv("listings.csv")
detailed.ny.listings <- read_csv("ListingReviewsInfo.csv")

# DATA WRANGLING
listings_cleaned <- listings |>
  select(-c(name, host_profile_id, host_name, latitude, longitude,
            price, last_review, calculated_host_listings_count, license)) |>
  drop_na(reviews_per_month, minimum_nights) |>
  filter(minimum_nights != 0,
         availability_365 != 0) |>
  filter(room_type != "Shared room",
         room_type != "Hotel room") |>
  filter(minimum_nights < 366)

listing_reviews <- detailed.ny.listings |>
  select(id, review_scores_rating) |>
  mutate(review_scores_rating = as.numeric(trimws(review_scores_rating))) |>
  drop_na(review_scores_rating)

combined_listings <- listings_cleaned |>
  mutate(id = as.numeric(id)) |>
  inner_join(listing_reviews |> mutate(id = as.numeric(id)), by = "id")

# SUMMARY TABLE BY BOROUGH 
borough_summary <- combined_listings |>
  group_by(neighbourhood_group) |>
  summarise(
    `Listings (n)`          = n(),
    `Avg Reviews/Month`     = round(mean(reviews_per_month, na.rm = TRUE), 2),
    `Avg Review Score`      = round(mean(review_scores_rating, na.rm = TRUE), 2),
    `Avg Availability (days)` = round(mean(availability_365, na.rm = TRUE), 0),
    `Avg Min. Nights`       = round(mean(minimum_nights, na.rm = TRUE), 1)
  ) |>
  arrange(desc(`Avg Reviews/Month`)) |>
  rename(Borough = neighbourhood_group)

# PRINT TABLE 
borough_summary |>
  kable(
    caption = "Table 1. Summary statistics by NYC borough for Airbnb listings."
  ) |>
  kable_styling(
    bootstrap_options = c("striped", "hover", "condensed"),
    full_width = FALSE
  )
