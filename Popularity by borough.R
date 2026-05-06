# NYC AIRBNB ANALYSIS - PLOT 1 
# Listing Popularity by Borough



# LOAD PACKAGES ----
library(tidyverse)

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

# SUMMARIZE DATA FOR PLOT 
borough_pop <- combined_listings |>
  group_by(neighbourhood_group) |>
  summarise(avg_reviews_pm = mean(reviews_per_month, na.rm = TRUE)) |>
  mutate(neighbourhood_group = fct_reorder(neighbourhood_group, avg_reviews_pm))

# PLOT 1: Average Reviews Per Month by Borough
plot1 <- ggplot(borough_pop, aes(x = neighbourhood_group,
                                 y = avg_reviews_pm,
                                 fill = neighbourhood_group)) +
  geom_col(show.legend = FALSE, width = 0.6) +
  geom_text(aes(label = round(avg_reviews_pm, 2)),
            vjust = -0.5, size = 4, fontface = "bold") +
  scale_fill_manual(values = c(
    "Staten Island" = "#8ecae6",
    "Bronx"         = "#219ebc",
    "Manhattan"     = "#023047",
    "Brooklyn"      = "#ffb703",
    "Queens"        = "#fb8500"
  )) +
  scale_y_continuous(
    limits = c(0.7, 1.2),
    oob    = scales::squish
  ) +
  labs(
    title    = "Listing popularity by NYC borough",
    subtitle = "Measured by average number of reviews per month",
    x        = "Borough",
    y        = "Avg reviews per month",
    caption  = "Source: NYC Airbnb listings dataset"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title         = element_text(face = "bold", size = 14),
    plot.subtitle      = element_text(color = "gray40", size = 11),
    plot.caption       = element_text(color = "gray50", size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor   = element_blank(),
    axis.text.x        = element_text(size = 11)
  )

print(plot1)
