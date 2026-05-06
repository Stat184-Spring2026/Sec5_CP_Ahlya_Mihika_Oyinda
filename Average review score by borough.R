# 
# NYC AIRBNB ANALYSIS - PLOT 2
# Average Review Score by Borough


# LOAD PACKAGES 
library(tidyverse)

# LOAD DATA 
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
borough_rating <- combined_listings |>
  group_by(neighbourhood_group) |>
  summarise(avg_rating = mean(review_scores_rating, na.rm = TRUE)) |>
  mutate(neighbourhood_group = fct_reorder(neighbourhood_group, avg_rating))

# PLOT 2: Average Review Score by Borough
plot2 <- ggplot(borough_rating, aes(x = neighbourhood_group,
                                    y = avg_rating,
                                    fill = neighbourhood_group)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = round(avg_rating, 2)),
            vjust = -0.5, size = 4) +
  scale_y_continuous(limits = c(0, 5)) +
  labs(
    title   = "Average review score by NYC borough",
    x       = "Borough",
    y       = "Avg review score (out of 5)",
    caption = "Source: NYC Airbnb listings dataset"
  ) +
  theme_minimal()

print(plot2)
