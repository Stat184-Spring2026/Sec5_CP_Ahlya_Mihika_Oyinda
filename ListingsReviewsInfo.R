#LOAD PACKAGES
library(tidyverse)
library(dplyr)
library(esquisse)

#DETERMINING NULL VALUES, OUTLIERS, QUARTILES, ETC.
summary(listings)

#GEERATE NEW DF FOR WRANGLING
listings_cleaned <- listings |>
  #GETTING RID OF UNNECESSARY COLUMNS
  select(-c(name,host_profile_id,host_name,latitude,longitude,price,calculated_host_listings_count,license)) |>
  #DROPING NA'S
  drop_na(reviews_per_month, minimum_nights) |> 
  #FILTERING FOR AVAILABLE AIRBNBs
  filter(minimum_nights!=0,
         availability_365!=0) |>
  #FILTERING OUT ROOMS NOT INCLUDED IN ANALYSIS
  filter(room_type != "Shared room", 
         room_type != "Hotel room") |>
  #FILTERING MINIMUM NIGHTS LESS THAN A YEAR FOR SKEWNESS CONTROL
  filter(minimum_nights<366)

#CREATING NEW DF CONTANING ID AND REVIEW RATING
listing_reviews <- detailed.ny.listings |>
  select(id, review_scores_rating) |>
  mutate(review_scores_rating = as.numeric(trimws(review_scores_rating))) |>
  drop_na(review_scores_rating)

#DETERMINING NULL VALUES AND OUTLIERS, ETC.
summary(listings_cleaned)
colMeans(is.na(listings_cleaned)) * 100

#COMBINING BOTH DFs BY ID
combined_listings <- listings_cleaned |>
  mutate(id = as.numeric(id)) |>
  inner_join(listing_reviews |> mutate(id = as.numeric(id)), by = "id") |>
  #SPLITTING LAST_REVIEW INTO SEPARATE YEAR, MONTH, AND DAY TO SEE HOW MANY GENERAL BOOKINGS PER MONTH
  separate_wider_delim(
    last_review,
    delim = "-",
    names = c("year", "month", "day")
  ) |>
  #REMOVING YEAR AND DAY TO ONLY FOCUS ON MONTH
  select(-c(year,day)) |>
  rename(
    "last_month_booked" = "month"
  ) |>
  #MUTATE NUMERICAL MONTH VALUE INTO STRING VALUE (ex. January, February, etc)
  mutate(last_month_booked = month.name[as.integer(last_month_booked)]) |>
  #MAKE LEVELS OF EACH MONTH FOR DATA VISUALIZATION PURPOSES (CATEGORIES DO NOT TURN OUT ALPHABETICAL)
  mutate(last_month_booked = factor(last_month_booked, levels = month.name))

#DETERMINING NULL VALUES AND OUTLIERS, ETC.
summary(combined_listings)
colMeans(is.na(combined_listings)) * 100


write.csv(combined_listings, "ListingReviewsInfo(1).csv", row.names = FALSE)



