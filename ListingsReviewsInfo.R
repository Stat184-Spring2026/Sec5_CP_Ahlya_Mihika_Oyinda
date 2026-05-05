#LOAD PACKAGES
library(tidyverse)
library(dplyr)
library(esquisse)

#DETERMINING NULL VALUES, OUTLIERS, QUARTILES, ETC.
summary(listings)

#GEERATE NEW DF FOR WRANGLING
listings_cleaned <- listings |>
  #GETTING RID OF UNNECESSARY COLUMNS
  select(-c(name,host_profile_id,host_name,latitude,longitude,price,last_review,calculated_host_listings_count,license)) |>
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
  inner_join(listing_reviews |> mutate(id = as.numeric(id)), by = "id")


write.csv(combined_listings, "ListingReviewsInfo(1).csv", row.names = FALSE)


