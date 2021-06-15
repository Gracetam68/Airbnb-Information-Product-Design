library(jsonlite)
library(readxl)
library(writexl)

library(readr)
library(dplyr)
library(stringr)

# read the file 
yelp_business <- "~/Desktop/BA/6213/project/yelp_academic_dataset_business.json"
review_lines_business <- read_lines(yelp_business, progress = FALSE)
business_combined <- str_c("[", str_c(review_lines_business, collapse = ", "), "]")

# set it as a dataframe
business <- fromJSON(business_combined) 
businessData <- as_tibble(business)
head(businessData)
names(businessData)
dim(businessData) # 209393*14, but we have many hidden attributes in the dataset
businessData$categories

# subset the dataset to only contain restaurants
businessInRestaurants <- subset(businessData, grepl("Restaurants", businessData$categories))
names(businessInRestaurants)
dim(businessInRestaurants) # 63944 *14,but we have many hidden attributes in the dataset
head(businessInRestaurants)

# subset the dataset to only contain Toronto
toronto <- subset(businessInRestaurants,city == "Toronto")
head(toronto) 
dim(toronto) # 8679*14
names(toronto)

# subset the dataset to only contain the opening restaurants
openedRestaurantsTo <- subset(toronto,is_open == 1)
head(openedRestaurantsTo)
View(openedRestaurantsTo)
dim(openedRestaurantsTo) # 5462 * 14
names(openedRestaurantsTo)

openedRestaurantsTo$categories
b <- subset(openedRestaurantsTo,name=="Banknote Bar")
b
b$latitude
b$longitude
write.csv(openedRestaurantsTo, "~/Desktop/BA/6213/project/torontoRestaurants.csv",
          row.names=FALSE)



### find the reviews for Toronto

# Loading 1 million reviews 
yelp_review <- "~/Desktop/BA/6213/project/yelp_academic_dataset_review.json"
review_lines <- read_lines(yelp_review, n_max = 500000, progress = FALSE)
reviews_combined <- str_c("[", str_c(review_lines, collapse = ", "), "]")

reviews <- fromJSON(reviews_combined) 
reviewsData <- as_tibble(reviews) 
head(reviewsData) # take a look at the dataset
summary(reviewsData) # no missing values
dim(reviewsData) # 500000 * 9
names(reviewsData)

# only pick some columns 
reviewsData <- data.frame(business_id = reviewsData$business_id,
                          stars = reviewsData$stars,
                          text = reviewsData$text)
head(reviewsData)
names(reviewsData)

# match the business_id from the toronto dataset with the review dataset
reviewsTorontoRestaurants <- reviewsData %>%
        inner_join(openedRestaurantsTo, by = "business_id") %>%
        group_by(business_id) 

head(reviewsTorontoRestaurants)
dim(reviewsTorontoRestaurants) # 19127*16
names(reviewsTorontoRestaurants)

# only pick some columns
reviewsTorontoRestaurants <- data.frame(business_id=reviewsTorontoRestaurants$business_id,
                                        restaurant_name = reviewsTorontoRestaurants$name,
                                        address = reviewsTorontoRestaurants$address,
                                        city = reviewsTorontoRestaurants$city,
                                        postal_code = reviewsTorontoRestaurants$postal_code,
                                        type=reviewsTorontoRestaurants$categories,
                                        rating = reviewsTorontoRestaurants$stars.y,
                                        review = reviewsTorontoRestaurants$text,
                                        review_count = reviewsTorontoRestaurants$review_count,
                                        latitude = reviewsTorontoRestaurants$latitude,
                                        longitude = reviewsTorontoRestaurants$longitude)

head(reviewsTorontoRestaurants)
dim(reviewsTorontoRestaurants) #19127 * 11
names(reviewsTorontoRestaurants)

# copy the latitude and longitude twice and put it back to the 
# reviewsTorontoRestaurants1 dataset for comprison.
reviewsTorontoRestaurants$latitude1 <- reviewsTorontoRestaurants$latitude
reviewsTorontoRestaurants$longitude1 <- reviewsTorontoRestaurants$longitude
names(reviewsTorontoRestaurants)

# reposition the two new columns next to latitude and longitude
reviewsTorontoRestaurants1 <- reviewsTorontoRestaurants %>% 
        relocate(latitude1, .after = latitude)  %>%
        relocate(longitude1, .after = longitude)

names(reviewsTorontoRestaurants1)

# remove the last three digits of latitude1 and longitude1
reviewsTorontoRestaurants1$latitude1 <- format(reviewsTorontoRestaurants1$latitude1,digits=2,nsmall=2)

reviewsTorontoRestaurants1$longitude1 <- format(reviewsTorontoRestaurants1$longitude1, digits=2, nsmall=2)
head(reviewsTorontoRestaurants1)
names(reviewsTorontoRestaurants1) 
dim(reviewsTorontoRestaurants1) # 19127    13

write.csv(reviewsTorontoRestaurants1, "~/Desktop/BA/6213/project/RatingTorontoRestaurants.csv",
          row.names=FALSE)

# average the rating for each business
rating_average <- reviewsTorontoRestaurants1 %>%                                        
        group_by(restaurant_name) %>%                         
        summarise_at(vars(rating),              
                     list(average_rating = mean))             
head(rating_average)
dim(rating_average) # 426*2

# join the airbnb listing dataset 
# import the listing2 dataset
listing <- read.csv("listings_2.csv")
head(listing)
dim(listing) # 15832*74
names(listing)
summary(listing)

# some columns are completely empty, drop these columns
listing <- subset(listing, select = -c(neighbourhood_group_cleansed,bathrooms,
                                       calendar_updated))
# remove missing values from the dataset
listing <- na.omit(listing)
head(listing)
dim(listing) # 10961 *71
names(listing)
summary(listing)

# remove some columns from the dataset 
# listing_url,scrape_id,last_scraped,picture_url,host_id,host_url,
# host_since,host_location,host_about,host_response_time,host_response_rate,
# host_acceptance_rate,host_thumbnail_url,host_picture_url,host_listings_count,
# host_total_listings_count,host_verifications,host_has_profile_pic,
# neighbourhood_cleansed,beds,amenities,minimum_nights,maximum_nights,
# minimum_minimum_nights,maximum_minimum_nights,minimum_maximum_nights,
# maximum_maximum_nights,minimum_nights_avg_ntm,maximum_nights_avg_ntm,
# has_availability,availability_30,availability_60,availability_90,
# availability_365,calendar_last_scraped,number_of_reviews_ltm,
# number_of_reviews_l30d,first_review,last_review,review_scores_accuracy,
# review_scores_cleanliness,review_scores_checkin,review_scores_communication,
# review_scores_location,review_scores_value,license,calculated_host_listings_count,
# calculated_host_listings_count_entire_homes,calculated_host_listings_count_private_rooms,
# calculated_host_listings_count_shared_rooms,reviews_per_month

listing <- subset(listing, select = -c(listing_url,scrape_id,last_scraped,
                                       picture_url,host_id,host_url,
                                       host_location,host_about,
                                       host_thumbnail_url,
                                       host_picture_url,host_listings_count,
                                       host_verifications,
                                       minimum_minimum_nights,maximum_minimum_nights,
                                       maximum_maximum_nights,minimum_nights_avg_ntm,
                                       maximum_nights_avg_ntm,
                                       availability_30,
                                       availability_60,availability_365,
                                       calendar_last_scraped,
                                       number_of_reviews_ltm,
                                       number_of_reviews_l30d,
                                       review_scores_accuracy,
                                       review_scores_cleanliness,review_scores_checkin,
                                       review_scores_communication,
                                       review_scores_location,review_scores_value,
                                       license,
                                       calculated_host_listings_count_entire_homes,
                                       calculated_host_listings_count_private_rooms,
                                       calculated_host_listings_count_shared_rooms,
                                       reviews_per_month
))

dim(listing) # 10961 *37
names(listing)
summary(listing)

# copy the latitude and longitude twice and put it back to the airbnb dataset
# for comprison.
listing$latitude1 <- listing$latitude
listing$longitude1 <- listing$longitude
names(listing)

# reposition the two new columns next to latitude and longitude
listing1 <- listing %>% 
        relocate(latitude1, .after = latitude)  %>%
        relocate(longitude1, .after = longitude)

names(listing1)

# remove the last three digits of latitude1 and longitude1
listing1$latitude1 <- format(listing1$latitude1,digits=2,nsmall=2)

listing1$longitude1 <- format(listing1$longitude1, digits=2, nsmall=2)
head(listing1)
names(listing1)

# join reviewsTorontoRestaurants1 table with listing1 table by latitude1 and 
# longitude1
merged <- reviewsTorontoRestaurants1  %>%
        inner_join(listing1, by = c("latitude1","longitude1")) 


head(merged,20) 
View(merged)
dim(merged) # 3148636      50
names(merged)
head(merged$latitude.x)

write.csv(merged, "~/Desktop/BA/6213/project/merged.csv",
          row.names=FALSE)

# find out the number of restaurants around each home
count1 <- merged  %>%
        group_by(id,name,latitude.y,longitude.y,property_type,room_type,amenities,
                 host_neighbourhood,
                 bedrooms,beds,price,number_of_reviews,
                 review_scores_rating,instant_bookable,
                 business_id,restaurant_name,address,postal_code,type,city,
                 latitude.x,longitude.x) %>%
        summarise(restaurant_average_rating = mean(rating),
                  restaurant_average_review_count = mean(review_count)) %>%
        rename(home_name=name,
               restaurant_address=address,
               restaurant_latitude=latitude.x,
               restaurant_longitude=longitude.x,
               home_latitude=latitude.y,
               home_longitude=longitude.y) %>%
        filter(home_name != "") 

head(count1,50)
View(count1)
names(count1)
dim(count1) #  50418    23

write.csv(count1, "~/Desktop/BA/6213/project/count1.csv",
          row.names=FALSE)

# calculate the distance between each apartment and each restaurants 
# do the longitude first, then latitude

library(geosphere)
dist<-distGeo(count1[, c("restaurant_latitude", "restaurant_longitude")], 
              count1[, c("home_latitude", "home_longitude")])/1609.344
mins <- dist * 30
mins_round <- round(mins)
df<-cbind(count1, distance_miles=dist,mins = mins,mins_walk=mins_round)
dim(df) #87127    24
names(df)

df$miles <- as.numeric(df$distance_miles)

# round the miles to two decimal points
df$miles <- round(df$miles, digits=2)


# remove distance miles and mins 
Yelp_Airbnb2 <- subset(df, select = -c(distance_miles,mins))
View(Yelp_Airbnb2)


summary(Yelp_Airbnb2)
dim(Yelp_Airbnb2) # 87127    23

##
table(Yelp_Airbnb2$host_neighbourhood)
table(Yelp_Airbnb2$neighbourhood)
table(Yelp_Airbnb2$host_is_superhost)
table(Yelp_Airbnb2$property_type)
table(Yelp_Airbnb2$room_type)

abc <- Yelp_Airbnb2 %>%
              filter(home_name=='Bright and cozy apt with view to rinity Bellwoods')
View(abc)
write.csv(Yelp_Airbnb2, "~/Desktop/BA/6213/project/Yelp_Airbnb2.csv",
          row.names=FALSE)



###
# read the Yelp_Airbnb2 file 
Yelp_Airbnb <- read.csv('Yelp_Airbnb2.csv')
names(Yelp_Airbnb)


# the numbers of restaurants around each apartment
grouped <- Yelp_Airbnb  %>%
        group_by(id,home_name,home_latitude,home_longitude,
                 property_type,room_type,amenities,
                 host_neighbourhood,bedrooms,beds,
                 price,number_of_reviews,review_scores_rating,
                 instant_bookable) %>%
        summarise(restaurant_counts = n_distinct(business_id)) 

head(grouped,50)
tail(grouped,50)
dim(grouped) # 8074   15
View(grouped)

write.csv(grouped, "~/Desktop/BA/6213/project/Yelp_Airbnb3.csv",
          row.names=FALSE)

# combine the crime rate dataset 
# read the crime rate file 
crime <- read.csv("airbnb_crime.csv")
head(crime)
dim(crime) # 15832*13
names(crime)
summary(crime)

# remove X column
crime$X <- NULL

# remove missing values 
crime_omit <- na.omit(crime)
head(crime_omit)
dim(crime_omit) # 15641*12
names(crime_omit)
summary(crime_omit)

# combine the clean crime dataset with grouped 
restaurants_crime <- grouped  %>%
        inner_join(crime_omit, by = c("id")) 
head(restaurants_crime)
dim(restaurants_crime) # 8042   26
names(restaurants_crime)
summary(restaurants_crime)


# remove price.y, name columns
restaurants_crime$name <- NULL
restaurants_crime$price.y <- NULL
dim(restaurants_crime) # 7598*33

restaurants_crime$description <- NULL
restaurants_crime$neighbourhood_cleansed <- NULL
View(restaurants_crime)

# rename price.x to price
restaurants_crime <- plyr::rename(restaurants_crime, c("price.x" = "price"))
dim(restaurants_crime) # 8042   22

write.csv(restaurants_crime, "~/Desktop/BA/6213/project/restaurants_crime.csv",
          row.names=FALSE)



# only select one home and restaurants
restaurant <- read.csv("Yelp_Airbnb_mile_rating.csv")
head(restaurant)

one_restaurant <- restaurant %>%
        filter(id==1419)

write.csv(one_restaurant, "~/Desktop/BA/6213/project/one_restaurant.csv",
          row.names=FALSE)



# combine yelp_airbnb2 with listing
Yelp_Airbnb2 <- read.csv('Yelp_Airbnb2.csv')
names(Yelp_Airbnb2)
dim(Yelp_Airbnb2)

# subset the listing one with only keeping the id and neighbourhood_cleansed columns
listing3 <- subset(listing, select = c(id,neighbourhood_cleansed))
names(listing3)
summary(listing3)
dim(listing3) # 10961     2

# combination
Yelp_Airbnb_visu <- listing3  %>%
        inner_join(Yelp_Airbnb2, by = c("id")) 
head(Yelp_Airbnb_visu)
dim(Yelp_Airbnb_visu) # 50418    27
names(Yelp_Airbnb_visu)
summary(Yelp_Airbnb_visu)
write.csv(Yelp_Airbnb_visu, "~/Desktop/BA/6213/project/Yelp_Airbnb_visu.csv",
          row.names=FALSE)




