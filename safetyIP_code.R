library(data.table)
library(dplyr)



airbnb <- read.csv("listings.csv")
crime <- read.csv("crime.csv")

#same amount of neighborhoods in both datasets, 140 total.
unique(airbnb$neighbourhood_cleansed)
unique(crime$Neighbourhood)

###NOTE: need some baseline to determine overall safety. 
###For now we will use the average rating of Toronto as the baseline
#Or we can use the 20th percentage as the baseline since Toronto is a safe city


#use weighted average rating of assault, autotheft, robbery, break and enter
#allocation (25% 25% 25% 25%)
#the lower the better
crime$safety_rating <- crime$Assault_Rate_2019*0.20 + 
  crime$AutoTheft_Rate_2019*0.10 + crime$Robbery_Rate_2019*0.25 + 
  crime$BreakandEnter_Rate_2019*0.10 + crime$Homicide_Rate_2019*0.15 +
  crime$TheftOver_Rate_2019*0.20

#combine the two datasets
colnames(crime)[2] <- "neighbourhood_cleansed"

head(setDT(airbnb)[setDT(crime), on = c("neighbourhood_cleansed"), 
                safety := safety_rating])
head(setDT(airbnb)[setDT(crime), on = c("neighbourhood_cleansed"), 
                   Assault_Rate_2019 := Assault_Rate_2019])
head(setDT(airbnb)[setDT(crime), on = c("neighbourhood_cleansed"), 
                   AutoTheft_Rate_2019 := AutoTheft_Rate_2019])
head(setDT(airbnb)[setDT(crime), on = c("neighbourhood_cleansed"), 
                   Robbery_Rate_2019 := Robbery_Rate_2019])
head(setDT(airbnb)[setDT(crime), on = c("neighbourhood_cleansed"), 
                   BreakandEnter_Rate_2019 := BreakandEnter_Rate_2019])
head(setDT(airbnb)[setDT(crime), on = c("neighbourhood_cleansed"), 
                   TheftOver_Rate_2019 := TheftOver_Rate_2019])
head(setDT(airbnb)[setDT(crime), on = c("neighbourhood_cleansed"), 
                   Homicide_Rate_2019 := Homicide_Rate_2019])


##select the columns of interest
airbnb_cleaned <- select(airbnb, id, name, description, property_type, room_type, accommodates, bathrooms_text, bedrooms, beds, amenities,
                         neighbourhood_cleansed, price, safety, Assault_Rate_2019, AutoTheft_Rate_2019, Robbery_Rate_2019, 
                         BreakandEnter_Rate_2019, TheftOver_Rate_2019, Homicide_Rate_2019)


#finding the percentiles of the safety ratings
quantile(crime$safety_rating, robs = seq(.1, .9, by = .1))
#       0%      25%      50%      75%     100% 
# 88.7600 145.9538 206.7625 262.4213 986.9300

#need to remove outliers
boxplot(crime$safety_rating)
Q <- quantile(crime$safety_rating, probs=c(.25, .75), na.rm = FALSE)
iqr <- IQR(crime$safety_rating)
up <-  Q[2]+1.5*iqr # Upper Range  
low<- Q[1]-1.5*iqr # Lower Range
#upper range at 437.1225


#assign safety levels to the neighborhoods.
airbnb_cleaned$safety_level <- 
  ifelse(airbnb_cleaned$safety <= 145.9538, "Excellent",
         ifelse(airbnb_cleaned$safety <= 437.1225, "Good", "Poor"))


#export data into 
write.csv(airbnb, file = "airbnb_crime_data.csv")

write.csv(airbnb_cleaned, file = "airbnb_crime_data_cleaned.csv")


