setwd("/Users/yendl/oneDrive/Desktop/R") 
data <- read.table("New York.csv", sep=",", header=T,strip.white = T, na.strings = c("NA","NaN","","?"))
str(data)
View(data)

library(mice)
library(lattice)
# Variable Importance 
install.packages("Boruta", repos = "https://cran.r-project.org")
library(ranger)
library(Boruta)


summary(data$price)
attach(data)

data$pricebin <- cut(data$price, breaks = c(0,200,334), labels = c('>=200','<200'))
View(data$pricebin)


data$pricebin <- as.factor(ifelse(data$pricebin==">=200", 0, 1))

View(data$pricebin)
summary(data$pricebin)
data$pricebin[is.na(data$pricebin)] <- 0
summary(data$price)


sum(is.na(data))

boruta <- Boruta(data$pricebin~., data = data, doTrace = 2)
print(boruta)



summary(data)
View(data$maximum_nights)
View(data)

#outliers (price)
boxplot(data$price)
bench <- 175 + 1.5*IQR(data$price)

data$price[data$price >334]

data$price[data$price > bench] <-bench
summary(data$price)

#outliers (number of outliers)
boxplot(data$number_of_reviews)
summary(data$number_of_reviews)
bench <- 19 + 1.5*IQR(data$number_of_reviews)

data$number_of_reviews[data$number_of_reviews >46]

data$number_of_reviews[data$number_of_reviews > bench] <-bench
summary(data$number_of_reviews)

#outliers (availability_30)
boxplot(data$availability_30)
summary(data$availability_30)
bench <- 9 + 1.5*IQR(data$availability_30)

data$availability_30[data$availability_30 >22.5]

data$availability_30[data$availability_30 > bench] <-bench
summary(data$availability_30)

#outliers (max_nights)
boxplot(data$maximum_nights)
summary(data$maximum_nights)
bench <- 1.125e+03 + 1.5*IQR(data$maximum_nights)

data$maximum_nights[data$maximum_nights >2769]

data$maximum_nights[data$maximum_nights > bench] <-bench
summary(data$maximum_nights)

summary(data$bedrooms)
summary(data$beds)



#replacing Na values with mode(host)
summary(data$host_has_profile_pic)
data$host_has_profile_pic[is.na(data$host_has_profile_pic)] <- 't'
data$host_has_profile_pic[!is.na(data$host_has_profile_pic)]

#bedrooms
summary(data$bedrooms)
data$bedrooms[is.na(data$bedrooms)] <- 1

#beds
summary(data$beds)
data$beds[is.na(data$beds)] <- 1




summary(data$host_is_superhost)
data$host_is_superhost[is.na(data$host_is_superhost)] <- 'f'
data$host_is_superhost[!is.na(data$host_is_superhost)]

#summary(data$host_response_rate)
summary(data$host_response_time)
data$host_response_rate[data$host_response_rate== "N/A"] <- NA
#data$host_response_rate[is.na(data$host_response_rate)]
#data$host_response_rate[!is.na(data$host_response_rate)]

#response time
summary(data$host_response_time)
data$host_response_time[data$host_response_time == "N/A"] <- NA
data$host_response_time[is.na(data$host_response_time)] <- 'within an hour'


# Guests Included
summary(data$guests_included)

#max_nights
summary(data$maximum_nights)

#min_nights
summary(data$minimum_nights)

#cal
summary(data$calendar_updated)

#availability
summary(data$availability_30)

#number of review
summary(data$number_of_reviews)

#reviews
summary(data$review_scores_rating)
data$review_scores_rating[is.na(data$review_scores_rating)] <-  96

# instant 
summary(data$cancellation_policy)


summary(data$reviews_per_month)
data$reviews_per_month[is.na(data$reviews_per_month)] <- 1.436
data$reviews_per_month[!is.na(data$reviews_per_month)] 

summary(data$bathrooms)
data$bathrooms[is.na(data$bathrooms)] <- 1
View(data$bathrooms)
summary(data$price)

summary(data$neighbourhood_cleansed)
sum(is.na(data$neighbourhood_cleansed))

summary(data$room_type)

# removed variable
data$square_feet <- NULL
data$host_response_rate <- NULL

write.csv(data, 'data000.csv')

sum(is.na(data))
sapply(data,function(x) sum(is.na(x)))
summary(data$calendar_updated)
data$rev<-multiply(data$price, data$amenities)

data$rev<-(data$price) * (data$availability_30)
View(data$rev)
write.csv(data,"data001.csv")





