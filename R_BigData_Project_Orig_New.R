#install.packages("dplyr")
#install.packages("CatEncoders")

#install Libraries
library(randomForest)
library(caret)
library(ggplot2)
library(lattice)
f <- file.choose()
#library(dplyr)
df <- read.csv(f)
print(df)
dim(df)

View(df)
#EDA

#CHUNK_1
# Create the countplot using base R plotting functions
#Hotel types and cancellation counts
counts <- table(df$is_canceled,df$hotel)
View(counts)
barplot(counts, beside = TRUE, col = rainbow(2),
        xlab = "Hotel", ylab = "Count", main = "Countplot by Hotel and Cancellation Status",
        names.arg = levels(df$hotel))

legend("topright", legend = c("0", "1"), fill = rainbow(2),
       title = "is_canceled",cex=0.6)


#CHUNK_2
#months against cancellation
counts2<- table(df$is_canceled,df$arrival_date_month)
View(counts2)
barplot(counts2, beside = TRUE, col = rainbow(2),
        xlab = "Months", ylab = "Count", main = "Countplot by Months and Cancellation Status",
        names.arg = levels(df$arrival_date_month),cex.names = 0.7)

legend("topright", legend = c("0", "1"), fill = rainbow(2),
       title = "is_canceled",cex = 0.6)


#CHUNK_3
#meals against cancellation
counts3<- table(df$is_canceled,df$meal)
View(counts3)
barplot(counts3, beside = TRUE, col = rainbow(2),
        xlab = "Meal Types", ylab = "Count", main = "Countplot by Meals and Cancellation Status",
        names.arg = levels(df$meal),cex.names = 0.7)

legend("topright", legend = c("0", "1"), fill = rainbow(2),
       title = "is_canceled",cex = 0.6)


#CHUNK_4
#density plot 
# Separate the lead time values by cancellation status
lead_time_not_canceled <- na.omit(df$lead_time[df$is_canceled == 0])
lead_time_canceled <- na.omit(df$lead_time[df$is_canceled == 1])

# Plot the kernel density estimates
plot(density(lead_time_not_canceled), col = "blue", main = "Kernel Density Plot by Lead Time and Cancellation Status",
     xlab = "Lead Time", ylab = "Density")
lines(density(lead_time_canceled), col = "red")
legend("topright", legend = c("Not Canceled", "Canceled"), col = c("blue", "red"), cex=0.7,lwd=0.5)

#CHUNK_5
#boxplot
df_not_canceled <- subset(df, is_canceled == 0)

# Create the boxplot
boxplot(adr ~ reserved_room_type, data = df_not_canceled, 
        main = "Boxplot of ADR by Reserved Room Type (Not Canceled)", 
        xlab = "Reserved Room Type", ylab = "ADR", 
        col = c("red", "blue"), border = "black")

# Add a legend
legend("topright", legend = unique(df_not_canceled$hotel), 
       fill = c("red", "blue"), border = "black", title = "Hotel",cex = 0.6)




#CHUNK_6
# Create a table of counts for each combination of 'wanted_unwanted_room' and 'is_canceled'
df$wanted_unwanted_room <- ifelse(df$reserved_room_type == df$assigned_room_type, "wanted", "unwanted")
View(df$wanted_unwanted_room)

counts4 <- table(df$is_canceled,df$wanted_unwanted_room)
View(counts4)
# Plot the countplot using barplot
barplot(counts4, beside = TRUE, col = c("blue", "red"), 
        xlab = "wanted_unwanted_room", ylab = "Count", 
        main = "Countplot by wanted_unwanted_room and Cancellation Status")

# Create a legend
legend("topright", legend = c("Not Canceled", "Canceled"), 
       fill = c("blue", "red"),cex=0.4)


#CHUNK_7
ggplot(data = df) +
  geom_bar(mapping=aes(x=distribution_channel, fill=deposit_type)) +
  facet_wrap(~deposit_type) +
  theme(axis.text.x = element_text(angle=45)) +
  labs(title='Deposit Type vs. Distribution Channel', x='Distribution Channel', y='Deposit Type Proportion')


#CHUNK_8
counts5 <- table(df$is_canceled,df$deposit_type)
View(counts5)
barplot(counts5, beside = TRUE, col = rainbow(2),
        xlab = "Deposit Type", ylab = "Count", main = "Countplot by Deposit Type and Cancellation Status",
        names.arg = levels(df$deposit_type))

legend("topright", legend = c("0", "1"), fill = rainbow(2),
       title = "is_canceled",cex=0.6)



#CHUNK_9
#Hypothesis Testing
hotel_summary <- aggregate(lead_time ~ hotel, data = df,
                           FUN = function(x) c(mean_lead_time = mean(x),
                                               max_lead_time = max(x),
                                               min_lead_time = min(x)))

head(hotel_summary)

ggplot(data = df) +
  geom_point(mapping = aes(x = lead_time, y = children))


#CHUNK_10
counts5 <- table(df$is_canceled,df$deposit_type)
View(counts5)
barplot(counts5, beside = TRUE, col = rainbow(2),
        xlab = "Deposit Type", ylab = "Count", main = "Countplot by Deposit Type and Cancellation Status",
        names.arg = levels(df$deposit_type))

legend("topright", legend = c("0", "1"), fill = rainbow(2),
       title = "is_canceled",cex=0.6)


#CHUNK_11
# Subset the data for city hotel bookings
city_hotel <- subset(df, hotel == "City Hotel" & is_canceled == 0)

# Aggregate the city hotel bookings count by month
bookings_city_hotel_month <- aggregate(hotel ~ arrival_date_month, data = city_hotel, FUN = length)
colnames(bookings_city_hotel_month) <- c("month", "city_hotel")

# Subset the data for resort hotel bookings
resort_hotel <- subset(df, hotel == "Resort Hotel" & is_canceled == 0)

# Aggregate the resort hotel bookings count by month
bookings_resort_hotel_month <- aggregate(hotel ~ arrival_date_month, data = resort_hotel, FUN = length)
colnames(bookings_resort_hotel_month) <- c("month", "resort_hotel")

# Combine the bookings data
total_bookings <- merge(bookings_city_hotel_month, bookings_resort_hotel_month, by = "month")

# Define the order of months
month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

# Sort the data by month order
total_bookings <- total_bookings[match( month_order,total_bookings$month), ]

# Plotting
plot(total_bookings$city_hotel, type = "l", xlab = "Month", ylab = "No. of Guests",
     main = "Bookings in City Hotel and Resort Hotel by Month (Non-Canceled)", 
     xlim = c(1, 12), ylim = c(0, max(total_bookings$city_hotel, total_bookings$resort_hotel)), 
     xaxt = "n", col = "blue")
lines(total_bookings$resort_hotel, col = "red")
axis(1, at = 1:12, labels = month_order)
legend("topright", legend = c("City Hotel", "Resort Hotel"), col = c("blue", "red"), lty = 1,cex = 0.5)


#HYPOTHESIS TESTING 
####Hypothesis #1

#Null Hypothesis: There is no difference in the lead time between resort hotels and city hotels.
#Alternative Hypothesis: There is a difference in the lead time between resort hotels and city hotels.

# Subset the data for resort and city hotels
resort_bookings <- subset(df, hotel == "Resort Hotel")
city_bookings <- subset(df, hotel == "City Hotel")

# Perform t-test
t.test(resort_bookings$lead_time, city_bookings$lead_time, var.equal=TRUE)
qt(0.05/2,df=119388,lower.tail = FALSE)

# Perform a Welch's t-test 
t.test(resort_bookings$lead_time, city_bookings$lead_time, var.equal=FALSE)
qt(0.05/2,df=90291,lower.tail = FALSE)

####Hypothesis #2

#Null Hypothesis: There is no difference in the mean number of nights guests stay between weekends and weekdays in resort hotel bookings.
#Alternative Hypothesis:There is difference in the mean number of nights guests stay between weekends and weekdays in resort hotel bookings.

resort_hotel <- subset(df, hotel == "Resort Hotel" & is_canceled == 0)

# Subset the data for weekend and weekday nights in resort hotel bookings
weekend_nights <- subset(resort_hotel, select = c("stays_in_weekend_nights"))
weekday_nights <- subset(resort_hotel, select = c("stays_in_week_nights"))


# Perform t-test
t.test(weekend_nights, weekday_nights, var.equal=TRUE)
qt(0.05/2,df=57874,lower.tail = FALSE)


# Perform a Welch's t-test 
t.test(weekend_nights, weekday_nights, var.equal=FALSE)
qt(0.05/2,df=40981,lower.tail = FALSE)




####Hypothesis #3

#Null Hypothesis: There is no difference in the booking cancellation rates between resort hotels and city hotels.
#Alternative Hypothesis: There is a difference in the booking cancellation rates between the two types of hotels.


# Separate the data into two groups: resort hotels and city hotels
resort_cancelled <- df$is_canceled[df$hotel == "Resort Hotel"]
city_cancelled <- df$is_canceled[df$hotel == "City Hotel"]

# Perform a t-test
t.test(resort_cancelled, city_cancelled, var.equal = TRUE)
qt(0.05/2,df=119388,lower.tail = FALSE)

# Perform a Welch's t-test 
t.test(resort_cancelled, city_cancelled, var.equal = FALSE)
qt(0.05/2,df=87554,lower.tail=FALSE)


###Data Preprocessing 

# drop nulls from columns
df <- df[!is.na(df$is_canceled), ]

# Calculate the number of null values in each column
null_counts <- colSums(is.na(df))

# Print the result
print(null_counts)





# replace nulls or empty spaces with zero agent
#df$agent[is.na(df$agent) | df$agent == ""] <- 0
#print(df[,"agent"])

# drop unwanted columns
df <- subset(df, select = -c(arrival_date_week_number))
df <- subset(df, select = -c(arrival_date_day_of_month))
df <- subset(df, select = -c(reservation_status))
df <- subset(df, select = -c(arrival_date_year))
df <- subset(df, select = -c(assigned_room_type))
df <- subset(df, select = -c(booking_changes))
df <- subset(df, select = -c(company))
df <- subset(df, select = -c(email))
df <- subset(df, select = -c(phone_number))
df <- subset(df, select = -c(credit_card))
df <- subset(df, select = -c(name))
df <- subset(df, select = -c(wanted_unwanted_room))
View(df)

#fill null values
df$children <- ifelse(is.na(df$children), 0, df$children)
df$country <- ifelse(is.na(df$country), mode(df$country)[1], df$country)
df$agent <- ifelse(is.na(df$agent), 0, df$agent)
#count <- sum(is.na(df$country))
#print(count)
# Get the data type of columns in a dataframe
#str(df)
# Create a list of indices where the sum of adults, children, and babies is 0
no_guests <- as.list(which(df$adults + df$children + df$babies == 0))

# Create a list of indices where the number of adults is 0
no_adults <- as.list(which(df$adults == 0))

# Combine the indices from no_guests and no_adults
indices_to_drop <- unique(c(no_guests, no_adults))

# Drop the rows from the DataFrame
df <- df[setdiff(seq_len(nrow(df)), indices_to_drop), ]

# Check the shape of the DataFrame
count <- sum(df$adults == 0)
print(count)


# Get column names of character features
str(df)
character_features <- names(df)[sapply(df, is.character)]
# Convert character features to factor
df[character_features] <- lapply(df[character_features], as.factor)
# Assign character features to categorical features
categorical_features <- character_features
# Get column names of continuous features
continuous_feature <- names(df)[sapply(df, is.numeric)]



df_cat <- df[categorical_features]
df_num <- df[continuous_feature]

#CHECKING SKEWNESS
hist(df_num[["lead_time"]], main = "Lead Time", xlab = "Value", ylab = "Frequency",col='lightblue')
hist(df_num$adr[df$adr>=0&df$adr<=500],xlab="Value",main="Average Daily Rate",breaks=20,col='purple')


# ###### for observingd outliers

# Plot boxplot for each column and remove outliers
lapply(df_num, function(x) boxplot(x, outline = FALSE))

# 
# ###########Dropping Outliers





######Normalization
View(df_cat)
View(df_num)
summary(df_num)
df_num$lead_time<-df_num$lead_time+1
df_num$lead_time<-log(df_num$lead_time)

#########other methods previously used
# #MINMAX scale to df_num
# #Identify the numerical columns
# columns_to_scale<-c("lead_time","adr")
# # # Loop through each numerical column and scale the values
# # for (col in columns_to_scale) {
# #   df_num[[col]] <- (df_num[[col]] - min(df_num[[col]])) / (max(df_num[[col]]) - min(df_num[[col]]))
# # }
# ##Normalization with z-score
# for (col in columns_to_scale) {
#   df_num[[col]] <- scale(df_num[[col]])
# }


###checking skewness after handling
hist(df_num[["lead_time"]], main = "Lead Time", xlab = "Value", ylab = "Frequency",col='lightblue')
hist(df_num$adr,xlab="Value",main="Average Daily Rate",breaks=20,col='purple')




####categorical preprocessing
# Convert reservation_status_date column to character type
df_cat$reservation_status_date <- as.character(df_cat$reservation_status_date)

# Extracting year and month from reservation_status_date
df_cat$year <- sapply(strsplit(df_cat$reservation_status_date, "/"), function(x) as.integer(x[3]))
df_cat$month <- sapply(strsplit(df_cat$reservation_status_date, "/"), function(x) as.integer(x[1]))
#View(df_cat)
df_cat <- subset(df_cat, select = -c(reservation_status_date))

#Encoding categorical features
df_cat <- lapply(df_cat, function(x) as.integer(factor(x)))
df_cat <- as.data.frame(df_cat)




#Concat categorical and numerical dfs
df_new <- cbind(df_num, df_cat)
View(df_new)



# Calculate the correlation of each column with a single column
correlation <- cor(df_new[, "is_canceled"], df_new)
View(correlation)


summary(df_new)
# y <- df$is_canceled
set.seed(42)  # Set the random seed for reproducibility
train_indices <- createDataPartition(df_new$is_canceled, p = 0.8, list = FALSE)
train <- df_new[train_indices, ]
test <- df_new[-train_indices, ]

# Define the train control settings
ctrl <- trainControl(method = "cv", number = 5, verboseIter = TRUE)

train$is_canceled <- factor(train$is_canceled, levels = c(0, 1))
# Train a classification model (e.g., XGB)
model_1 <- train(is_canceled ~ ., data = train, method = "xgbTree", trControl = ctrl)

#Make predictions on train data
y_train_pred<-predict(model_1, newdata = train)
cat("Train ACCURACY: ",mean(y_train_pred==train$is_canceled),"\n")


# Make predictions on the test data
y_pred <- predict(model_1, newdata = test)

cat("TEST ACCURACY: ",mean(y_pred==test$is_canceled),"\n")


# Convert predicted values to factor with correct levels
predictions <- factor(y_pred, levels = c(0, 1))
actual <- factor(test$is_canceled, levels = c(0, 1))

# Create confusion matrix
cm <- confusionMatrix(predictions, actual)
print(cm)


# Train a classification model (e.g., Randomforest)
model_2 <- train(is_canceled ~ ., data = train, method = "rf", trControl = ctrl)

#Make predictions on train data
y_train_pred2<-predict(model_2, newdata = train)
cat("Train ACCURACY: ",mean(y_train_pred2==train$is_canceled),"\n")

# Make predictions on the test data
y_pred_2 <- predict(model_2, newdata = test)

cat("TEST ACCURACY: ",mean(y_pred_2==test$is_canceled),"\n")
# Convert predicted values to factor with correct levels
predictions_2 <- factor(y_pred_2, levels = c(0, 1))
actual_2 <- factor(test$is_canceled, levels = c(0, 1))

# Create confusion matrix
cm_2 <- confusionMatrix(predictions_2, actual_2)
print(cm_2)
