# Will ETA be greater than 15 mins for a given flight?
# PART 1 CLEANING DATA
# Import flights dataset after dowloading from the TSA website

library(readr)
setwd("~/Google Drive/ix 2016 /Project3")
flights <- read.csv2('flights.csv', sep=",", header=TRUE, stringsAsFactors = FALSE)
View(flights)

# Select 10 busiest airports in the US by total passenger traffic

airports <-c('ATL','LAX','ORD','DFW','DEN','JFK','SFO','MIA','CLT','LAS')
flights <- subset(flights, DEST %in% airports & ORIGIN %in% airports)

#	Clean data

flights$X <- NULL 
flights$ORIGIN_AIRPORT_SEQ_ID <- NULL 
flights$DEST_AIRPORT_SEQ_ID <- NULL 
flights$UNIQUE_CARRIER <- NULL

# exploratory data analysis

# Group flights 

ontime <- flights[!is.na(flights$ARR_DEL15) & flights$ARR_DEL15!="" & !is.na(flights$DEP_DEL15) & flights$DEP_DEL15!="",]

# Change data type to run model

ontime$DEST_AIRPORT_ID <- as.factor(ontime$DEST_AIRPORT_ID)
ontime$ORIGIN_AIRPORT_ID <- as.factor(ontime$ORIGIN_AIRPORT_ID)
ontime$DAY_OF_WEEK <- as.factor(ontime$DAY_OF_WEEK)
ontime$DISTANCE <- as.integer(ontime$DISTANCE)
ontime$CANCELLED <- as.integer(ontime$CANCELLED)
ontime$DIVERTED <- as.integer(ontime$DIVERTED)
ontime$ORIGIN <- as.factor(ontime$ORIGIN)
ontime$DEP_TIME_BLK <- as.factor(ontime$DEP_TIME_BLK)
ontime$CARRIER <- as.factor(ontime$CARRIER)
ontime$ARR_DEL15 <- as.factor(ontime$ARR_DEL15)
ontime$DEP_DEL15 <-as.factor(ontime$DEP_DEL15)
ontime$DEST <- as.factor(ontime$DEST)

# PART 2: TRAINING DATA

library(caret)
set.seed(13) 

# Select columns to be used in algorithm training

feature<- c("ARR_DEL15", "DAY_OF_WEEK", "CARRIER", "DEST","ORIGIN","DEP_TIME_BLK")

# Created sorted version of the ontime data

ontime_sorted <- ontime[,feature] 

# Select data to put into training

training_index <- createDataPartition(ontime_sorted$ARR_DEL15, p=0.75, list=FALSE)

# Create training & testing dataset

training_data <- ontime_sorted[training_index,] 
testing_data <- ontime_sorted[training_index,] 

# METHOD 1: Logistic Regression

log_reg_mod <- train(ARR_DEL15 ~ ., data = training_data, method = "glm", family = "binomial",
                          trControl=trainControl(method = "cv", number = 5, repeats = 5))

# Predict

log_reg_predict <- predict(log_reg_mod, testing_data)

# Confusion matrix 

confusion_matrix_reg <- confusionMatrix(log_reg_predict, testing_data[,"ARR_DEL15"])
confusion_matrix_reg

# METHOD 2: Random Forest Method

library(randomForest) 

random_forest <- randomForest(training_data[-1], training_data$ARR_DEL15, proximity = TRUE, importance = TRUE)
random_forest
random_forest_validation <- predict(random_forest, testing_data)

# Confusion matrix 

confusion_matrix_rf <- confusionMatrix(random_forest_validatio, testing_data[,"ARR_DEL15"])
confusion_matrix_rf