#UBER TAXI FARE PREDICTION

#libraries
library(readr)
library(ggplot2)
library(geosphere)
library(caTools)
library(ggmap)
library(mapview)
#import training and testing data
data =read_csv("uber.csv")

#summary of data
summary(data)
str(data)

#remove N/A
data=na.omit(data)

#remove wrong latitude and logitude
data = data[!(data$pickup_longitude < (-180)  | data$dropoff_longitude < (-180) | data$pickup_latitude > 90 | data$dropoff_latitude > 90 |
                data$pickup_longitude > 180 | data$dropoff_longitude > 180 | data$pickup_latitude < (-90) | data$dropoff_latitude < (-90) |
                data$pickup_longitude == 0 | data$dropoff_longitude == 0 | data$pickup_latitude == 0 | data$dropoff_latitude == 0),]

#remove key from data
data = subset(data, select =-2)
str(data)

measure_distance= function(long1, lat1, long2, lat2){
  temp=distm(cbind(long1,lat1), cbind(long2,lat2),fun = distHaversine)
  return(temp/1609.344)
}
#add distance column in dataset
data$distance = mapply(measure_distance,data$pickup_longitude,data$pickup_latitude,data$dropoff_longitude,data$dropoff_latitude)

#Data Visualization
#1 scatter plot of distance ~ fare amount
ggplot(data, aes(x=distance, y=fare_amount)) + geom_point(color='blue', alpha=0.3)

#remove data where distance >50
data=data[!(data$distance > 50),]
#remove data where fare amount > 0 and fair amount<500
data= data[!(data$fare_amount>300),]
data=data[(data$fare_amount>0),]
data=data[(data$distance>0),]
dim(data)
View(data)

#2 boxplot of passenger count ~ fare amount
ggplot(data = data, aes(x=as.factor(passenger_count), y=fare_amount))+geom_boxplot()

#removing row with passenger count=208 and 0
data=data[data$passenger_count<7,]
data=data[data$passenger_count>0,]

#3 scatter plot of distance ~ fare amount
ggplot(data, aes(x=distance, y=fare_amount, color=as.factor(passenger_count))) + geom_point( alpha=0.3)

#make a time column in table
data$hour= substr(data$pickup_datetime,12,13)

# 4.bar plot of time ~ fare amount
ggplot(data, aes(x=hour)) +geom_bar()
#lowest count at 5 am
#maximum rides in evening

#make a month column
data$month= substr(data$pickup_datetime,6,7)

# 5. bar plot of month
ggplot(data, aes(x=month)) +geom_bar()

#make a year column
data$year= substr(data$pickup_datetime,1,4)

# 6. bar plot of month
ggplot(data, aes(x=year)) +geom_bar()

#divide data into training and validation set
#training data will be 70 percent 
#validation data will be 30 percent
set.seed(101)
sample <- sample.split(data$fare_amount, SplitRatio = 0.7)
train_data  <- subset(data, split=T)
val_data   <- subset(data, split=F)



#LINEAR REGRESSION
model1 = lm(train_data$fare_amount~train_data$distance, data=train_data)
summary(model1)
plot(train_data$fare_amount~train_data$distance, pch=10)
abline(model1, col='red')
newdata=data.frame(distance=val_data$distance)
pred1 = predict(model1,newdata)
#evaluate the model
library(forecast)
accuracy1 = accuracy(pred1,val_data$fare_amount)
accuracy1

#MULTIPLE LINEAR REGRESSION with time
model2= lm(train_data$fare_amount~train_data$distance+data$hour)
pred2=predict(model2,newdata)
#evaluate the model
accuracy2= accuracy(pred2,val_data$fare_amount)
accuracy2
#adding time variable doesn't change the R square 
#significantly, so it is not useful

#MULTIPLE LINEAR REGRESSION with passenger count
model3= lm(train_data$fare_amount~train_data$distance+data$passenger_count)
pred3=predict(model3,newdata)
#evaluate the model
accuracy3= accuracy(pred2,val_data$fare_amount)
accuracy3
#Again R square did decrease significantly 
#thus, no need to add it
