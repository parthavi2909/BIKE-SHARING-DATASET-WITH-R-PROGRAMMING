library(tidyverse)
bike_df<-read.csv("C:/Users/Asus Laptop/OneDrive/Desktop/R assignment/day.csv")
head(bike_df,5)
bike_df<-subset(bike_df,select=-c(casual,registered))
head(bike_df,5)
#Dimension of dataset
dim(bike_df)
#Summary of the dataset
summary(bike_df)
#Structure of dataset
str(bike_df)
#Rename the columns
names(bike_df)<-c('rec_id','datetime','season','year','month','holiday','weekday','workingday','weather_condition','temp','atemp','humidity','windspeed','total_count')
#Read the data
head(bike_df,5)
#Typecasting the datetime and numerical attributes to category

bike_df$datetime<- as.Date(bike_df$datetime)
bike_df$year<-as.factor(bike_df$year)
bike_df$month<-as.factor(bike_df$month)
bike_df$season <- as.factor(bike_df$season)
bike_df$holiday<- as.factor(bike_df$holiday)
bike_df$weekday<- as.factor(bike_df$weekday)
bike_df$workingday<- as.factor(bike_df$workingday)
bike_df$weather_condition<- as.factor(bike_df$weather_condition)
#Missing values in dataset
missing_val<-data.frame(apply(bike_df,2,function(x){sum(is.na(x))}))
names(missing_val)[1]='missing_val'
missing_val
library(ggplot2)
#column plot for season wise monthly distribution of counts
ggplot(bike_df,aes(x=month,y=total_count,fill=season))+theme_bw()+geom_col()+
  labs(x='Month',y='Total_Count',title='Season wise monthly distribution of counts')+
  scale_fill_manual(values=c("red","green","purple","brown"))
#column plot for weekday wise monthly distribution of counts
ggplot(bike_df,aes(x=month,y=total_count,fill=weekday))+theme_bw()+geom_col()+
  labs(x='Month',y='Total_Count',title='Weekday wise monthly distribution of counts')+
  scale_fill_manual(values=c("dark green","sky blue","purple","red","magenta","yellow","blue"))
#Violin plot for Yearly wise distribution of counts
ggplot(bike_df,aes(x=year,y=total_count,fill=year))+geom_violin()+theme_bw()+
  labs(x='Year',y='Total_Count',title='Yearly wise distribution of counts')+
  scale_fill_manual(values=c("magenta","purple"))
#Column plot for holiday wise distribution of counts
ggplot(bike_df,aes(x=holiday,y=total_count,fill=season))+geom_col()+theme_bw()+
  labs(x='holiday',y='Total_Count',title='Holiday wise distribution of counts')+
  scale_fill_manual(values=c("light green","yellow","purple","red"))
#Column plot for workingday wise distribution of counts
ggplot(bike_df,aes(x=workingday,y=total_count,fill=season))+geom_col()+theme_bw()+
  labs(x='workingday',y='Total_Count',title='Workingday wise distribution of counts')
#Column plot for weather_condition distribution of counts
ggplot(bike_df,aes(x=weather_condition,y=total_count,fill=season))+geom_col()+theme_bw()+
  labs(x='Weather_condition',y='total_count',title='Weather_condition distribution of counts')+
  scale_fill_manual(values=c("purple","light green","light blue","red"))

#box plots for outliers
par(mfrow=c(2,2))
#Box plot for temp outliers
boxplot(bike_df$temp, main="Temp",sub=paste(boxplot.stats(bike_df$temp)$out))
#Box plot for humidity outliers
boxplot(bike_df$humidity,main="Humidity",sub=paste(boxplot.stats(bike_df$humidity)$out))
#Box plot for windspeed outliers
boxplot(bike_df$windspeed,main="Windspeed",sub=paste(boxplot.stats(bike_df$windspeed)$out))
#load the DMwR library
library(DMwR)
#create subset for windspeed and humidity variable
wind_hum<-subset(bike_df,select=c('windspeed','humidity'))
#column names of wind_hum
cnames<-colnames(wind_hum)
for(i in cnames){
  val=wind_hum[,i][wind_hum[,i] %in% boxplot.stats(wind_hum[,i])$out] #outlier values
  wind_hum[,i][wind_hum[,i] %in% val]= NA  # Replace outliers with NA 
}
#Imputating the missing values using mean imputation method
wind_hum$windspeed[is.na(wind_hum$windspeed)]<-mean(wind_hum$windspeed,na.rm=T) 
wind_hum$humidity[is.na(wind_hum$humidity)]<-mean(wind_hum$humidity,na.rm=T)
#Remove the windspeed and humidity variable in order to replace imputated data
new_df<-subset(bike_df,select=-c(windspeed,humidity))
#Combined new_df and wind_hum data frames
bike_df<-cbind(new_df,wind_hum)
head(bike_df,5)
#Quintle-Quintle normal plot
qqnorm(bike_df$total_count)
#Quintle-Quintle line
qqline(bike_df$total_count)
#load the corrgram for correlation
library(corrgram)
#Correlation plot
corrgram(bike_df[,10:14],order=F,upper.panel=panel.pie,text.panel=panel.txt,main='Correlation Plot')
#load the purrr library for functions and vectors
library(purrr)
#Split the dataset based on simple random resampling
train_index<-sample(1:nrow(bike_df),0.7*nrow(bike_df))
train_data<-bike_df[train_index,]
test_data<-bike_df[-train_index,]
dim(train_data)
dim(test_data)
#Read the train and test data
head(train_data,5)
head(test_data,5)
#Create a new subset for train attributes 
train<-subset(train_data,select=c('season','year','month','holiday', 'weekday','workingday','weather_condition','temp','humidity','windspeed','total_count'))
#Create a new subset for test attributes
test<-subset(test_data,select=c('season','year','month','holiday','weekday','workingday','weather_condition','temp','humidity','windspeed','total_count'))
head(train,5)
head(test,5)
#create a new subset for train categorical attributes
train_cat_attributes<-subset(train,select=c('season','holiday','workingday','weather_condition','year'))
#create a new subset for test categorical attributes
test_cat_attributes<-subset(test,select=c('season','holiday','workingday','weather_condition','year'))
#create a new subset for train numerical attributes
train_num_attributes<-subset(train,select=c('weekday','month','temp','humidity','windspeed','total_count'))
#create a new subset for test numerical attributes
test_num_attributes<-subset(test,select=c('weekday','month','temp', 'humidity','windspeed','total_count'))
#load the caret library
library(caret)
#other variables along with target variable to get dummy variables
othervars<-c('month','weekday','temp','humidity','windspeed','total_count')
set.seed(2626)
#Categorical variables
vars<-setdiff(colnames(train),c(train$total_count,othervars))
#formula pass through encoder to get dummy variables
f <- paste('~', paste(vars, collapse = ' + '))
#encoder is encoded the categorical variables to numeric
encoder<-dummyVars(as.formula(f), train)
#Predicting the encode attributes
encode_attributes<-predict(encoder,train)
#Binding the train_num_attributes and encode_attributes
train_encoded_attributes<-cbind(train_num_attributes,encode_attributes)
head(train_encoded_attributes,5)
set.seed(5662)
#Categorical variables
vars<-setdiff(colnames(test),c(test$total_count,othervars))
#formula pass through encoder to get dummy variables
f<- paste('~',paste(vars,collapse='+'))
#Encoder is encoded the categorical variables to numeric
encoder<-dummyVars(as.formula(f),test)
#Predicting the encoder attributes
encode_attributes<-predict(encoder,test)
#Binding the test_num_attributes and encode_attributes
test_encoded_attributes<-cbind(test_num_attributes,encode_attributes)
head(test_encoded_attributes,5)
#Set seed to reproduce the results of random sampling
set.seed(672)
#training the lr_model
lr_model<-lm(train_encoded_attributes$total_count~.,train_encoded_attributes[,-c(6)])
#Summary of the model
summary(lr_model)
#Cross validation prediction
#To ignore warning messages
options(warn=-1)
#Set seed to reproduce results of random sampling
set.seed(623)
#Cross validation resampling method
train.control<-trainControl(method='CV',number=3)
#Cross validation prediction
CV_predict<-train(total_count~.,data=train_encoded_attributes,method='lm',trControl=train.control)
#Summary of cross validation prediction
summary(CV_predict)
#Cross validation prediction plot
residuals<-resid(CV_predict)
y_train<-train_encoded_attributes$total_count
plot(y_train,residuals,ylab=('Residuals'),xlab=('Observed'),main=('Cross validation prediction plot'))
abline(0,0)
set.seed(6872)
options(warn=-1)
#predict the lr_model
lm_predict<- predict(lr_model,test_encoded_attributes[,-c(6)])
head(lm_predict,5)
set.seed(688)
#Root mean squared error
rmse<-RMSE(lm_predict, test_encoded_attributes$total_count)
print(rmse)
#Mean squared error
mae<-MAE(lm_predict, test_encoded_attributes$total_count)
print(mae)
#Residual plot
y_test<-test_encoded_attributes$total_count
residuals<-y_test-lm_predict
plot(y_test,residuals,xlab='Observed',ylab='Residuals',main='Residual plot')
abline(0,0)
set.seed(568)
#load the rpart library for decision trees
library(rpart)
#rpart.control to contro the performance of model
rpart.control<-rpart.control(minbucket = 2,cp = 0.01,maxcompete = 3, maxsurrogate = 4, usesurrogate = 2, xval = 3,surrogatestyle = 0, maxdepth = 10) 
#training the dtr model
dtr<-rpart(train_encoded_attributes$total_count~.,data=train_encoded_attributes[,-c(6)],control=rpart.control,method='anova',cp=0.01)
#Summary of dtr model
dtr
#load the rpart.plot for plot the learned dtr model
library(rpart.plot)
rpart.plot(dtr, box.palette="RdBu", shadow.col="green", nn=TRUE,roundint=FALSE)
#Cross validation prediction
options(warn=-1)
set.seed(5769)
#cross validation resampling method
train.control<-trainControl(method='CV',number=3)
#cross validation pred
dtr_CV_predict<-train(total_count~.,data=train_encoded_attributes,method='rpart',trControl=train.control)
dtr_CV_predict
#Cross validation prediction plot
residuals<-resid(dtr_CV_predict)
plot(y_train,residuals,xlab='Observed',ylab='Residuals',main='Cross validation plot')
abline(0,0)
set.seed(7882)
#predict the trained model
dtr_predict<-predict(dtr,test_encoded_attributes[,-c(6)])
head(dtr_predict,5)
set.seed(6889)
#Root mean squared error
rmse<-RMSE(y_test,dtr_predict)
print(rmse)
#Mean absolute error
mae<-MAE(y_test,dtr_predict)
print(mae)

set.seed(6788271)
#load the randomForest library
library(randomForest)
#training the model
rf_model<-randomForest(total_count~.,train_encoded_attributes,importance=TRUE,ntree=200)
rf_model
options(warn=-1)
set.seed(6772)
library(randomForest)
#load the ranger library for random forest CV
library(ranger)
#Cross validation resampling method
train.control<-trainControl(method='CV',number=3)
#Cross validation prediction
rf_CV_predict<-train(total_count~.,train_encoded_attributes,method='ranger',trControl=train.control)
rf_CV_predict
#Cross validation prediction plot
residuals<-resid(rf_CV_predict)
plot(y_train,residuals,xlab='Observed',ylab='Residuals',main='Cross validation prediction plot')
abline(0,0)
set.seed(7889)
#Predicting the model
rf_predict<-predict(rf_model,test_encoded_attributes[,-c(6)])
head(rf_predict,5)
set.seed(667)
#Root mean squared error
rmse<-RMSE(y_test,rf_predict)
print(rmse)
mae<-MAE(y_test,rf_predict)
print(mae)
#Residual plot
residuals<-y_test-rf_predict
plot(y_test,residuals,xlab='Observed',ylab='Residuals',main='Residual plot')
abline(0,0)
Bike_predictions=data.frame(y_test,rf_predict)
