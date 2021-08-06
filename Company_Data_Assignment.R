#Installing the required Packages
install.packages("randomForest")
install.packages("fastDummies")
install.packages("caret")
library(caret)
library(fastDummies)
library(randomForest)

#Loading the data file
file <- read.csv(file.choose())
View(file)

#Data Manipulation
data <- file
data['Urban'] <- ifelse(data['Urban']=='Yes',1,0)
data$US <- ifelse(data$US=='Yes',1,0)
data$Sales <- as.factor(cut(data$Sales,breaks = c(0,4,8,12,16),labels = c("Bad","Medium","Good","Best")))
data <- fastDummies::dummy_cols(data,select_columns = "ShelveLoc")

#Normalizing the dataset
norm <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

norm_data <- as.data.frame(lapply(data[c(-1,-7)],norm))
norm_data["Sales"] <- data$Sales

#Splitting dataframe into training and testing dataset
ind <- sample(2,nrow(norm_data),replace=TRUE,prob = c(0.8,0.2))
train <- norm_data[ind==1,]
test <- norm_data[ind==2,]


#Making the Model
model <- randomForest(Sales~.,data = train,na.action = na.roughfix,importance=TRUE,ntree=500)

#Predicting Training Values
pred_train <- predict(model,train)
#Checking the training accuracy
mean(train$Sales == predict(model,train))
confusionMatrix(train$Sales,pred_train)

#Predicting Testing Values
pred_test <- predict(model,test)
#Checking the testing Accuracy
mean(test$Sales==pred_test)
confusionMatrix(test$Sales,pred_test)


#Visualizing the Feature Importance
varImpPlot(model)
#Visualising the model
plot(model,lwd=2)
legend("topright",colnames(model$err.rate),
       col = 1:4,fill = 1:8,cex=0.8)
