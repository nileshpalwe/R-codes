#SVM

#library
library(caret)
library(e1071)

#read dataset
heart_dataframe <- read.csv(url("https://dataaspirant.com/wp-content/uploads/2017/01/heart_tidy.csv"))
heart_dataframe$X0.2 <- as.factor(heart_dataframe$X0.2)

#train test split
split<-createDataPartition(y=heart_dataframe$X0.2, p=0.6, list=FALSE)
training_data<-heart_dataframe[split,]
testing_data<-heart_dataframe[-split,]

#SVM model
train_control<-trainControl(method="repeatedcv",number=10,repeats=3)
svm_train<-train(X0.2 ~.,data=training_data,method="svmLinear", trControl=train_control,preProcess=c("center","scale"),tuneLength=10)

testing_predictions<-predict(svm_train, newdata=testing_data)

#confusion matrix
cm <-confusionMatrix(testing_predictions,testing_data$X0.2)

total <- cm$table[1,1] + cm$table[2,1] + cm$table[1,2] + cm$table[2,2]
writeLines(toString(total),"output.txt")
