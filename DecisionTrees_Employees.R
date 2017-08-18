#Reading the CSV file
Data <- read.csv("Salary_Data.csv")

#Analyzing the Data.. Checking class of all variables & looking for missing values
str(Data)
summary (Data)

#Removing the ID
Data_ID<- Data[,1]
Data_1 <- Data[,2:16]

set.seed(415)

#Missing Value treatent
install.packages("missForest")
library(missForest)
Data_2<-missForest(Data_1)
summary(Data_1)

#Creating the final dataset
Data_f <- cbind(Data_ID, Data_2$ximp)

#Splitting the Data into Test and train
Train <- Data_f[1:35000,]
Test<- Data_f[35001:48842,]


#Building the model using rpart library
library(rpart)
model_1 <- rpart(Salary ~ age + workclass + fnlwgt + education_num + 
               marital_status + occupation + relationship + race + sex + capital_gain
              + capital_loss + hours_per_week + native_country ,
              data=Train, method="class", minsplit = 100,
              parms = list(prior = c(.3,.7), split = "information") )

#Ploting the rules
plot(model_1)
text(model_1)

#installing required packages
install.packages('rpart.plot')
install.packages('RColorBrewer')
library(rpart.plot)
library(RColorBrewer)

#Visualising the Decision Tree
rpart.plot(model_1)

#Analyzing the model
printcp(model_1)
plotcp(model_1)
summary(model_1)

#Pruning the tree
Prune_model<- prune(model_1, cp=  model_1$cptable[which.min(model_1$cptable[,"xerror"]),"CP"])

#Predicting the output using the pruned tree
Prediction <- predict(Prune_model, Test, type = "class")
submit <- data.frame(Test, Salary_new = Prediction)


#Checking for accuracy
table(Prediction)
table(Prediction, Test$Salary) # Confusion Matrix

#Generating the output file
write.csv(submit, file = "Output_f.csv", row.names = FALSE)
##End##