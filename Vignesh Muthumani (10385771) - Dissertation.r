# VIGNESH MUTHUMANI (10385771) - Dissertation code
# A data mining approach on the analysis of road safety in GB

# ---------------------------------------------------------------------------------------------------------------------------

# Section 1: Merging the datasets

# loading all the individual datasets from the source
acc <- read.csv(file.choose())
cas <- read.csv(file.choose())
veh <- read.csv(file.choose())

# viewing the datasets
View(acc)
View(cas)
View(veh)

# merging the first two datasets
datam <- merge(acc, cas)

# merging the merged dataset with the third dataset
dataset <- merge(datam, veh)

# viewing the final version of the merged dataset
View(dataset)

# exporting the merged dataset
write.csv(dataset, file = "dataset.csv")

# ---------------------------------------------------------------------------------------------------------------------------------------------

# Section 2: Data preparation

# loading the mergeddata
data1 <- read.csv(file.choose())
View(data1)
str(data1)

# checking the dimensions of the data
dim(data1)

# checking for missing values
sum(is.na(data1))

# removing the missing values
data2 <- na.omit(data1)

# checking if the missing values have been removed
sum(is.na(data2))

# removing the unwanted variables
data3 <- data2[,c(8:13,18:19,26:28,37,39,48,50,59:61,65)]
View(data3)

# replacing the "-1" instances with NA
data4 <- data.frame(lapply(data3, function(x) {
  gsub(-1, NA, x)
}))
View(data4)

# checking the no. of observations that has missing values
table(is.na(data4))

# removing missing values as it accounts for only 1% of data
data <- na.omit(data4)

# checking the dimensions of the refined data
table(is.na(data))
dim(data)
str(data)
View(data)

# changing the class of certain variables
data[,2] <- as.integer(data$Number_of_Vehicles)
data[,3] <- as.integer(data$Number_of_Casualties)
data[,12] <- as.integer(data$Age_of_Casualty)
data[,18] <- as.integer(data$Age_of_Driver)
data[,19] <- as.integer(data$Age_of_Vehicle)
str(data)

# renaming the factor variables for better understanding
library(plyr)
data$Accident_Severity <- revalue(data$Accident_Severity, c('1'='Fatal','2'='Serious','3'='Slight'))
data$Day_of_Week <- revalue(data$Day_of_Week, c('1'='Sunday','2'='Monday','3'='Tuesday',
                                                '4'='Wednesday','5'='Thursday','6'='Friday','7'='Saturday'))
data$Casualty_Severity <- revalue(data$Casualty_Severity, c('1'='Fatal','2'='Serious','3'='Slight'))
data$Sex_of_Driver <- revalue(data$Sex_of_Driver, c('1'='Male','2'='Female','3'='Unknown'))
str(data)

# binning the time variable into 4 parts
time <- as.character(data$Time)
data[,6] <- as.numeric(time)
data$Time <- cut(data$Time, c(0,5,12,17,20,24), 
                 labels=c('Night1','Morning','Afternoon','Evening','Night2'))

# grouping the night1 and 2 into one level
levels(data$Time) <- c('Night','Morning','Afternoon','Evening','Night')
levels(data$Time)

str(data)

# exploring the distribution of target variable 
levels(data4$Accident_Severity) <- c("Fatal", "Severe", "Slight")
levels(data4$Accident_Severity)

par(mfrow=c(1,1))
plot(data4$Accident_Severity, xlab = 'Accident Severity', 
     col = c('red','yellow','green'), ylim = c(0,180000),
     main = 'Distribution of the Accident Severity')
table(data4$Accident_Severity)

# target variable is imbalanced. grouping the levels into high and low
levels(data$Accident_Severity)
levels(data$Accident_Severity) <- c("High", "High", "Low")
levels(data$Accident_Severity)

levels(data$Light_Conditions)
levels(data$Light_Conditions) <- c("1", "1", "2",'2','3')
levels(data$Light_Conditions)

levels(data$Weather_Conditions)
levels(data$Weather_Conditions) <- c("1", "2", "3",'4','5','6','7','8','8')
levels(data$Weather_Conditions)

levels(data$Vehicle_Type)
levels(data$Vehicle_Type) <- c('1','2','2','2','2','3','3','4','4','1','3','5','4','4','4','2','2','6','2','4')
levels(data$Vehicle_Type)

levels(data$Vehicle_Manoeuvre)
levels(data$Vehicle_Manoeuvre) <- c('1','2','2','2','2','3','3','3','3','3','3','3','4','4','4','4','4','4')
levels(data$Vehicle_Manoeuvre)

levels(data$Journey_Purpose_of_Driver)
levels(data$Journey_Purpose_of_Driver) <- c('1','1','2','2','3','3','3')
levels(data$Journey_Purpose_of_Driver)

##############################################

# handling the date variable. (with date variable)
data$Date <- as.Date(data$Date, "%d-%m-%Y")
str(data)

#############################################

# renaming the other factor variables

data$Road_Type <- revalue(data$Road_Type, c('1'='Roundabout','2'='One way street',
                                            '3'='Dual carriageway','6'='Single carriageway','7'='Slip road','9'='Unknown'))

data$Light_Conditions <- revalue(data$Light_Conditions, c('1'='Lit','2'='Dark','3'='unknown'))

data$Weather_Conditions <- revalue(data$Weather_Conditions, c('1'='Fine - no high winds','2'='Raining - no high winds',
                                                              '3'='Snowing - no high winds','4'='Fine - high winds','5'='Raining - high winds','6'='Snowing - high winds',
                                                              '7'='Fog or mist','8'='Other'))

data$Road_Surface_Conditions <- revalue(data$Road_Surface_Conditions, c('1'='Dry','2'='Wet or damp',
                                                                        '3'='Snow','4'='Frost or ice','5'='Flood over 3cm. deep','6'='Oil or diesel',
                                                                        '7'='Mud'))

data$Journey_Purpose_of_Driver <- revalue(data$Journey_Purpose_of_Driver, c('1'='For work','2'='For school',
                                                                            '3'='Other'))

data$Vehicle_Type <- revalue(data$Vehicle_Type, c('1'='Cycle','2'='Motorcycle','3'='Car','4'='Bus/coach','5'='Tram','6'='Other'))

data$Vehicle_Manoeuvre <- revalue(data$Vehicle_Manoeuvre, c('1'='Reversing','2'='Parked','3'='Taking turn','4'='Overtaking'))

#############################################

# removing the date variable for data modeling
data <- data[,-4]
str(data)

###############################################

# still there's an imbalance, hence we do random over sampling

install.packages('ROSE')
library(ROSE) # installing the required package

datas <- ROSE(Accident_Severity ~ ., data = data, seed=1)$data

View(datas)
table(datas$Accident_Severity)
plot(datas$Accident_Severity)
str(datas)

# exporting the data
write.csv(datas, file = "datas.csv")

###############################################

# comparison of distribution

par(mfrow = c(1,2))

plot(data$Accident_Severity, xlab = 'Accident Severity', 
     col = c('red','green'), ylim = c(0,90000),
     main = 'Imbalanced target variable')

plot(datas$Accident_Severity, xlab = 'Accident Severity', 
     col = c('dark green','dark red'), ylim = c(0,90000),
     main = 'Balanced target variable (ROSE)')

###############################################

# ---------------------------------------------------------------------------------------------------------------------------------------------

# Section 3: Data modelling

# Data modelling

# checking the correlation between target variable and cas. severity
dv <- data

levels(dv$Casualty_Severity)
levels(dv$Casualty_Severity) <- c('High','High','Low')
levels(dv$Casualty_Severity)

t <- table(Acc_Severity = dv$Accident_Severity, Cas_Severity = dv$Casualty_Severity)

plot(dv$Accident_Severity)
plot(dv$Casualty_Severity)

mean(dv$Accident_Severity == dv$Casualty_Severity)

prop.table(t)

dt <- datas[,-12] # removal of casualty severity because of strong correlation

######################################

set.seed(170283)	# for the consistency
str(dt)

#splitting the dataset in 70:30 ratio
nrows <- nrow(dt)
index <- sample(1:nrows, 0.7 * nrows)	# shuffle and divide
train <- dt[index,]			        
test <- dt[-index,]  		        

# checking the dimensions after the split
dim(data)  
dim(train)
dim(test)

prop.table(table(train$Accident_Severity)) # checking for the proportion
prop.table(table(test$Accident_Severity))

# (1) Naive Bayes: 

library(e1071)
library(caret)

# fitting the model
naiveb <- naiveBayes(train[,-1], train$Accident_Severity)

# predicting test set with the trained model
predicted_nb <- predict(naiveb, test[,-1],type = 'class')

# confusion matrix to see the performance
con_matrix_nb <- confusionMatrix(predicted_nb, test$Accident_Severity)		
con_matrix_nb

# (2) SVM:

# fitting the model
svm <- svm(Accident_Severity~., data=train)

# predicting test set with the trained model
predicted_svm <- predict(svm, test[,-1])

# confusion matrix to see the performance
con_matrix_svm <- confusionMatrix(predicted_svm, test$Accident_Severity)
con_matrix_svm

# (3) Random Forest:

library(randomForest)
str(data)
str(train)
str(test)

# fitting the model
ran_forest <- randomForest(Accident_Severity~., data=train, ntree=500)

# identifying the importance of variables based on Mean Decrease
imp <- importance(ran_forest)

# checking the importance of variables
par(mfrow=c(1,1))
varImpPlot(ran_forest,pch=18,col='red',cex=1)

# performance of random forest model
plot(ran_forest, main="Random Forest: MSE error vs. no of trees")

# predicting test set with the trained model
predicted_rf   <- predict(ran_forest, test[,-1])

# confusion matrix to see the performance
con_matrix_rf <- confusionMatrix(predicted_rf, test$Accident_Severity)
con_matrix_rf

# (4) LGR:

lgr <- glm(Accident_Severity ~.,train, family="binomial")
summary(lgr)

res <- predict(lgr, test, type="response")  # prediction

dim(test)
predictedvalues = rep(0,27714)
predictedvalues[res>0.5] = 1

con_matrix_lgr <- table(predictedvalues,actualvalues=test[,1]) 

testlgr <- test
testlgr$Accident_Severity <- revalue(testlgr$Accident_Severity, 
                                   c('High'='1','Low'='0'))
mean(predictedvalues == testlgr[,1]) # correctness of prediction

# (5) decision tree:

library(rpart)

detree <- rpart(Accident_Severity~.,train,method='class')
plot(detree)
text(detree,pretty = 0)
summary(detree)

predicted_dt <- predict(detree,test,type = 'class')
con_matrix_dt <- confusionMatrix(predicted_dt,test$Accident_Severity) 
con_matrix_dt

cfdt <- table(predicted_dt,test$Accident_Severity)
cfdt

#################################################################

# ---------------------------------------------------------------------------------------------------------------------------------------------

# Section 4: Chaining algorithms by passing the significant variables identified by random forest

# (4) Chaining:

library(randomForest)

# identifying the importance of variables based on Mean Decrease
imp <- importance(ran_forest)

# checking the importance of variables
varImpPlot(ran_forest,pch=18,col='red',cex=1)

################################################################
# (3.1) Rf model with only significant variables

str(train) # checking the variables to be removed based on Importance
str(test)

train1 <- train[,c(-8,-10,-14,-15)] # removing the insignificant variables
test1 <- test[,c(-8,-10,-14,-15)]

str(train1) # checking if the variables have been removed correctly
str(test1)

# building the rf model with the new trainset
ran_forest_1 <- randomForest(Accident_Severity~., data=train1, ntree=500)

# performance of the new random forest model
plot(ran_forest_1, main="Random Forest 1: MSE error vs. no of trees")

# predicting the new test set with the new trained model
predicted_rf_1 <- predict(ran_forest_1, test1[,-1])

# confusion matrix to see the performance
con_matrix_rf_1 <- confusionMatrix(predicted_rf_1, test1$Accident_Severity)
con_matrix_rf_1

################################################################

# (1) fitting the model
nb1 <- naiveBayes(train1[,-1], train1$Accident_Severity)

# predicting test set with the trained model
predicted_nb_1 <- predict(nb1, test1[,-1])

# confusion matrix to see the performance
con_matrix_nb_1 <- confusionMatrix(predicted_nb_1, test1$Accident_Severity)		
con_matrix_nb_1

###############################################################

# (2) decision tree:
library(rpart)
detree1 <- rpart(Accident_Severity~.,train1,method='class')

predicted_dt_1 <- predict(detree1,test1,type = 'class')
con_matrix_dt_1 <- confusionMatrix(predicted_dt_1,test1$Accident_Severity)
con_matrix_dt_1

#####################################################################

# (3) LGR

lgr1 <- glm(Accident_Severity ~.,train1, family="binomial")
summary(lgr1)

res1 <- predict(lgr1, test1, type="response")  # prediction

dim(test1)
predictedvalues1 = rep(0,27714)
predictedvalues1[res1>0.5] = 1

con_matrix_lgr_1 <- table(predictedvalues1,actualvalues=test1[,1]) 

testlgr1 <- test1
testlgr1$Accident_Severity <- revalue(testlgr1$Accident_Severity, 
                                   c('High'='1','Low'='0'))
mean(predictedvalues1 == testlgr1[,1]) # correctness of prediction

#####################################################################

# ---------------------------------------------------------------------------------------------------------------------------------------

# Section 5: Data evaluation:

# comparison of all the models based on accuracy

col <- c("#ed3b3b", "#0099ff")
par(mfrow=c(2,3))

fourfoldplot(con_matrix_nb$table, color = col, conf.level = 0, 
             margin = 1, main=paste("NaiveBayes (",round(con_matrix_nb$overall[1]*100),"%)",sep=""))

fourfoldplot(con_matrix_svm$table, color = col, conf.level = 0, 
             margin = 1, main=paste("SVM (",round(con_matrix_svm$overall[1]*100),"%)",sep=""))

fourfoldplot(con_matrix_rf$table, color = col, conf.level = 0, 
             margin = 1, main=paste("RandomForest (",round(con_matrix_rf$overall[1]*100),"%)",sep=""))

fourfoldplot(con_matrix_lgr, color = col, conf.level = 0, 
             margin = 1, main=paste("LGR (",round(mean(predictedvalues == test1[,1])*100),"%)",sep=""))

fourfoldplot(cfdt, color = col, conf.level = 0, 
             margin = 1, main=paste("Decision Tree (",round(con_matrix_dt$overall[1]*100),"%)",sep=""))

######################################################################

# comparison of all the models based on roc curve

install.packages('pROC')
library(pROC) # package required for plotting the roc curve

par(mfrow=c(2,2))

# (1) NB:

predicted_nb1 <- predict(naiveb, test, type = 'raw')

auc1 <- auc(test$Accident_Severity,predicted_nb1[,2])
plot(roc(test$Accident_Severity,predicted_nb1[,2]),
     main=paste('Naive Bayes (AUC =', round(auc1, digits = 3),')'), sep = '')

# (2) RF:

predicted_rf1 <- predict(ran_forest, test, type = 'prob')

auc3 <- auc(test$Accident_Severity,predicted_rf1[,2])
plot(roc(test$Accident_Severity,predicted_rf1[,2]),
     main=paste('Random Forest (AUC =', round(auc3, digits = 3),')'), sep = '')

# (3) LGR:

predicted_lgr1 <- predict(lgr, test, type = 'response')

auc4 <- auc(test$Accident_Severity,predicted_lgr1)
plot(roc(test$Accident_Severity,predicted_lgr1),
     main=paste('LGR (AUC =', round(auc4, digits = 3),')'), sep = '')

# (4) DT:

predicted_dt1 <- predict(detree,test,type = 'prob')

auc5 <- auc(test$Accident_Severity,predicted_dt1[,2])
plot(roc(test$Accident_Severity,predicted_dt1[,2]),
     main=paste('Decision Tree (AUC =', round(auc5, digits = 3),')'), sep = '')

###################################################################

# -----------------------------------------------------------------------------------------------------------------------------------------

# Section 6: Comparison of different split ratios

# comparison of different split ratios

library(e1071)
library(caret)
library(randomForest)
library(rpart)

# 50% ---------------------------------------------------------------

dt50 <- dt
set.seed(170283)	# for the consistency
str(dt50)

#splitting the dataset
nrows50 <- nrow(dt50)
index50 <- sample(1:nrows50, 0.5 * nrows50)	# shuffle and divide
train50 <- dt50[index50,]			        
test50 <- dt50[-index50,]  		        

# checking the dimensions after the split
dim(dt50)  
dim(train50)
dim(test50)

prop.table(table(train50$Accident_Severity))
prop.table(table(test50$Accident_Severity))

# (1) Naive Bayes: 

# fitting the model
naiveb50 <- naiveBayes(train50[,-1], train50$Accident_Severity)

# predicting test set with the trained model
predicted_nb50 <- predict(naiveb50, test50[,-1])

# confusion matrix to see the performance
con_matrix_nb50 <- confusionMatrix(predicted_nb50, test50$Accident_Severity)		
con_matrix_nb50 # Acc: 0.6603

# (2) Random Forest:

# fitting the model
ran_forest50 <- randomForest(Accident_Severity~., data=train50, ntree=300)

# predicting test set with the trained model
predicted_rf50   <- predict(ran_forest50, test50[,-1])

# confusion matrix to see the performance
con_matrix_rf50 <- confusionMatrix(predicted_rf50, test50$Accident_Severity)
con_matrix_rf50 # Acc: 0.7419

# (3) LGR:

lgr50 <- glm(Accident_Severity ~.,train50, family="binomial")

res50 <- predict(lgr50, test50, type="response")  # prediction

dim(test50)
predictedvalues50 = rep(0,46189)
predictedvalues50[res50>0.5] = 1

con_matrix_lgr50 <- table(predictedvalues50,actualvalues=test50[,1]) 

tests50 <- test50
tests50$Accident_Severity <- revalue(tests50$Accident_Severity, 
                                     c('High'='1','Low'='0'))
mean(predictedvalues50 == tests50[,1]) # correctness of prediction
# Acc: 0.6564

# (4) DT:

detree50 <- rpart(Accident_Severity~.,train50,method='class')

predicted_dt50 <- predict(detree50,test50,type = 'class')
con_matrix_dt50 <- confusionMatrix(predicted_dt50,test50$Accident_Severity) 
con_matrix_dt50 # Acc: 0.6366

# 60% ---------------------------------------------------------------

dt60 <- dt
set.seed(170283)	# for the consistency
str(dt60)

#splitting the dataset
nrows60 <- nrow(dt60)
index60 <- sample(1:nrows60, 0.6 * nrows60)	# shuffle and divide
train60 <- dt60[index60,]			        
test60 <- dt60[-index60,]  		        

# checking the dimensions after the split
dim(dt60)  
dim(train60)
dim(test60)

prop.table(table(train60$Accident_Severity))
prop.table(table(test60$Accident_Severity))

# (1) Naive Bayes: 

# fitting the model
naiveb60 <- naiveBayes(train60[,-1], train60$Accident_Severity)

# predicting test set with the trained model
predicted_nb60 <- predict(naiveb60, test60[,-1])

# confusion matrix to see the performance
con_matrix_nb60 <- confusionMatrix(predicted_nb60, test60$Accident_Severity)		
con_matrix_nb60 # Acc: 0.6587

# (2) Random Forest:

# fitting the model
ran_forest60 <- randomForest(Accident_Severity~., data=train60, ntree=300)

# predicting test set with the trained model
predicted_rf60   <- predict(ran_forest60, test60[,-1])

# confusion matrix to see the performance
con_matrix_rf60 <- confusionMatrix(predicted_rf60, test60$Accident_Severity)
con_matrix_rf60 # Acc: 0.7503

# (3) LGR:

lgr60 <- glm(Accident_Severity ~.,train60, family="binomial")

res60 <- predict(lgr60, test60, type="response")  # prediction

dim(test60)
predictedvalues60 = rep(0,36951)
predictedvalues60[res60>0.5] = 1

con_matrix_lgr60 <- table(predictedvalues60,actualvalues=test60[,1]) 

tests60 <- test60
tests60$Accident_Severity <- revalue(tests60$Accident_Severity, 
                                     c('High'='1','Low'='0'))
mean(predictedvalues60 == tests60[,1]) # correctness of prediction
# Acc: 0.6546

# (4) DT:

detree60 <- rpart(Accident_Severity~.,train60,method='class')

predicted_dt60 <- predict(detree60,test60,type = 'class')
con_matrix_dt60 <- confusionMatrix(predicted_dt60,test60$Accident_Severity) 
con_matrix_dt60 # Acc: 0.6358

# 70% ---------------------------------------------------------------

# NB: 0.6596
# RF: 0.7608
# LGR: 0.6561
# DT: 0.6440

# results obtained from existing models

# 80% ---------------------------------------------------------------

dt80 <- dt
set.seed(170283)	# for the consistency
str(dt80)

#splitting the dataset
nrows80 <- nrow(dt80)
index80 <- sample(1:nrows80, 0.8 * nrows80)	# shuffle and divide
train80 <- dt80[index80,]			        
test80 <- dt80[-index80,]  		        

# checking the dimensions after the split
dim(dt80)  
dim(train80)
dim(test80)

prop.table(table(train80$Accident_Severity))
prop.table(table(test80$Accident_Severity))

# (1) Naive Bayes: 

# fitting the model
naiveb80 <- naiveBayes(train80[,-1], train80$Accident_Severity)

# predicting test set with the trained model
predicted_nb80 <- predict(naiveb80, test80[,-1])

# confusion matrix to see the performance
con_matrix_nb80 <- confusionMatrix(predicted_nb80, test80$Accident_Severity)		
con_matrix_nb80 # Acc: 0.6558

# (2) Random Forest:

# fitting the model
ran_forest80 <- randomForest(Accident_Severity~., data=train80, ntree=300)

# predicting test set with the trained model
predicted_rf80   <- predict(ran_forest80, test80[,-1])

# confusion matrix to see the performance
con_matrix_rf80 <- confusionMatrix(predicted_rf80, test80$Accident_Severity)
con_matrix_rf80 # Acc: 0.7621

# (3) LGR:

lgr80 <- glm(Accident_Severity ~.,train80, family="binomial")

res80 <- predict(lgr80, test80, type="response")  # prediction

dim(test80)
predictedvalues80 = rep(0,18476)
predictedvalues80[res80>0.5] = 1

con_matrix_lgr80 <- table(predictedvalues80,actualvalues=test80[,1]) 

tests80 <- test80
tests80$Accident_Severity <- revalue(tests80$Accident_Severity, 
                                     c('High'='1','Low'='0'))
mean(predictedvalues80 == tests80[,1]) # correctness of prediction
# Acc: 0.6542

# (4) DT:

detree80 <- rpart(Accident_Severity~., train80, method='class')

predicted_dt80 <- predict(detree80,test80,type = 'class')
con_matrix_dt80 <- confusionMatrix(predicted_dt80,test80$Accident_Severity) 
con_matrix_dt80 # Acc: 0.6428

# 90% ---------------------------------------------------------------

dt90 <- dt
set.seed(170283)	# for the consistency
str(dt90)

#splitting the dataset
nrows90 <- nrow(dt90)
index90 <- sample(1:nrows90, 0.9 * nrows90)	# shuffle and divide
train90 <- dt90[index90,]			        
test90 <- dt90[-index90,]  		        

# checking the dimensions after the split
dim(dt90)  
dim(train90)
dim(test90)

prop.table(table(train90$Accident_Severity))
prop.table(table(test90$Accident_Severity))

# (1) Naive Bayes: 

# fitting the model
naiveb90 <- naiveBayes(train90[,-1], train90$Accident_Severity)

# predicting test set with the trained model
predicted_nb90 <- predict(naiveb90, test90[,-1])

# confusion matrix to see the performance
con_matrix_nb90 <- confusionMatrix(predicted_nb90, test90$Accident_Severity)		
con_matrix_nb90 # Acc: 0.6525

# (2) Random Forest:

# fitting the model
ran_forest90 <- randomForest(Accident_Severity~., data=train90, ntree=300)

# predicting test set with the trained model
predicted_rf90   <- predict(ran_forest90, test90[,-1])

# confusion matrix to see the performance
con_matrix_rf90 <- confusionMatrix(predicted_rf90, test90$Accident_Severity)
con_matrix_rf90 # Acc: 0.7703

# (3) LGR:

lgr90 <- glm(Accident_Severity ~.,train90, family="binomial")

res90 <- predict(lgr90, test90, type="response")  # prediction

dim(test90)
predictedvalues90 = rep(0,9238)
predictedvalues90[res90>0.5] = 1

con_matrix_lgr90 <- table(predictedvalues90,actualvalues=test90[,1]) 

tests90 <- test90
tests90$Accident_Severity <- revalue(tests90$Accident_Severity, 
                                     c('High'='1','Low'='0'))
mean(predictedvalues90 == tests90[,1]) # correctness of prediction
# Acc: 0.6499

# (4) DT:

detree90 <- rpart(Accident_Severity~., train90, method='class')

predicted_dt90 <- predict(detree90,test90,type = 'class')
con_matrix_dt90 <- confusionMatrix(predicted_dt90,test90$Accident_Severity) 
con_matrix_dt90 # Acc: 0.6375

######################################################################

# plotting the trend

nb_accuracy <- c(0.6603,0.6587,0.6596,0.6558,0.6525)
rf_accuracy <- c(0.7419,0.7503,0.7608,0.7621,0.7703)
lgr_accuracy <- c(0.6564,0.6546,0.6561,0.6542,0.6499)
dt_accuracy <- c(0.6366, 0.6358,0.6440,0.6428,0.6375)
splitratio <- c(50,60,70,80,90)

par(mfrow=c(2,2))

plot(splitratio, nb_accuracy, type='l',col='blue',main = 'Naive Bayes')
plot(splitratio, rf_accuracy, type='l',col='red',main='Random Forest')
plot(splitratio, lgr_accuracy, type='l',col='green',main='Logistic Regression')
plot(splitratio, dt_accuracy, type='l',col='brown',main = 'Decision Tree')

#--------------------------------------------------------------------------------------------------------------------------------
######################################################################################################################################
######################################################################################################################################
######################################################################################################################################