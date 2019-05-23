#load libraries
#library(e1071)
#library(caret)
library(dplyr)
library(tidyr)
#library(ModelMetrics)
library(rpart)
library(glmnet)
library(caTools)
library(randomForest)
#library(rpart.plot)
library(RColorBrewer)

#set random seed
set.seed(5023)

df <- read.csv("/home/jmoy001/Documents/Analytics HW/cell2cell-v1.csv")
#df <- read.csv("C:/Users/jlowh001/Desktop/Summit Assignment/cell2cell-v1.csv")

#drop dummy variables, avoid dummy variable trap (n-1)
df <- df %>%
  dplyr::select(-newcelln,-incmiss,-occself,-marryun,-mailord,-prizmrur)


####split data: test and training set

#test set
validation <- df[is.na(df$churndep),]

#training set
df <- df[!is.na(df$churndep),]
df <- df[,-c(1,2,3,4,5)] #row number, X, customer, traintest, churn

#Get NA imputation for validation
#source("/home/jmoy001/Documents/Analytics HW/impute.R",  local = TRUE)
source("C:/Users/jlowh001/Desktop/Summit Assignment/impute.R",  local = TRUE)
#use training set to model NAs for test set
#source("/home/jmoy001/Documents/Analytics HW/JLImpute.R",  local = TRUE)
source("C:/Users/jlowh001/Desktop/Summit Assignment/JLImpute.R",  local = TRUE)

####summarize data for num of NA
sapply(df, function(x) sum(is.na(x)))


#specify weight, classwt of 1.75
RF_weights <- c(1,1.75)

#Random Forest
model_rf <- randomForest(as.factor(churndep)~., data=df, ntree=100, mtry=20, classwt=RF_weights, control=rpart.control(cp=0))
#have this saved if you want to read in an Rdata file
#save(model_rf, file="/home/jmoy001/Documents/Analytics HW/model_rf.Rdata")
#load("/home/jmoy001/Documents/Analytics HW/model_rf.Rdata")


#plot sorting by Mean Decreasing Gini
varImpPlot(model_rf, type=2)


#Variable Importance: get top 10 variables
var_imp10 <- importance(model_rf, type=2)
names <- row.names(var_imp10)
var_imp10<- data.frame(vars=names,importance=var_imp10, row.names=NULL)

var_imp10 <- var_imp10 %>%
  arrange(desc(MeanDecreaseGini))

#top 10 variables by importance
var_imp10 <- as.character(var_imp10$vars[1:10])

var_imp10 <- append(var_imp10, "churndep")

top10.df <- df[,names(df)%in%test]

View(top10.df)


#REDUE WITH VIF, CORRELATION MATRIX IS INVALID

#corr matrix - Upper and Lower
colnames(top10.df)
churndep <- top10.df$churndep
independent_vars <- top10.df[,!names(top10.df)%in%"churndep"]

pcor <- cor(independent_vars, use = "complete.obs")
pcor[upper.tri(pcor)] <- 0
diag(pcor) <- 0
upper.df <-independent_vars[,!apply(pcor,2,function(x) any(x > abs(0.6)))]
upper.df <- cbind(churndep,upper.df)

pcor <- cor(independent_vars, use = "complete.obs")
pcor[lower.tri(pcor)] <- 0
diag(pcor) <- 0
lower.df<-independent_vars[,!apply(pcor,2,function(x) any(x > abs(0.6)))]
lower.df <- cbind(churndep,lower.df)

rm(pcor);rm(independent_vars);rm(churndep)

colnames(upper.df)
colnames(lower.df)



#Lower.df

#model weights
#penalize for misclassifying, imbalanced dataset (30k towards not turn, 600 churn)
model_weights_l <- ifelse(lower.df$churndep==1,1.75,1)

mod_lower <-glm(churndep~., data=lower.df, weights= model_weights_l, family="quasibinomial")
#if model was created using glm function, must add type="response" to predict command

pred <- predict(mod_lower, lower.df, type="response")


pred<- ifelse(pred>=0.75,1,0)

#confusion matrix
#55.63% balanced accuracy for lower triangle training set
caret::confusionMatrix(lower.df$churndep, pred)

#test on test set

###impute the validation set with the JLImpute function
imputed.val <- JLImpute(validation)
imputed.val <- imputed.val[,-c(1,2,3,4,6)]

pred_test_set <- predict(mod_lower, imputed.val, type="response")

#threshold we want to use for our probability for 1 or 0
pred_test_set2<- ifelse(pred_test_set>=0.75,1,0)

#confusion matrix
#50.08% balanced accuracy for lower triangle training model on test set
caret::confusionMatrix(imputed.val$churn, pred_test_set2)


#Upper.df

#model weights
#penalize for misclassifying, imbalanced dataset (30k towards not turn, 600 churn)
model_weights_u <- ifelse(upper.df$churndep==1,1.75,1)

mod_upper <-glm(churndep~., data=upper.df, weights= model_weights_u, family="quasibinomial")
#if model was created using glm function, must add type="response" to predict command

pred <- predict(mod_upper, upper.df, type="response")

pred<- ifelse(pred>=0.75,1,0)

#confusion matrix
#55.63% balanced accuracy for lower triangle training set
caret::confusionMatrix(upper.df$churndep, pred)

#test training model on test set

###impute the validation set with the JLImpute function
imputed.val <- JLImpute(validation)
imputed.val <- imputed.val[,-c(1,2,3,4,6)]

pred_test_set <- predict(mod_upper, imputed.val, type="response")

#threshold we want to use for our probability for 1 or 0
pred_test_set2<- ifelse(pred_test_set>=0.75,1,0)


#confusion matrix
#50.08% balanced accuracy for lower triangle training model on test set
caret::confusionMatrix(imputed.val$churn, pred_test_set2)
