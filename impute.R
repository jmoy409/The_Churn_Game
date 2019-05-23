#script performs feature selection for new columns and uses recursive partitioning and regression trees
#to fill in missing values instead of using the mean


####age cannot be zero, set to NA
df <- df %>%
  mutate(age1 = ifelse(age1==0,NA,age1)) %>%

####Create block ratio
  mutate(blockratio = ifelse((incalls+outcalls)==0,
                             0,
                             ifelse(blckvce==0,
                                    0,
                                    blckvce/(incalls+outcalls)))) %>%
  
####Create dropped ratio
  mutate(dropratio = ifelse((incalls+outcalls)==0,
                            0,
                            ifelse(dropvce==0,
                                   0,
                                   dropvce/(incalls+outcalls)))) %>%

####if na changem means that previous values do not exist thus impute NAs

  mutate(changem = ifelse(is.na(changem),0,changem)) %>%

####if na changer means that previous values do not exist thus impute NAs

  mutate(changer = ifelse(is.na(changer),0,changer)) %>%
  dplyr::select(-incalls,-outcalls,-dropvce,-blckvce)




####z score all continuous variables and set the threshold to the 99th percentile in either direction
####initially used type of == double. did not convert all non dummy vars
for(i in 2:length(df)){
  #skip churndep
  #perform for each column z score normalization between 0 and 1
  if(length(unique(df[,i]))!=2){
    df[,i] <- (df[,i]-mean(df[,i],na.rm=T))/sd(df[,i],na.rm=T)
    df[,i] <- ifelse(df[,i]>= 3.29,3.29,df[,i])
    df[,i] <- ifelse(df[,i]<= -3.29,-3.29,df[,i])
  }
}


####rpart to impute nas in df and test sets. 
####stepwise regression was not realistic and decision trees selected best features for partitioning data
####mse of 11.51 will get us closer to actual values rather than simply using the mean. 
age.df <- df %>% select(-churndep)

#build a recursive partition model (Decision Tree) for age1
#make a prediction for age
#method='class' if categorical

#plot(fit.age)
#plot alone just draws how complex the DT is. text helps
#text(fit.age)

#but essentially, rpart.plot will provide better visualizations.
#rpart.plot(fit.age)
#more information from plot
#rpart.plot(fit.age, type=4, extra=101)

#cptable-matrix of information on the optimal pruning based on a complexity parameter
#contains the mean and standard deviations of the errors
#cp - desired list of complexity values

fit.age <- rpart(age1~.,data=age.df)
#choose the lowest error in the datatable
opt <- which.min(fit.age$cptable[,"xerror"])
#choose the corresponding CP value
cp <- fit.age$cptable[opt, "CP"]
#prune(cost-complexity) the tree to that degree
#snip off the least important splits based on complexity parameter
age.prune <- prune(fit.age, cp = cp)

#predict(model,dataframe)
pred.age <- predict(age.prune,age.df)
#how much error are we off by(Of the values that are NOT NA for comparison on the technique itself)
sqrt(round(mean((age.df$age1 - pred.age)^2,na.rm=T),3))
age.impute <- predict(age.prune,df)


fit.age2 <- rpart(age2~.,data=age.df)

opt <- which.min(fit.age2$cptable[,"xerror"])
cp <- fit.age2$cptable[opt,"CP"]
age2.prune <- prune(fit.age2, cp = cp)
pred.age2 <- predict(age2.prune,age.df)
sqrt(round(mean((age.df$age2 - pred.age2)^2,na.rm=T),3))
age2.impute <- predict(age2.prune,df)


fit.recchrge<- rpart(recchrge~.,data=age.df)
opt <- which.min(fit.recchrge$cptable[,"xerror"])
cp <- fit.recchrge$cptable[opt, "CP"]
recchrge.prune <- prune(fit.recchrge, cp = cp)
pred.recchrge<- predict(recchrge.prune,age.df)
sqrt(round(mean((age.df$recchrge - pred.recchrge)^2,na.rm=T),3))
recchrge.impute <- predict(recchrge.prune,df)


####Impute the revenue variable with regression trees
####z score df error is 0.707
fit.revenue <- rpart(revenue~.,data=age.df)
opt <- which.min(fit.revenue$cptable[,"xerror"])
cp <- fit.revenue$cptable[opt, "CP"]
revenue.prune <- prune(fit.revenue, cp = cp)
pred.revenue <- predict(revenue.prune,age.df)
sqrt(round(mean((age.df$revenue - pred.revenue)^2,na.rm=T),3))
revenue.impute <- predict(revenue.prune,df)

####Impute the overage variable with regression trees
fit.overage <- rpart(overage~.,data=age.df)
opt <- which.min(fit.overage$cptable[,"xerror"])
cp <- fit.overage$cptable[opt, "CP"]
overage.prune <- prune(fit.overage, cp = cp)
pred.overage <- predict(overage.prune,data=age.df)
sqrt(round(mean((age.df$overage - pred.overage)^2,na.rm=T),3))
overage.impute <- predict(overage.prune,df) 


####Impute the directas variable with regression trees
fit.directas <- rpart(directas~.,data=age.df)
opt <- which.min(fit.directas$cptable[,"xerror"])
cp <- fit.directas$cptable[opt, "CP"]
directas.prune <- prune(fit.directas, cp = cp)
pred.directas <- predict(directas.prune,data=age.df)
sqrt(round(mean((age.df$directas - pred.directas)^2,na.rm=T),3))

directas.impute <- predict(directas.prune,df) 

####Impute the roam variable with regression trees
fit.roam <- rpart(roam~.,data=age.df)
opt <- which.min(fit.roam$cptable[,"xerror"])
cp <- fit.roam$cptable[opt, "CP"]
roam.prune <- prune(fit.roam, cp = cp)
pred.roam <- predict(roam.prune,data=age.df)
sqrt(round(mean((age.df$roam - pred.roam)^2,na.rm=T),3))
roam.impute <- predict(roam.prune,df) 


####Impute the mou variable with regression trees
fit.mou <- rpart(mou~.,data=age.df)
opt <- which.min(fit.mou$cptable[,"xerror"])
cp <- fit.mou$cptable[opt, "CP"]
mou.prune <- prune(fit.mou, cp = cp)
pred.mou <- predict(mou.prune,data=age.df)
sqrt(round(mean((age.df$mou - pred.mou)^2,na.rm=T),3))
mou.impute <- predict(mou.prune,df) 



df <- df %>%
  cbind(age.impute) %>%
  cbind(revenue.impute) %>%
  cbind(directas.impute) %>%
  cbind(overage.impute) %>%
  cbind(age2.impute) %>%
  cbind(recchrge.impute) %>%
  cbind(roam.impute) %>%
  cbind(mou.impute) %>%
  mutate(age1 = ifelse(is.na(age1),age.impute,age1),
         age2 = ifelse(is.na(age2),age.impute,age2),
         revenue = ifelse(is.na(revenue),revenue.impute,revenue),
         recchrge = ifelse(is.na(recchrge),recchrge.impute,recchrge),
         directas = ifelse(is.na(directas),directas.impute,directas),
         roam = ifelse(is.na(roam),roam.impute,roam),
         overage = ifelse(is.na(overage),overage.impute,overage),
         mou = ifelse(is.na(mou),mou.impute,mou)
  ) %>%
  select(-age.impute,-age2.impute,-revenue.impute,-directas.impute,-roam.impute,
         -recchrge.impute,-overage.impute,-mou.impute)
df <- df[sample(nrow(df)),]

rm(age.df);rm(age.impute);rm(age2.impute);rm(cp);rm(directas.impute);rm(fit.age);rm(fit.age2)
rm(fit.directas);rm(fit.overage);rm(fit.recchrge);rm(fit.revenue)
rm(overage.impute);rm(i);rm(pred.age);rm(pred.age2);rm(pred.directas);rm(pred.overage)
rm(pred.recchrge);rm(pred.revenue);rm(pred.roam);rm(recchrge.impute);rm(revenue.impute)
rm(roam.impute);rm(fit.mou);rm(mou.impute);rm(opt);rm(pred.mou);rm(fit.roam)
