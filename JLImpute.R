
JLImpute <- function(data){

####age cannot be zero, set to NA
  data <- data %>%
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
for(i in 2:length(data)){
  if(length(unique(data[,i]))!=2){
    data[,i] <- (data[,i]-mean(data[,i],na.rm=T))/sd(data[,i],na.rm=T)
    data[,i] <- ifelse(data[,i]>= 3.29,3.29,data[,i])
    data[,i] <- ifelse(data[,i]<= -3.29,-3.29,data[,i])
  }
}

####rpart to impute nas in data and test sets. 
####stepwise regression was not realistic and decision trees selected best features for partitioning data
####mse of 11.51 will get us closer to actual values rather than simply using the mean. 
age.impute <- predict(age.prune,data)
age2.impute <- predict(age2.prune,data)
recchrge.impute <- predict(recchrge.prune,data)
revenue.impute <- predict(revenue.prune,data)
overage.impute <- predict(overage.prune,data) 
directas.impute <- predict(directas.prune,data) 
roam.impute <- predict(roam.prune,data) 
mou.impute <- predict(mou.prune,data) 

data <- data %>%
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
data <- data[sample(nrow(data)),]


}