library(dplyr)
library(car)
library(cvTools)
library(pROC)
library(gbm) # for gradient boosting
library(cvTools) # for cv tuning
library(xgboost) # for xgboost
library(randomForest) 
setwd("C:/Users/ASUS/OneDrive/Desktop/Edvancer R/R")
source("createDummies.r")
setwd("C:/Users/ASUS/OneDrive/Desktop/Edvancer R/R/projectdb")

hr_train = read.csv("hr_train.csv")
hr_test = read.csv("hr_test.csv", stringsAsFactors = T)
hr_test$left = NA
hr_test$data = "test"
hr_train$data = "train"

modi = read.csv("dimag.csv") 

hr_train1 = read.csv("hr_train.csv")

all_hr = rbind(hr_train,hr_test)

all_hr = createDummies(all_hr, "sales")
all_hr = createDummies(all_hr ,"salary")

sum(is.na(all_hr)) # no na values
colSums(is.na(all_hr))
all_hr$left = as.numeric(all_hr$left ==1)
all_hr$left = as.factor(all_hr$left)


hr_train = all_hr %>% filter(data == "train") %>% select(-data)
hr_test = all_hr %>% filter(data =="test") %>% select(-left, -data)
#---------------------------------------------------------------
 # param = list(interaction.depth = 1:20,
 #              n.trees = c(50,100,200,500,700,300,800,900,1000,
 #                          150,250,350,450,550,650,750),
 #              shrinkage = c(0.1, 0.01, 0.001, 0.0001),
 #              n.minobsinnode = c(1,2,5,10,15,20,25,30,35,40)
 #   )
# 
# subset_paras=function(full_list_para,n=10){
#   all_comb=expand.grid(full_list_para)
#   s=sample(1:nrow(all_comb),n)
#   subset_para=all_comb[s,]
#   return(subset_para)
# }
# 
# num_trials = 100
# my_params = subset_paras(param,num_trials)

mykfolds = function(nobs, nfold = 5){
  t = cvFolds(nobs, K = nfold, type = "random")
  folds = list()
  for(i in 1:nfold){
    test = t$subsets[t$which == i]
    train = t$subsets[t$which != i]
    folds[[i]] = list("train" = train, "test"= test)
  }
  return(folds)
}



nfold = 100
myfolds = mykfolds(nrow(hr_train), nfold);

hr_train_layer = data.frame(rf_var = numeric(nrow(hr_train)),
                            gbm_var = numeric(nrow(hr_train)))



for(i in 1:nfold){
  print(paste0("starting iteration:" , i))
  fold = myfolds[[i]]
  
  train_data = hr_train[fold$train, ]
  test_data = hr_train[fold$test, ]
  print("rf")
  
  rf.fit = randomForest(as.factor(left)~., data = train_data,  mtry = 20, ntree= 50)
  
  rf_score = predict(rf.fit, newdata = test_data, type = "prob")[ ,1]
  
  print("gbm")
  gbm.fit = gbm(left~., data = train_data, distribution = "bernoulli", n.trees = 500)
  gbm_score = predict(gbm.fit, newdata = test_data, 
                      n.trees = 500, type = "response")
  
  hr_train_layer$rf_var[fold$test] = rf_score
  hr_train_layer$gbm_var[fold$test] = gbm_score
  
};View(hr_train_layer)

#rf.val = ifelse(rf_score > 0.5, 1, 0)


hr_test_layer = data.frame(rf_var = numeric(nrow(hr_test)),
                            gbm_var = numeric(nrow(hr_test)))

full.rf = randomForest(factor(left)~., data = hr_train, mtry = 20, ntree= 50)

hr_test_layer$rf_var = predict(full.rf, newdata = hr_test, type="prob")[,1]

full.gbm = gbm(left~., data =hr_train, distribution = "bernoulli",n.trees = 500)
hr_test_layer$gbm_var = predict(full.gbm, newdata = hr_test, n.trees = 500,
                                type = "response")

hr_train_layer$left = hr_train$left

log.mod = glm(left~., data = hr_train_layer, family = "binomial")


test.score = predict(log.mod, newdata = hr_test_layer, type ="response")

class.pred = as.numeric(test.score > 0.5)


test.score_train = predict(log.mod, newdata = hr_train_layer, type ="response")

class.pred_train = as.numeric(test.score_train > 0.5)

#layer_rf = ifelse(hr_test_layer$rf_var > 0.5, 1, 0)
#layer_gbm =  ifelse(hr_test_layer$gbm_var < 0.5, 1, 0)

table(layer_gbm, layer_rf)
err = mean(class.pred != hrtestlayer_rf)
print(paste0("test error ", err))

auc_score=auc(roc(hr_train_layer$left,class.pred_train)); auc_score

write.table(class.pred, "hr5.csv", row.names = F, col.names = "left")



scores12 = View(data.frame( "rf" = layer_rf, 
                            "lr" = class.pred,"gbm" = layer_gbm, "rfsc"= hr_test_layer$rf_var,
                             "gbmscore "= hr_test_layer$gbm_var)
                  )


modifi = read.csv("train_hr1.csv")


































