library(dplyr)
library(car)
library(cvTools)
library(pROC)
library(gbm) # for gradient boosting
library(cvTools) # for cv tuning
library(xgboost) # for xgboost
library(randomForest) 
#createDummies function
source("createDummies.r")

#Impoting data in R
hr_train = read.csv("hr_train.csv")
hr_test = read.csv("hr_test.csv", stringsAsFactors = T)

#Adding target variable "left" in test data
hr_test$left = NA

#Adding new column to differentiate between train and test data after combining
hr_test$data = "test"
hr_train$data = "train"

#Combining train and test data
all_hr = rbind(hr_train,hr_test)

#creating dummies for categorical columns
all_hr = createDummies(all_hr, "sales")
all_hr = createDummies(all_hr ,"salary")

#Finding NA values
sum(is.na(all_hr)) # no na values
colSums(is.na(all_hr))
#Converting left variable to factor
all_hr$left = as.numeric(all_hr$left ==1)
all_hr$left = as.factor(all_hr$left)


#Splitting train and test data from combined data
hr_train = all_hr %>% filter(data == "train") %>% select(-data)
hr_test = all_hr %>% filter(data =="test") %>% select(-left, -data)

#Building stacking model---------------------------
#function to create folds
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

#using only two models heres for stacking
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


## stack layer 2  data  for test 
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

table(layer_gbm, layer_rf)
err = mean(class.pred != hrtestlayer_rf)
print(paste0("test error ", err))

auc_score=auc(roc(hr_train_layer$left,class.pred_train)); auc_score


###Random Forest model--------------------------------------------------------------------
#Paramter list
param = list(mtry = c(1,2,4,6,8,10,12,14,16,18,20),
             ntree = c(50,100,500,700,800,900,1000,
                       #150,250,350,450,550,650,750),
             ntree = c(500,400,600,700),
             maxnodes = c(5,10,15,20,30),
             nodesize = c(1,2,5,15,10))

#subtsetting parameter
subset_paras = function(full_list_para, n = 10){
  all_comb = expand.grid(full_list_para)
  set.seed(128)
  s = sample(1:nrow(all_comb), n)
  subset_para = all_comb[s,]
  return(subset_para)
}
  #Cost function
  mycost_auc = function(y, yhat){
    roccurve=pROC::roc(y,yhat)
    score=pROC::auc(roccurve)
    return(score)
  }
  num_trials= 20
  my_params=subset_paras(param,num_trials)
  
  myauc <- 0
  
  for(i in 1:num_trials){
    print(paste('starting iteration :',i))
    # uncomment the line above to keep track of progress
    params = my_params[i,]
    
    k = cvTuning(randomForest, left~., 
                 data =train1,
                 tuning =params,
                 folds = cvFolds(nrow(train1), K=12, type ="random"),
                 cost =mycost_auc, 
                 seed =2,
                 predictArgs = list(type="prob")
    )
    score.this=k$cv[,2]
   
    if(score.this>myauc){
      print(params)

      myauc=score.this
      print(myauc)
      best_params=params
    }
    
    print('DONE')
  };best_params;myauc
             
  best_params=data.frame(mtry = 8,
                         ntree=500,
                         maxnodes=110,
                         nodesize=1)
  
  ## Model on the entire training data
  
  left_new_rf=randomForest(as.factor(left)~.,
                           mtry=best_params$mtry,
                           ntree=best_params$ntree,
                           maxnodes=best_params$maxnodes,
                           nodesize=best_params$nodesize,
                           data=train1
  );left_new_rf
  


train.score = predict(left_new_rf, newdata = train1, type = "prob")
train.pred = ifelse(train.score> 0.5, 1, 0)
auc_score=auc(roc(train1$left,train.score[,2])); auc_score

val.score = predict(left_new_rf, newdata = train2, type = "prob")
val.pred = ifelse(val.score > 0.5, 1, 0)
auc_score=auc(roc(train2$left,val.score[,1])); auc_score


test.score = predict(left_new_rf, newdata = hr_test, type = "prob")
test.pred = ifelse(test.score > 0.5, 1, 0)

left = test.pred[,2]


































