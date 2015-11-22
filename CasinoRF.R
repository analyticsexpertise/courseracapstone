
require(caret)
require(randomForest)

### Random Forest Classification 


### Train model to predict stars based upon attribute review analysis

pred_rf <-function(){
  
  if(exists("casinoTrain",1)==FALSE)
  {
    load("casinoTrain.RData",.GlobalEnv)
  }
  
  set.seed(1431)
  
  df_train <<- casinoTrain[ ,3:ncol(casinoTrain)]
  
  df_train$name_col <<- as.factor(df_train$name_col)
  
  pred.rf <<- randomForest(stars~name_col+reviews+mean_stars+mean_cor
                           ,data=df_train
                           ,importance=TRUE,do.trace=100,mtry=2,ntree=1000)
  
  save(pred.rf, file = "predrf.RData")
}

test_rf <-function(){
  
  df_test <<- casinoTest[ ,3:ncol(casinoTest)]
  
  df_test$name_col <<- as.factor(df_test$name_col)
  
  levels(df_test$name_col) <<- levels(df_train$name_col)
  
  rfPred <<- predict(pred.rf,newdata=df_test)
  
  rftest <<- table(rfPred,df_test$stars)
  
  rfcm <<- confusionMatrix(rftest)
  
  save(rfcm,file="rfcm.RData")
  
}

see_rf_importance <-function(){
  
  par(mfrow=c(1,2))
  
  varImpPlot(pred.rf,type=2,main = "Variable Importance - GINI",cex=0.7)
  
  varImpPlot(pred.rf,type=1,main = "Variable Importance - Accuracy",cex=0.7)
  
}




