

### Load Data Frames 

load_dfs <- function(){
  
  # Load data frame containing casino business and attribute data for modeling and analysis
  load("bigcasinodf.RData",.GlobalEnv)
  
  # Load data frame containing model training data
  load("casinoTrain.RData",.GlobalEnv)
  
  # Load data frame containing model testing data
  load("casinoTest.RData",.GlobalEnv)
  
  # Load data frame containing casino business data
  load("biz_sample.RData",.GlobalEnv)
  
  # Load data frame containing summary of reivew & attribute association analysis
  load("fitdf.RData",.GlobalEnv)
  
  # Load data frame containing result of review and attribute association analysis
  load("revassocdf.RData",.GlobalEnv)
  
  # Load random forest predictive model
  load("predrf.RData",.GlobalEnv)
  
  #Load random forest confusion matrix
  load("rfcm.RData",.GlobalEnv)
  
  #Load rpart classification model
  load("predTree.RData",.GlobalEnv)
}
