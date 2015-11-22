
require(rpart)
require(rpart.plot)

### classify casino star

see_predict_tree <- function(){
  
  if(exists("bigcasinodf",1)==FALSE)
  {
    load("bigcasinodf.RData",.GlobalEnv)
  }
  
  df <- bigcasinodf
  
  pred_control <- rpart.control(xval=10, minbucket=25, minsplit=50, cp=0.0001)
  
  predTree <<- rpart(name_col ~ stars+max_cor,
                     data=df, na.action = na.pass,control=pred_control)
  
  rpart.plot(predTree, type=4, faclen = 0, cex=0.7)
  
}



casino_tree <- function(list_num=1){
  
  if(exists("bigcasinodf",1)==FALSE)
  {
    load("bigcasinodf.RData",.GlobalEnv)
  }
  
  casino_list <- unique(bigcasinodf$name)
  
  df <- bigcasinodf[bigcasinodf$name==casino_list[list_num], ]
  
  pred_control <- rpart.control(xval=10, minbucket=2, minsplit=1, cp=0.0001)
  
  predTree <<- rpart(name_col ~ max_stars+max_cor+mean_stars+mean_cor+min_stars+min_cor,
                     data=df, na.action = na.pass,control=pred_control)
  
  rpart.plot(predTree, type=4, faclen = 0, cex=0.7)
  
}