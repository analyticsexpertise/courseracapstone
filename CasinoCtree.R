
require(partykit)
require(party)


### classify atrributes and ratings via conditional inference tree

pred_ctree <-function(mystars_upper=5,mystars_lower=4){
  
  if(exists("bigcasinodf",1)==FALSE)
  {
    load("bigcasinodf.RData",.GlobalEnv)
  }
  
  df <- bigcasinodf[as.numeric(droplevels(bigcasinodf$stars))>=mystars_lower &
                      as.numeric(droplevels(bigcasinodf$stars))<=mystars_upper, ]
  
  df$stars <- as.numeric(droplevels(df$stars))
  
  
  pred.ct <<- ctree(factor(name_col)~max_stars+max_cor
                    ,data=df)
  
  plot(pred.ct,type="simple")
  
}