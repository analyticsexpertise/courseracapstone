### Explore Text

### Get Reviews for target data
require(tm)
require(SnowballC)
require(wordcloud)
require(data.table)
require(partykit)
require(wordcloud)
require(gdata)
require(rpart)
require(rpart.plot)
require(dplyr)
require(randomForest)


### Create data frame of reviews word association with attributes and stars
create_revassocdf <- function(){
  
  create_attrib_vecs()
  
  exploretext(num_stars=1,df=crevdf)
  
  revassocdf <<- assocdf
  
  exploretext(num_stars=2,df=crevdf)
  
  revassocdf <<- rbind(revassocdf,assocdf)
  
  exploretext(num_stars=3,df=crevdf)
  
  revassocdf <<- rbind(revassocdf,assocdf)
  
  exploretext(num_stars=4,df=crevdf)
  
  revassocdf <<- rbind(revassocdf,assocdf)
  
  exploretext(num_stars=5,df=crevdf)
  
  revassocdf <<- rbind(revassocdf,assocdf)
  
  
  revassocdf$max_cor <<- as.numeric(revassocdf$max_cor)
  revassocdf$median_cor <<- as.numeric(revassocdf$median_cor)
  revassocdf$min_cor <<- as.numeric(revassocdf$min_cor)
  revassocdf$mean_cor <<- as.numeric(revassocdf$mean_cor)
  revassocdf$words <<- as.numeric(revassocdf$words)
  revassocdf$stars <<- as.numeric(revassocdf$stars)
  
  revassocdf <<- revassocdf[order(revassocdf$name_col), ]
  
}

### See correlation of attributes & stars based upon review word association

see_rtree <- function(attribs = v_ambiance){
  
  revTree <<- rpart(stars ~ name_col + max_cor,
                    data=revassocdf[revassocdf$name_col %in% attribs, ])
  
  rpart.plot(revTree, type=4, faclen = 0, cex=0.7)
  
  return(revTree)
}

### create attribute dataframe for modelling

create_model_df <- function(){
  
 fitdf <<- summarize(group_by(revassocdf, name_col), 
                    max_cor=max(max_cor), 
                    mean_cor = mean(mean_cor),
                    min_cor = min(min_cor),
                    max_stars=max(stars),
                    mean_stars = mean(stars),
                    min_stars = min(stars))
  
}


### predict casino star rating based upon attribute sets

see_predict_tree <- function(){
  
  pred_control <- rpart.control(xval=10, minbucket=2, minsplit=10, cp=0.0001)
  
  predTree <<- rpart(max_stars ~ name_col + max_cor + min_cor + min_stars + mean_stars,
                     data=fitdf, na.action = na.pass,control=pred_control)
  
  return(predTree)
  
}

predict_attr_star <- function(df = casinodf[casinodf$name_col %in% fitdf$name_col, ]){
  
  fit <<- see_predict_tree()
  
}

### create training set to predict casino star rating based upon atttibutes

create_training_set <- function(){
  
  get_samples()
  
  create_preddf()
  
}

get_samples <- function(sample_percent = 0.75){
  
  set.seed(13554779)
  
  num_casinos <- length(attdf$business_id)
  
  sample_size <- round((num_casinos * sample_percent),1)
  
  index <- sample(1:nrow(attdf),sample_size)
  
  df <- attdf[attdf$business_id %in% crevdf]
  
  biz_sample <<- attdf[index,c("business_id","name","stars","review_count")] 
  
  
}


create_preddf <- function(){
  
  preddf <<- data.frame(t(as.data.frame(fitdf$name_col)),row.names = NULL,stringsAsFactors = FALSE)
  colnames(preddf)<<-fitdf$name_col
  preddf <<- preddf[-1, ]
  
  for (i in 1 : length(as.character(biz_sample$business_id))){
    
    rm(casinoassocdf,pos=1)
    
    create_casinoassocdf(casino_id=as.character(biz_sample$business_id[i]))
    
    if(is.null(casinoassocdf)==FALSE)
    {
      create_casino_df()
      
      
      
      create_predicting_df(i)
    }
    
  }
  
  add_preddf_bizdata()
  
  
}



create_predicting_df <- function(n=1){
  
 df<-casinodf[casinodf$name_col %in% fitdf$name_col, ]
 
 if (length(df$name_col) > 0){
 star_pred <<- predict(fit,newdata=df,na.action=na.pass)
 
 if(is.null(star_pred)==FALSE){
  map_to_preddf(n)
 }
 
 }
  
}



add_preddf_bizdata <- function(df=biz_sample){
  
  preddf <<- cbind(df,preddf)
  
}

map_to_preddf <- function(n=1){
  
  df <- data.frame(t(as.data.frame(star_pred)),stringsAsFactors = FALSE,row.names = NULL)
  colnames(df) <- casinodf$name_col
  
  cols2use <- which(fitdf$name_col %in% casinodf$name_col)
  
  if(length(cols2use) > 0){
    for (i in 1:length(cols2use)){
      preddf[n,cols2use[i]] <<- as.numeric(star_pred[i])
      ### preddf[n,cols2use[i]] <<- !is.na(star_pred[i])
    }
  }
  
}

### from training set create CART model to predict casino stars 
create_pred_tree <- function(){
  
  predMat <- preddf[ ,3:length(preddf[1, ])]
  
  predMat[is.na(predMat)]<-FALSE
  
  predMat <- as.data.frame(unclass(predMat))
  
  predMat$review_count <- as.numeric(predMat$review_count)
  
  pred_control <- rpart.control(xval=5, minbucket=10, minsplit=20, cp=0.0001)
  
  pred_Tree<<- rpart(as.factor(stars) ~ .,data = predMat,method='class',control=pred_control)
  
}

see_pred_tree <- function(tree=pred_Tree){
  
  rpart.plot(tree, type=4, faclen = 0, cex=0.7)
  
  printcp(tree)
  
}

review_pred_tree <- function(tree=pred_Tree){
  
  create_pred_tree()
  
  see_pred_tree()
  
  
}


  
### Casino_Star_Rating ~ Ambiance + Good4 + Parking + Music + Payment

rpart_casino <-function(){
  
  pred_control <- rpart.control(xval=10, minbucket=50, minsplit=50, cp=0.0001)
  
  casinoTree <<- rpart(as.factor(name_col) ~ max_stars + max_cor,
                     data=bigcasinodf, na.action = na.pass,control=pred_control)
  
  see_pred_tree(casinoTree)
  
}




pred_rf_model <- function(){
  
  predMat <- preddf[ ,3:length(preddf[1, ])]
  
  predMat[is.na(predMat)]<-0
  
  predMat <- as.data.frame(unclass(predMat))
  
  predMat$review_count <- as.numeric(predMat$review_count)
  
  predMat <- predMat[ ,sapply(predMat, function(col) length(unique(col))) > 1]
  
  rf_model<<-train(as.factor(stars)~.,data=predMat,method="rf",
                  trControl=trainControl(method="cv",number=10),
                  prox=TRUE,allowParallel=TRUE,
                  preProcess = c("pca"))
}

pred_rforest <- function(){
  
  set.seed(1431)
  
  pred.rf <<- randomForest(factor(name_col)~max_stars+max_cor,data=bigcasinodf,importance=TRUE,do.trace=100,mtry=2,ntree=1000)
  
  
}

pred_ctree <-function(){
  
  
  pred.ct <<- ctree(factor(mean_stars)~factor(name_col)+max_cor,data=bigcasinodf)
  
  plot(pred.ct,type="simple")
  
}

################################################### Support Functions ##################################

prune_tree <- function(tree=predTree){
  
  return(prune(tree,cp= tree$cptable[which.min(tree$cptable[,"xerror"]),"CP"]))
  
  
}

preddf_num_cols <- function(){
  
  for(i in 1:length(preddf[1, ])){
    
    if (i > 3){
      
      preddf[,i] <<- as.numeric(preddf[,i])
      
    }
    
  }
  
}

create_casino_df <- function(){
  
  df <- casinoassocdf[casinoassocdf$name_col %in% revassocdf$name_col, ]
  
  casinodf <<- summarize(group_by(df, name_col), 
                         max_cor=max(max_cor), 
                         mean_cor = mean(mean_cor),
                         min_cor = min(min_cor),
                         max_stars=max(stars),
                         mean_stars = mean(stars),
                         min_stars = min(stars))
  
 if(exists("bigcasinodf",1)==FALSE){
   
   bigcasinodf <<- casinodf
 }
 else
 {
   
   
   bigcasinodf <<- rbind(bigcasinodf,casinodf)
   
 }

  
}


### Create review sentiment attribute data frame for a casino
create_casinoassocdf <- function(casino_id=crevdf$business_id[1]){
  
  
  
  casino_id = as.character(casino_id)
  
  casino_df = crevdf[as.character(crevdf$business_id)==casino_id, ]
  
  crevdf_stars = unique(casino_df$stars)
  
  for (i in 1:length(crevdf_stars)){
    
    exploretext(num_stars=crevdf_stars[i],df=casino_df)
    
    if(exists("assocdf")==TRUE)
    {  
    
      if(i==1)
      {  
        casinoassocdf <<- assocdf
      }
      else
      {
        
        casinoassocdf <<- rbind(casinoassocdf,assocdf)
        
      }
    
    }
  
  }
  
  if(exists("casinoassocdf",1)==FALSE)
  {  
    casinoassocdf <<- NULL
  }
  else
  {
    if(is.null(casinoassocdf)==FALSE)
    {
      casinoassocdf$max_cor <<- as.numeric(casinoassocdf$max_cor)
      casinoassocdf$median_cor <<- as.numeric(casinoassocdf$median_cor)
      casinoassocdf$min_cor <<- as.numeric(casinoassocdf$min_cor)
      casinoassocdf$mean_cor <<- as.numeric(casinoassocdf$mean_cor)
      casinoassocdf$words <<- as.numeric(casinoassocdf$words)
      casinoassocdf$stars <<- as.numeric(casinoassocdf$stars)
        
      casinoassocdf <<- casinoassocdf[order(casinoassocdf$name_col), ]
    }
  }
  
}




create_attrib_vecs <- function(){
  
  v_ambiance <<- c("romantic","intimate","classy","hipster"
                   ,"divey","touristy","trendy","upscale"
                   ,"casual","ambiance")
  
  v_good4 <<- c("dessert","latenight","lunch","dinner","breakfast"
                ,"brunch","kids","dancing","delivery","smoking","waiter","service"
                ,"coat","takeout","reservations","alcohol"
                ,"noise","attire","tv","television","wifi"
                ,"caters","drivethru","wheelchair","access","byob",
                "corkage","dogs","24hours","24","hours","price","food")
  
  v_parking <<- c("garage","street","validated","lot", "valet"
                  ,"parking")
  
  v_music <<- c("dj","background","karaoke","live","video",
                "jukebox","playlist","music")
  
  v_payment <<- c("amex","cash","mastercard","visa","discover","payment")
  
  
  
  
}

exploretext <- function(num_stars=5,df=crevdf){
  
  ### 1. Create Corpus
  
  explore_stars <<- num_stars
  
  createCorpus(df[df$stars==num_stars, ])
  
  ### 2. Create Document Term Matrix (DTM)
  createDtm()
  
  ### 3. Create association tables
  tableAssocs()
  
  ### 4. Create association data frame  
  make_assocsdf()
  
}

createCorpus <- function(df=crevdf){
  
  myCorpus <<- Corpus(VectorSource(df$text))
  
  myCorpus <<- tm_map(myCorpus, tolower)
  
  myCorpus <<- tm_map(myCorpus, PlainTextDocument)
  
  myCorpus <<- tm_map(myCorpus, removePunctuation)
  
  myCorpus <<- tm_map(myCorpus, removeNumbers)
  
  myCorpus <<- tm_map(myCorpus, removeWords, stopwords("english"))
  
 
 myCorpus <<- tm_map(myCorpus, stemDocument)
  
 
  
}

createDtm <- function(){
  
  myDtm <<- TermDocumentMatrix(myCorpus, control = list(minWordLength = 4)) 
  
}


getAssoc <- function(myWord = "good"){
  
  findAssocs(myDtm,myWord,0.30)
  
}

getAssocs <- function(myWords = c("bad","good")){
  
  return(rbindlist(lapply(myWords, getAssoc),use.names = TRUE,
                   fill=TRUE))
  
}

tableAssocs <- function(){
  
  tab_ambiance <<-getAssocs(v_ambiance)
  
  tab_good4 <<- getAssocs(v_good4)
  
  
  tab_parking <<- getAssocs(v_parking)
  
  
  tab_music <<- getAssocs(v_music)
  
  tab_payment <<- getAssocs(v_payment)
  
  
}

makeAssocs <- function(df=tab_ambiance){
  
  
  n = ncol(df)
  
  for (i in 1 : n)
  {
    dfc <- df[[i]]
    
    name_col = colnames(df)[i]
    
    dfc <- dfc[complete.cases(dfc)]
    
    words = length(dfc)
    
    if(is.na(words)==TRUE){words = 0}
    
    max_cor = round(max(dfc,na.rm=TRUE),2)
    median_cor = round(median(dfc,na.rm=TRUE),2)
    mean_cor = round(mean(dfc,na.rm=TRUE),2)
    min_cor = round(min(dfc,na.rm=TRUE) ,2)
    
    if(i==1 && exists("assocdf",1)==FALSE){
      
      assocdf <<- as.data.frame(cbind(name_col,words,max_cor,median_cor,mean_cor,min_cor
                                ,stars=explore_stars),
                                stringsAsFactors=FALSE)
      
    }
    else{
      
      assocdf <<- rbind(assocdf,cbind(name_col,words,max_cor,median_cor,mean_cor,min_cor
                                      ,stars=explore_stars))  
    }
    
  }  
  
}
  
  
make_assocsdf <- function(){
  
  rm(assocdf,pos=1)
  
  if (nrow(tab_ambiance) > 0) {makeAssocs(tab_ambiance)}
  if (nrow(tab_good4) > 0) {makeAssocs(tab_good4)}
  if (nrow(tab_music) > 0) {makeAssocs(tab_music)}
  if (nrow(tab_parking) > 0) {makeAssocs(tab_parking)}
  if (nrow(tab_payment) > 0) {makeAssocs(tab_payment)}
  
  if(exists("assocdf")==TRUE)
  {
    assocdf <<- assocdf[assocdf$words >0, ]
    
    assocdf <<- assocdf[order(assocdf$mean_cor,decreasing=TRUE), ]
  }
  else {assocdf <<- NULL}
  
}


  
