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
require(knitr)



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


### classify casino star rating based upon atttibutes

create_bigcasino <- function(){
  
  rm(bigcasinodf,pos=1)
  
  biz_sample <<- attdf[ ,c("business_id","name","stars","review_count")]
  
  create_preddf()
  
}

## create training and test sets from bigcasino data frame
split_data<-function(){
 
  library(caret)
  set.seed(1431)
  trainIndex <- createDataPartition(bigcasinodf$stars, p = .8,
                                    list = FALSE,
                                    times = 1)
  
  casinoTrain <<- bigcasinodf[trainIndex, ]
  casinoTest <<-  bigcasinodf[-trainIndex, ]
  
  save(casinoTrain, file = "casinoTrain.RData")
  save(casinoTest, file = "casinoTest.RData")
  
}





## create data frame with attribute review correlation and star classification for each casino
create_preddf <- function(){
  
  for (i in 1 : length(as.character(biz_sample$business_id))){
    
    bizID <<- as.character(biz_sample$business_id[i])
    bizName <<- as.character(biz_sample$name[i])
    bizstars <<- as.numeric(biz_sample$stars[i])
    bizreviews <<- as.numeric(biz_sample$review_count[i])
      
    rm(casinoassocdf,pos=1)
    
    create_casinoassocdf(casino_id=bizID)
    
    if(is.null(casinoassocdf)==FALSE)
    {
      create_casino_df()
      
    }
    
  }
  
  
}



### classify atrributes and ratings via conditional inference tree

pred_ctree <-function(){
  
  
  pred.ct <<- ctree(factor(name_col)~reviews+mean_stars+mean_cor
                    ,data=bigcasinodf)
  
  plot(pred.ct,type="simple")
  
}

################################################### Support Functions ##################################

#### Save data frames to RDS files

saveopdfs <- function(){
  
  save(biz_sample, file = "biz_sample.RData")
  
  save(fitdf, file = "fitdf.RData")
  
  save(revassocdf, file = "revassocdf.RData")
  
  save(bigcasinodf, file = "bigcasinodf.RData")
  
}

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

 if(length(casinodf$name_col)>0)
 {  
   if(exists("bigcasinodf",1)==FALSE){
     
     bigcasinodf <<- cbind(id=bizID,name=bizName,stars=bizstars,reviews=bizreviews,casinodf)
   }
   else
   {
     
     df2 <- cbind(id=bizID,name=bizName,stars=bizstars,reviews=bizreviews,casinodf)
     
     bigcasinodf <<- rbind(bigcasinodf,df2)
     
   }
  
    
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

attribute_words <- function(){
  
  att_words <<- as.data.frame(rbind(ambiance=v_ambiance,
                                   good_for=v_good4,
                                   music = v_music,
                                   parking = v_parking,
                                   payment = v_payment,deparse.level = 0))
  
}

association_words <- function(){
  
 unique(fitdf$name_col)
  
  
}


att_summary <- function(){
  
  df <- summarize(group_by(bigcasinodf,name_col),stars = mean(mean_stars),reviews = mean(reviews), corr = mean(mean_cor))
  
  df <- df[order(-df$corr), ]
  
  df[1:10, ]
  
}
