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



### Create review sentiment attribute data frame for a casino
create_casinoassocdf <- function(casino_id=crevdf$business_id[1]){
  
  exploretext(num_stars=1,df=crevdf[crevdf$business_id==casino_id, ])
  
  casinoassocdf <<- assocdf
  
  exploretext(num_stars=2,df=crevdf[crevdf$business_id==casino_id, ])
  
  casinoassocdf <<- rbind(casinoassocdf,assocdf)
  
  exploretext(num_stars=3,df=crevdf[crevdf$business_id==casino_id, ])
  
  casinoassocdf <<- rbind(casinoassocdf,assocdf)
  
  exploretext(num_stars=4,df=crevdf[crevdf$business_id==casino_id, ])
  
  casinoassocdf <<- rbind(casinoassocdf,assocdf)
  
  exploretext(num_stars=5,df=crevdf[crevdf$business_id==casino_id, ])
  
  casinoassocdf <<- rbind(casinoassocdf,assocdf)
  
  casinoassocdf$max_cor <<- as.numeric(casinoassocdf$max_cor)
  casinoassocdf$median_cor <<- as.numeric(casinoassocdf$median_cor)
  casinoassocdf$min_cor <<- as.numeric(casinoassocdf$min_cor)
  casinoassocdf$mean_cor <<- as.numeric(casinoassocdf$mean_cor)
  casinoassocdf$words <<- as.numeric(casinoassocdf$words)
  casinoassocdf$stars <<- as.numeric(casinoassocdf$stars)
  
  casinoassocdf <<- casinoassocdf[order(casinoassocdf$name_col), ]
  
  
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

create_casino_df <- function(){
  
  df <- casinoassocdf[casinoassocdf$name_col %in% revassocdf$name_col, ]
  
  casinodf <<- summarize(group_by(df, name_col), 
                          max_cor=max(max_cor), 
                          mean_cor = mean(mean_cor),
                          min_cor = min(min_cor),
                          max_stars=max(stars),
                          mean_stars = mean(stars),
                          min_stars = min(stars))
  
}
### predict casino star rating based upon attribute sets

make_lm_model <- function(){
  
  fit_lm <<- lm(max_stars ~ .,data=fitdf)
  
}

see_predict_tree <- function(){
  
  predTree <<- rpart(max_stars ~ name_col + min_stars + mean_stars + max_cor + min_cor + mean_cor,
                     data=fitdf, na.action = na.pass)
  
  ## rpart.plot(predTree, type=4, faclen = 0, cex=0.7)
  
}

predict_attr_star <- function(){
  
  df = casinodf[casinodf$name_col %in% fitdf$name_col, ]
  
  fit <<- see_predict_tree()
  
  star_pred <<- predict(fit,newdata=df,na.action=na.pass)
  
  star_table <<- table(star_pred,df[ ,"max_stars"])
}

### create training & test sets combining casino star rating with attribute sets ratings



### from training set create linear regression model to predict casino stars 



### Casino_Star_Rating ~ Ambiance + Good4 + Parking + Music + Payment





################################################### Support Functions ##################################

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
  
  assocdf <<- assocdf[assocdf$words >0, ]
  
  assocdf <<- assocdf[order(assocdf$mean_cor,decreasing=TRUE), ]
  
}


  
