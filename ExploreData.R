### Data Preparation

require(jsonlite)

require(dplyr)

require(ggplot2)

require(splitstackshape)

require(data.table)

#########################################################################################################
### CREATE DATA STRUCTURE
#########################################################################################################

### GET YELP DATA

## set working directory to data files location

get_yelp <- function(){
## read data from JSON and convert to data frame

## businesses
bizdf <<- stream_in(file("yelp_academic_dataset_business.json"))

## check ins
chkindf <<- stream_in(file("yelp_academic_dataset_checkin.json"))

## reviews
revdf <<- stream_in(file("yelp_academic_dataset_review.json"))

## tips
tipdf <<- stream_in(file("yelp_academic_dataset_tip.json"))

## users
userdf <<- stream_in(file("yelp_academic_dataset_userdf.json"))

}

### Create Data Structure

create_structure <- function(){
  
  
  bycat()
  
  byatt()
  
  unlist_att()
  
  casino_revs()
  
  casino_users()
  
  casino_tips()
  
  
}

### Remove unneeded data frames

rm_yelp_df <- function(){
  
  rm(bizdf)
  rm(chkindf)
  rm(revdf)
  rm(tipdf)
  rm(userdf)
  
  
}

########################################################################################################
### DATA STRUCTURE
########################################################################################################

bycat <- function(state="NV",city="Las Vegas"){
  
  df <- bizdf[tolower(bizdf$state)==tolower(state) & tolower(bizdf$city)==tolower(city) 
              ,c("business_id","name","categories","review_count", "stars")]
  
  df <- cSplit(indt = df, splitCols = "categories", sep = ",", direction = "long")
  
  
  df$categories <- gsub("c(","",df$categories,fixed=TRUE)
  
  df$categories <- gsub(")","",df$categories,fixed=TRUE)
  
  df$categories <- gsub("\"","",df$categories,fixed=TRUE)
  
  bizcat <<- df[tolower(df$categories)=="casinos", ]
  
  
}

byatt <- function(state="NV",city="Las Vegas"){
  
  df <- bizdf[tolower(bizdf$state)==tolower(state) & tolower(bizdf$city)==tolower(city) 
                     , c("business_id","name","attributes","review_count", "stars")]
  
  bizatt <<- df[df$business_id %in% bizcat$business_id,  ]
  
  }





unlist_att <- function(){
  
  df1 <- bind_cols(bizatt$attributes$Ambience,
                       bizatt$attributes$`Good For`,
                       bizatt$attributes$Parking,
                       bizatt$attributes$Music,
                       bizatt$attributes$`Hair Types Specialized In`,
                       bizatt$attributes$`Payment Types`,
                       bizatt$attributes$`Dietary Restrictions`
                       )
  
  df2 <- cbind(business_id = bizatt$business_id,
                   name = (bizatt$name),
                   review_count = bizatt$review_count,
                   stars = bizatt$stars,
                   appt_only = bizatt$attributes$`By Appointment Only`,
                   happy_hour = bizatt$attributes$`Happy Hour`,
                   accepts_credit_cards = bizatt$attributes$`Accepts Credit Cards`,
                   good_for_groups = bizatt$attributes$`Good For Groups`,
                   outdoor_seating = bizatt$attributes$`Outdoor Seating`,
                   price_range = bizatt$attributes$`Price Range`,
                   good_for_kids = bizatt$attributes$`Good for Kids`,
                   alcohol = bizatt$attributes$Alcohol,
                   noise_level = bizatt$attributes$`Noise Level`,
                   has_tv = bizatt$attributes$`Has TV`,
                   attire = bizatt$attributes$Attire,
                   good_for_dancing = bizatt$attributes$`Good For Dancing`,
                   delivery = bizatt$attributes$Delivery,
                   coat_check = bizatt$attributes$`Coat Check`,
                   smoking = bizatt$attributes$Smoking,
                   take_out = bizatt$attributes$`Take-out`,
                   takes_reservations = bizatt$attributes$`Takes Reservations`,
                   waiter_service = bizatt$attributes$`Waiter Service`,
                   wifi = bizatt$attributes$`Wi-Fi`,
                   caters = bizatt$attributes$Caters,
                   drive_thru = bizatt$attributes$`Drive-Thru`,
                   wheelchair_access = bizatt$attributes$`Wheelchair Accessible`,
                   byob = bizatt$attributes$BYOB,
                   corkage = bizatt$attributes$Corkage,
                   byobcorkage = bizatt$attributes$`BYOB/Corkage`,
                   order_counter = bizatt$attributes$`Order at Counter`,
                   Good_For_Kids = bizatt$attributes$`Good For Kids`,
                   dogs_allowed = bizatt$attributes$`Dogs Allowed`,
                   open_24_hrs = bizatt$attributes$`Open 24 Hours`,
                   accepts_insurance = bizatt$attributes$`Accepts Insurance`,
                   ages_allowed = bizatt$attributes$`Ages Allowed`
                   )
  
    df3 <- cbind(df2,df1)
    
    ## http://stackoverflow.com/questions/8923754/dataframe-has-lists-how-to-convert-to-columns
    attdf <<- do.call(data.frame, 
            lapply( df3, 
                    function (u) unlist( 
                      if( is.list(u) ) lapply(u, function(v) c(v,NA)[1]) 
                      else u 
                    )))
  
}






### create Casino Reviews data frame
casino_revs <- function(){
  
  crevdf <<- revdf[revdf$business_id %in% attdf$business_id, ]

}


casino_users <- function(){
  
 cuserdf <<- userdf[userdf$user_id %in% crevdf$user_id, ]
  
}

casino_tips <- function(){
  
  ctipdf <<- tipdf[tipdf$business_id %in% attdf$business_id, ]
  
}




####################################################################################################
### EXPLORATION ONLY
####################################################################################################

bizbycity <- function(state="NV"){
  
  citydf <- bizdf[bizdf$state==state, ]
  
  citydf <- summarise(group_by(citydf,city), reviews=sum(review_count))
  
  g <- ggplot(citydf,aes(x = factor(city),y=reviews)) + geom_bar(stat = "identity")
  
  g <- g + theme(axis.text.x =
                   element_text(size  = 10,
                                angle = 45,
                                hjust = 1,
                                vjust = 1))
  g
  
}

bizbyid <- function(state="NV",city="Las Vegas"){
  
  citydf <- bizdf[bizdf$state==state & bizdf$city==city, ]
  
  iddf <- summarise(group_by(citydf,name),reviews=sum(review_count))
  
  g <- ggplot(iddf,aes(x = factor(name),y=reviews)) + geom_bar(stat = "identity")
  
  g
}


bizbycat <- function(state="NV",city="Las Vegas"){
  
  df <- bizdf[tolower(bizdf$state)==tolower(state) & tolower(bizdf$city)==tolower(city) 
              ,c("business_id","name","categories","review_count", "stars")]
  
  df <- cSplit(indt = df, splitCols = "categories", sep = ",", direction = "long")
  
  
  df$categories <- gsub("c(","",df$categories,fixed=TRUE)
  
  df$categories <- gsub(")","",df$categories,fixed=TRUE)
  
  df$categories <- gsub("\"","",df$categories,fixed=TRUE)
  
  
  
  iddf <- summarise(group_by(df,categories),reviews=sum(review_count))
  
  iddf <- iddf[order(-reviews), ]
  
  iddf <- iddf[1:10, ]
  
  g <- ggplot(iddf,aes(x = factor(categories),y=reviews)) + geom_bar(stat = "identity")
  
  print(g)
  
  return(df[tolower(df$categories)=="casinos", ])
  
  
}

viewdata <- function(state="NV",city="Las Vegas"){
  
  df <- bizdf[tolower(bizdf$state)==tolower(state) & tolower(bizdf$city)==tolower(city) 
              ,c("business_id","name","state","city","categories","review_count")]
  
  df <- cSplit(indt = df, splitCols = "categories", sep = ",", direction = "long")
  
  
}

cleancats <- function(state="NV",city="Las Vegas"){
  
  df <- bizdf[tolower(bizdf$state)==tolower(state) & tolower(bizdf$city)==tolower(city) 
              ,c("name","state","city","categories","review_count")]
  
  df <- cSplit(indt = df, splitCols = "categories", sep = ",", direction = "wide")
  
  df$categories_01 <- gsub("c(","",df$categories_01,fixed=TRUE)
  
  df$categories_01 <- gsub(")","",df$categories_01,fixed=TRUE)
  
  df$categories_02 <- gsub("c(","",df$categories_02,fixed=TRUE)
  
  df$categories_02 <- gsub(")","",df$categories_02,fixed=TRUE)
  
  df$categories_03 <- gsub("c(","",df$categories_03,fixed=TRUE)
  
  df$categories_03 <- gsub(")","",df$categories_03,fixed=TRUE)
  
  df$categories_04 <- gsub("c(","",df$categories_04,fixed=TRUE)
  
  df$categories_04 <- gsub(")","",df$categories_04,fixed=TRUE)
  
  df$categories_05 <- gsub("c(","",df$categories_05,fixed=TRUE)
  
  df$categories_05 <- gsub(")","",df$categories_05,fixed=TRUE)
  
  df$categories_06 <- gsub("c(","",df$categories_06,fixed=TRUE)
  
  df$categories_06 <- gsub(")","",df$categories_06,fixed=TRUE)
  
  df$categories_07 <- gsub("c(","",df$categories_07,fixed=TRUE)
  
  df$categories_07 <- gsub(")","",df$categories_07,fixed=TRUE)
  
  df$categories_08 <- gsub("c(","",df$categories_08,fixed=TRUE)
  
  df$categories_08 <- gsub(")","",df$categories_08,fixed=TRUE)
  
  df$categories_09 <- gsub("c(","",df$categories_09,fixed=TRUE)
  
  df$categories_09 <- gsub(")","",df$categories_09,fixed=TRUE)
  
  df$categories_10 <- gsub("c(","",df$categories_10,fixed=TRUE)
  
  df$categories_10 <- gsub(")","",df$categories_10,fixed=TRUE)
  
  ## Remove Quotes
  
  df$categories_01 <- gsub("\"","",df$categories_01,fixed=TRUE)
  
  df$categories_02 <- gsub("\"","",df$categories_02,fixed=TRUE)
  
  df$categories_03 <- gsub("\"","",df$categories_03,fixed=TRUE)
  
  df$categories_04 <- gsub("\"","",df$categories_04,fixed=TRUE)
  
  df$categories_05 <- gsub("\"","",df$categories_05,fixed=TRUE)
  
  df$categories_06 <- gsub("\"","",df$categories_06,fixed=TRUE)
  
  df$categories_07 <- gsub("\"","",df$categories_07,fixed=TRUE)
  
  df$categories_08 <- gsub("\"","",df$categories_08,fixed=TRUE)
  
  df$categories_09 <- gsub("\"","",df$categories_09,fixed=TRUE)
  
  df$categories_10 <- gsub("\"","",df$categories_10,fixed=TRUE)
  
  return(df)
}

findcats <- function(){
  
  cat_types <<- c(
    
    unique(testdf$categories_01),
    unique(testdf$categories_02),
    unique(testdf$categories_03),
    unique(testdf$categories_04),
    unique(testdf$categories_05),
    unique(testdf$categories_06),
    unique(testdf$categories_07),
    unique(testdf$categories_08),
    unique(testdf$categories_09),
    unique(testdf$categories_10)
  )
  
  cat_types <<- unique(cat_types)
  
  
  
}

att_types <- function(){
  
  seq_vec <- colnames(unique(bizatt$attributes$Ambience))
  
  atttype_table <<- cbind( 
    Ambiance = colnames(unique(bizatt$attributes$Ambience))[seq(seq_vec)],
    Good_For = colnames(unique(bizatt$attributes$`Good For` ))[seq(seq_vec)],
    Parking = colnames(unique(bizatt$attributes$`Parking` ))[seq(seq_vec)],
    Music = colnames(unique(bizatt$attributes$`Music` ))[seq(seq_vec)],
    Hair_Types = colnames(unique(bizatt$attributes$`Hair Types Specialized In` ))[seq(seq_vec)],
    Payment_Types = colnames(unique(bizatt$attributes$`Payment Types` ))[seq(seq_vec)],
    Dietary_Restrict = colnames(unique(bizatt$attributes$`Dietary Restrictions` ))[seq(seq_vec)],
    deparse.level = 0
  )
  
  
  
  
}

starmean <- function(attrib="alcohol"){
  
  summarize(group_by(attdf,attribute = testdf[ ,attrib]),median=median(stars),max=max(stars),min=min(stars))
  
  
}

bizbystate <- function(){
  
  statedf <- summarise(group_by(bizdf,state),reviews=sum(review_count))
  
  
  g <- ggplot(statedf,aes(x = factor(state),y=reviews)) + geom_bar(stat = "identity")
  
  g
  
  
}

