### Mark Stephens
### Data Science Certification Capstone Project
### October 31, 2015

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
  
  casino_chkins()
  
}

### Remove unneeded data frames

rm_yelp_df <- function(){
  
  rm(list = c('bizdf','chkindf','revdf','tipdf','userdf'),envir = as.environment(.GlobalEnv))
  
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

casino_chkins <-function(){
  
  cchkindf <<- chkindf[chkindf$business_id %in% attdf$business_id, ]
  
}


