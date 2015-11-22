### Casino Attribute Analysis
### Data Science Certification Capstone Project
### Mark A. Stephens
### 11/3/2015

require(dplyr)


### Distribution by attribute
distbyatt <- function(att = "appt_only"){
  
  df <- attdf[ ,c("name","stars","review_count",att)]
  
  summarize(group_by(df,df[ ,att]),reviews = sum(review_count),min = min(stars)
            , median = median(stars)
            , max = max(stars)
            
            )
  
  
}

### Create list of review distribtuion for each attribute
distbyatt_loop <- function(){
  
  
  cnames <- colnames(attdf)
  
  tdf <- lapply(cnames,distbyatt)
  
  names(tdf) <- cnames
  
  tdf <- tdf[-c(1,3,4)]
  
  attdistdf <<- do.call(rbind, lapply(tdf, data.frame, stringsAsFactors=FALSE))
  
}




### AMBIANCE = romantic | intimate | classy | hipster | divey | touristy | trendy | upscale | casual | NA

ambianceval <- function(attr="romantic"){
  
  unique(attdf[ ,attr])
  
}

ambiancevals <- function(){
  
  ## cnames <- colnames(attdf)
  
  cnames <- c('romantic','intimate','classy','hipster','divey','touristy','trendy'
                     ,'upscale','casual')
  
  lst <- lapply(cnames,ambianceval)
  
  names(lst) <- cnames
  
  ambvals <<- do.call(rbind, lapply(lst, data.frame, stringsAsFactors=FALSE))
  
}

makeambdf <- function(){
  
  cnames <- c('business_id','review_count','stars','romantic','intimate','classy','hipster','divey','touristy','trendy'
              ,'upscale','casual')
  
  ambdf <<- attdf[ ,cnames]
  
}


