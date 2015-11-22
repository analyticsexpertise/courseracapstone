### Quiz 1
### Capstone Project Quiz
### Exploring the data

### Question 3,6,8 
### How many lines of text are there in the [filename] file?

require(dplyr)

filelines <- function(filename){
  
  length(readLines(filename))
  
}

### Question 4,9
### Find requested information on specified line in the file 

line100 <- function(filename,linenumber){
  
 linedata <<- readLines(con=filename,n=linenumber)
 
 linedata[linenumber]
  
}

### Question 5
### What % of the reviews are five star reviews?
star5 <- function(){
  
  prop.table(summarise(group_by(revdf,stars),length(stars)))
  
}

### Question 7
### Conditional on having a response for the attribute Wi-Fi what % business have free wi-fi
havewifi <- function(){
  
  wifi <<- as.data.frame(cbind(bizdf$business_id,bizdf$attributes$`Wi-Fi`))
  colnames(wifi)<<-c("business_id","wifi")
  
  wifi_answer <<- wifi[is.na(wifi$wifi)==FALSE, ]
  
  wifi_free <<- wifi_answer[tolower(wifi_answer$wifi)=="free", ]
    
  length(wifi_free[ ,"business_id"])/length(wifi_answer[ ,"business_id"])
}


### Question 10 
### What is the name of the user with over 10000 compliment votes of type "funny"?
funnyuser <- function(){
  
  funnydf <<- as.data.frame(cbind(userdf$name,userdf$votes$funny,userdf$compliments$funny))
  colnames(funnydf)<<-c("name","votes_funny","comp_funny")
  
  funnydf$comp_funny <<- as.numeric(funnydf$comp_funny)
  
  funnydf <<- funnydf[is.na(funnydf$comp_funny)==FALSE, ]
  
  ## funnydf$name <<- tolower(funnydf$name)
  
  funnydf$name <<- as.character(funnydf$name)
  
  funnysum <<- summarise(group_by(funnydf,name),sum(comp_funny))
  
  funnyusers <<- funnysum[funnysum$`sum(comp_funny)`>=10000, ]
  
  funnyusers[funnyusers$name %in% c("Brian","Jeff"), ]
  
}


### Question 11
### Create a 2x2 cross tabulation table of when a user has more than 1 fan to if the user has 
### more than 1 compliment vote of type "funny". Treat missing values a 0 (fans or votes of that type).
### Pass the 2 by 2 table to fisher.test in R. 
### What is the P-value for the test of independence?

fans <- function(){
  
  funnydf <<- as.data.frame(cbind(userdf$name,userdf$compliments$funny,userdf$fans))
  colnames(funnydf) <<- c("name","votes","fans")
  
  
  funnydf$name <<- as.character(funnydf$name)
  
  funnydf$votes <<- as.numeric(funnydf$votes)
  
  funnydf$fans <<- as.numeric(funnydf$fans)
  
  funnydf[is.na(funnydf)] <<- as.numeric(0)
  
  r1c1 <<- nrow(funnydf[funnydf$fans<1 & funnydf$votes<1, ]) ## no fans, not funny
  r2c1 <<- nrow(funnydf[funnydf$fans<1 & funnydf$votes>1, ]) ## no fans, funny
  r1c2 <<- nrow(funnydf[funnydf$fans>1 & funnydf$votes<1, ]) ## yes fans, not funny
  r2c2 <<- nrow(funnydf[funnydf$fans>1 & funnydf$votes>1, ]) ## no fans, funny
  
  ftestmax <<- matrix(c(r1c1,r2c1,r1c2,r2c2),nrow=2,
                      dimnames = list(Funny = c("No", "Yes"), 
                                      Fans = c("No", "Yes")))

  fisher.test(ftestmax)
  
  
}


