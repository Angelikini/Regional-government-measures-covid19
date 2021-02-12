## get necessary package

if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

if(!require(dplyr)){
  install.packages("dplyr")
  library(dplyr)           
}

if(!require(tidyr)){
  install.packages("tidyr")
  library(tidyr)           
}

if(!require(stats)){
  install.packages("stats")
  library(stats)           
}

if(!require(factoextra)){
  install.packages("factoextra")
  library(factoextra)           
}

## load object first measure

firstmeasure
## take necessary columns
fm <- subset(firstmeasure, select = c("country", "measure", "date_implemented"))

## unlist country column
fm$country <- unlist(fm$country)

## convert date
fm$date_implemented <- ymd(fm$date_implemented)

## load cases
cases

##take necessary columns
cs <- subset(cases, select = -c(Province.State, Lat, Long))

## reshape
cs <- cs %>% gather(date, case, X1.22.20:X2.11.21)
cs$date <- gsub("X", "0", cs$date)
cs$date <- gsub("\\.", "/", cs$date)
cs$date <- mdy(cs$date)
fm$date <- ymd(fm$date)
fm$daybefore <- fm$date -1

names(cs)[names(cs) == "Country.Region"] <- "country"
names(fm)[names(fm) == "date_implemented"] <- "date"

## merge
dat <- 
  left_join(fm, cs, by=c("country","date")) %>%
  rowwise()

names(cs)[names(cs) == "date"] <- "daybefore"
names(cs)[names(cs) == "case"] <- "case-1"

dat <- 
  left_join(dat, cs, by=c("country","daybefore")) %>%
  rowwise()

## reordering columns
order <- c("country", "measure", "daybefore", "date", "case-1", "case")
dat <- dat[, order]

## slope function

slope  <-  function(x){
    if(all(is.na(x)))
      # if x is all missing, then lm will throw an error that we want to avoid
      return(NA)
    else
      return(coef(lm(I(1:2)~x))[2])
  }

dat$slope <- apply(dat[,c("case-1","case")], 1, slope)

root <- subset(dat, select = c("country", "measure", "slope"))
root <- root %>% drop_na()
root <- root %>% distinct()
root <- as.data.table(root)
root <- root[,.SD[which.max(slope)], by= country]

root$measure <- sapply (root$measure, function(x) if(x == "Health screenings in airports and border crossings") {
    1
  } else if (x == "Emergency administrative structures activated or established") {
    2    
  } else if (x == "Isolation and quarantine policies") {                        
    3  
  } else if (x == "Schools closure") {
    4  
  } else if (x == "Visa restrictions") { 
    5  
  } else if (x == "Economic measures") {
    6  
  } else if (x == "Awareness campaigns") {
    7  
  } else if (x == "Border closure") {
    8  
  } else if (x == "International flights suspension") {                      
    9  
  } else if (x == "Limit public gatherings") {
    10  
  } else if (x == "Border checks") {
    11 
  } else if (x == "Strengthening the public health system") {
   12 
  } else {13
})


## root$slope <- as.numeric(format(round(root$slope, 5), nsmall = 5))

df <- subset(root, select = c("measure", "slope"))

df <- as.data.frame(df)
row.names(df) <- root$country


## launching cluster algorithm

df <- scale(df)

set.seed(123)

res <- kmeans(df, 5)

plot <- fviz_cluster(res, df, geom = "text", repel = TRUE, ellipse = TRUE, 
                     labelsize = 9, outlier.color = "gray",
                     ggtheme = theme_linedraw())

plot
