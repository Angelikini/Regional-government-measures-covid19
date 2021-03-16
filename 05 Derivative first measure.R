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
fm$one <- fm$date -1
fm$two <- fm$date -2
fm$three <- fm$date -3
fm$four <- fm$date -4
fm$five <- fm$date -5
fm$six <- fm$date -6
fm$seven <- fm$date -7


names(cs)[names(cs) == "Country.Region"] <- "country"
#names(fm)[names(fm) == "date_implemented"] <- "date"

## merge
dat <- 
  left_join(fm, cs, by=c("country","date")) %>%
  rowwise()

names(cs)[names(cs) == "date"] <- "one"
names(cs)[names(cs) == "case"] <- "case-1"

dat <- 
  left_join(dat, cs, by=c("country","one")) %>%
  rowwise()

names(cs)[names(cs) == "one"] <- "two"
names(cs)[names(cs) == "case-1"] <- "case-2"

dat <- 
  left_join(dat, cs, by=c("country","two")) %>%
  rowwise()

names(cs)[names(cs) == "two"] <- "three"
names(cs)[names(cs) == "case-2"] <- "case-3"

dat <- 
  left_join(dat, cs, by=c("country","three")) %>%
  rowwise()

names(cs)[names(cs) == "three"] <- "four"
names(cs)[names(cs) == "case-3"] <- "case-4"

dat <- 
  left_join(dat, cs, by=c("country","four")) %>%
  rowwise()

names(cs)[names(cs) == "four"] <- "five"
names(cs)[names(cs) == "case-4"] <- "case-5"

dat <- 
  left_join(dat, cs, by=c("country","five")) %>%
  rowwise()

## reordering columns
order <- c("country", "measure", "five", "four", "three","two", "one",
           "date", "case-5", "case-4", "case-3", "case-2","case-1", "case")
dat <- dat[, order]

## slope function

slope  <-  function(x){
    if(all(is.na(x)))
      # if x is all missing, then lm will throw an error that we want to avoid
      return(NA)
    else
      return(coef(lm(I(1:2)~x))[2])
  }


install.packages("pbapply")
library(pbapply)

## reduce dimension
dat <- unique(dat)


dat$slope7 <- pbapply(dat[,c("case-1","case")], 1, slope)
dat$slope6 <- pbapply(dat[,c("case-2","case-1")], 1, slope)
dat$slope5 <- pbapply(dat[,c("case-3","case-2")], 1, slope)
dat$slope4 <- pbapply(dat[,c("case-4","case-3")], 1, slope)
dat$slope3 <- pbapply(dat[,c("case-5","case-4")], 1, slope)


dat$slope <- rowMeans(dat[,c("slope3", "slope4", "slope5", 
                        "slope6", "slope7")], na.rm = TRUE)

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

clusters <- as.data.frame(res$cluster)
clusters

write.csv(clusters,"~/Programming/00_github/covid-data-ODI-submission/clusters.csv")

