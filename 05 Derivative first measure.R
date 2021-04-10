## get necessary package

if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

if(!require(pbapply)){
  install.packages("pbapply")
  library(pbapply)
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

if(!require(DescTools)){
  install.packages("DescTools")
  library(DescTools)           
}

if(!require(klaR)){
  install.packages("klaR")
  library(klarR)           
}


## load object first measure
firstmeasure

## take necessary columns
fm <- subset(firstmeasure, select = c("country", "category", "date_implemented", "region"))

## unlist country column
fm$country <- unlist(fm$country)

## convert date
fm$date_implemented <- ymd(fm$date_implemented)

## load cases
cases

##take necessary columns
cs <- subset(cases, select = -c(Province.State, Lat, Long))

## aggregate per country
cs <- cs %>%
  group_by(Country.Region) %>%
  summarise_each(list(sum))

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

## merge and prepare data for the slope analysis
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

names(cs)[names(cs) == "five"] <- "six"
names(cs)[names(cs) == "case-5"] <- "case-6"

dat <- 
  left_join(dat, cs, by=c("country","six")) %>%
  rowwise()

names(cs)[names(cs) == "six"] <- "seven"
names(cs)[names(cs) == "case-6"] <- "case-7"

dat <- 
  left_join(dat, cs, by=c("country","seven")) %>%
  rowwise()


## reordering columns
order <- c("country", "region", "category", "seven", "six", "five", "four", "three","two", "one",
           "date", "case-7", "case-6", "case-5", "case-4", "case-3", "case-2","case-1", "case")

dat <- dat[, order]
dat[is.na(dat)] <- 0

## slope function / slope coefficient of lm()
slope  <-  function(x){
    if(all(is.na(x)))
      # if x is all missing, then lm will throw an error that we want to avoid
      return(NA)
    else
      return(coef(lm(I(1:2)~x))[2])
}

## running slope function on the 7 days before first measure
dat$slope7 <- pbapply(dat[,c("case-1","case")], 1, slope)
dat$slope6 <- pbapply(dat[,c("case-2","case-1")], 1, slope)
dat$slope5 <- pbapply(dat[,c("case-3","case-2")], 1, slope)
dat$slope4 <- pbapply(dat[,c("case-4","case-3")], 1, slope)
dat$slope3 <- pbapply(dat[,c("case-5","case-4")], 1, slope)
dat$slope2 <- pbapply(dat[,c("case-6","case-5")], 1, slope)
dat$slope1 <- pbapply(dat[,c("case-7","case-6")], 1, slope)

## getting the mean of slope values
dat[is.na(dat)] <- 0
dat$slope <- rowMeans(dat[,c("slope1", "slope2", "slope3", 
                        "slope4", "slope5", "slope6", "slope7")], na.rm = TRUE)

## preparing the dataframe for analysis
root <- subset(dat, select = c("country", "region","category", "slope"))
root <- root %>% drop_na()
root <- root %>% distinct()
root <- as.data.table(root)

## quantifying the measures' categories in hierarchical order
root$category <- sapply (root$category, function(x) if(x == "Governance and socio-economic measures") {
  1
} else if (x == "Public health measures") {
  2    
} else if (x == "Social distancing") {                        
  3  
} else if (x == "Movement restrictions") {
  4  
} else if (x == "Lockdown") { 
  5  
} else {
  0
})


df <- subset(root, select = c("category", "slope"))

df <- as.data.frame(df)
row.names(df) <- root$country

## launching cluster algorithm
df <- scale(df)

set.seed(123)

res <- kmeans(df, 5)

# tiff("clusters.tiff", width=2800, height=2000, res = 600)
# png("clusters.png", width=1400, height= 1000, res = 300)

fviz_cluster(res, df, geom = "point",
             repel = TRUE,
             show.clust.cent = FALSE,
             ellipse = TRUE,
             ellipse.type = "norm",
             ellipse.level = 0.90,
             ellipse.alpha = 0.1,
             shape = NULL,
             pointsize = 1.5,
             labelsize = 12,
             main = "Cluster plot",
             xlab = NULL,
             ylab = NULL)

## dev.off()

## cross checking clustering with dataset values
clusters <- as.data.frame(res$cluster)
names(clusters)[names(clusters) == "res$cluster"] <- "cluster"
clusters$label <- root$region
clusters$slope <- root$slope
clusters$category <- root$category


clusters

test <- clusters %>%
  group_by(label)

europe <- test[test$label == "Europe",]
asia <- test[test$label == "Asia",]
middle_east <- test[test$label == "Middle east",]
africa <- test[test$label == "Africa",]
americas <- test[test$label == "Americas",]

## Gini coefficient for continent "homogeneity"
eu <- Gini(europe$cluster)
as <- Gini(asia$cluster)
me <- Gini(middle_east$cluster)
afr <- Gini(africa$cluster)
am <- Gini(americas$cluster)

continents <- c("eu", "asia", "me", "africa", "americas")
value <- c(eu, as, me, afr, am)

results <- data.frame(continents, value)

results

