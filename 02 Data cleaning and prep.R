#datacleaning

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

##Task 1: Clean the measures dataset

  ###checking if data type is right
  str(measures)

  ###Change date columns to type "Date"
  measures$date_implemented <- as.Date(measures$date_implemented, "%Y-%m-%d")
  measures$date_implemented[18995]<-"2020-08-13" #correct wrong date
  measures$entry_date<- as.Date(measures$entry_date,"%Y-%m-%d")
  str(measures)

  ###Change lists to characters
  measures$category<- as.character(measures$category)
  measures$region<- as.character(measures$region)
  measures$country<- as.character(measures$country)
  str(measures)
  
##Task 2: Clean the cases dataset
  
  ###Keep only the 2020 data
  cases<-cases[,1:349]
  
  ###Add the ISO3 country codes
  names(cases)[2]<-"country" #rename column
  names(ISO3)[8]<-"country" #rename column
  cases$ISO3<-with(ISO3, iso3[match(cases$country, country)]) #add ISO3 column
  cases<-cases[,c(1:2,350,3:349)] #reorder columns
  
##Task 3:Clean the deaths dataset
  
  ###Keep only the 2020 data
  deaths<-deaths[,1:349]
  
  ###Add the ISO3 country codes
  names(deaths)[2]<-"country" #rename column
  deaths$ISO3<-with(ISO3, iso3[match(deaths$country, country)]) #add ISO3 column
  deaths<-deaths[,c(1:2,350,3:349)] #reorder columns
  
#Clean the environment
rm(list=setdiff(ls(), c("measures","cases","deaths")))