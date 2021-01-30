#datacleaning

#checking if data type is right
str(df)

##Change date columns to type "Date"
df$date_implemented <- as.Date(df$date_implemented, "%Y-%m-%d")
df$date_implemented[18995]<-"2020-08-13"
df$entry_date<- as.Date(df$entry_date,"%Y-%m-%d")
str(df)

#Change lists to characters
df$category<- as.character(df$category)
df$region<- as.character(df$region)
df$country<- as.character(df$country)
str(df)