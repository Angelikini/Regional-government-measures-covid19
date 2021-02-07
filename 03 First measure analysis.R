#Relationship of countries in terms of time the measures were adopted?

#packages
library(ggplot2)
library(xlsx)
library(tidyverse)

##Task 1: Timeline of when the first measure was adopted

  datesorted<-measures[order(measures$date_implemented),] #sort gm data based on date and save in a new object
  firstmeasure<- datesorted[match(unique(datesorted$country), datesorted$country),] #I kept only the first measure adoption for each country
  firstmeasure[!is.na(firstmeasure$date_implemented), ]  #I decided to remove the rows with NA in the date_implemented. Alternatively I could have used the date of entry as a proxy.

  ###Assign colours to regions
  region_range <- c("Asia", "Europe", "Americas", "Africa","Middle east","Pacific")
  region_colors <- c("#d11141", "#00b159","#00aedb","#f37735","#ffc425", "#000000")
  firstmeasure$region <- factor(firstmeasure$region, levels=region_range, ordered=TRUE)
  
  ###Assign positions and direction for the plot so measures occuring at the same date won't clash
  firstmeasure<-firstmeasure %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1))
  firstmeasure$positions <- ave(firstmeasure$direction, cumsum(c(0, diff(firstmeasure$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  text_offset <- 0.05
  firstmeasure$date_count <- ave(firstmeasure$date_implemented==firstmeasure$date_implemented, firstmeasure$date_implemented, FUN=cumsum)
  firstmeasure$text_position <- (firstmeasure$date_count * text_offset * firstmeasure$direction) + firstmeasure$positions
  
  ###create a new framework with the months
  month_date_range <- seq(min(firstmeasure$date_implemented), max(firstmeasure$date_implemented), by='month')
  month_format <- format(month_date_range, '%b')
  month_df <- data.frame(month_date_range, month_format)
  
  day_date_range <- seq(min(firstmeasure$date_implemented), max(firstmeasure$date_implemented), by='day')
  day_format <- format(day_date_range, '%d')
  day_df <- data.frame(day_date_range, day_format)
  
  ###create the plot
  timeline_plot<-ggplot(firstmeasure,aes(x=date_implemented,y=0, col=region, label=country))
  timeline_plot+labs(col="Countries")+
  scale_color_manual(values=region_colors, labels=region_range, drop = FALSE)+
  theme_classic()+
  geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
  geom_segment(data=firstmeasure[firstmeasure$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
  geom_point(aes(y=0), size=3)+ # Plot scatter points at zero and date
  theme(axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x =element_blank(),
        legend.position = "bottom"
  )+ # Don't show axes, appropriately position legend
  geom_text(data=month_df, aes(x=month_date_range,y=-0.5,label=month_format),size=2,vjust=1.5, color='black', angle=0)+ # Show text for each month
  geom_text(data=day_df, aes(x=day_date_range,y=-0.2,label=day_format),size=2,vjust=1.5, color='black', angle=0,label.padding=1)+ # Show text for each day
  geom_text(aes(y=text_position,label=country),size=2.5)+ # Show text for each milestone
  ggsave(filename="First measure timeline.png",width=17)
  
  ### Save the dataset in excel
  write.xlsx(firstmeasure,"~/GitHub/covid-data-ODI-submittion/firstmeasurebycountry.xlsx")
  
    
##Task 2: Create a table with first measure by category
  firstmeasurecat<-firstmeasure$category
  firstmeasurecat<-as.data.frame(table(firstmeasurecat))
  firstmeasurecat<-firstmeasurecat[order(firstmeasurecat$Freq,decreasing = TRUE),]
  write.xlsx(firstmeasurecat,"~/GitHub/covid-data-ODI-submittion/firstmeasurebycategory.xlsx")

##Task3: Create a table with first measure by type of measure
  firstmeasuretype<-firstmeasure$measure
  firstmeasuretype<-as.data.frame(table(firstmeasuretype))
  firstmeasuretype<-firstmeasuretype[order(firstmeasuretype$Freq,decreasing = TRUE),]
  write.xlsx(firstmeasuretype,"~/GitHub/covid-data-ODI-submittion/firstmeasurebytype.xlsx")
  
##Task 4: Identify distance between first case and first value

 ###Create first case per country table
 casesod <- cases[,-(1:5),drop=FALSE]#subset the date columns
 firstcasedate<-names(casesod[-1])[apply(casesod[-1] != 0, 1, which.max)]#pivot the table so that I keep the first date a case was reported for each country
 firstcase<-as.data.frame(firstcasedate)#create the new table
 firstcase$country<-cases$country#add country names
 firstcase$iso<-cases$ISO3#add iso3
 firstcase$province<-cases$Province.State#add region names
 names(firstcase)[1]<-"fcdate"#rename the date column
 firstcase$fcdate <- gsub("^.{0,1}", "", firstcase$date)#remove the X before the date
 firstcase$fcdate<- as.Date(firstcase$date, "%m.%d.%y")#set the date format
 firstcase<-firstcase[-c(9:14,16,40:49,51:55,59:71,73:91,102:103,119:129,191:194,253:262),,drop=FALSE]#keep only the first case per country and remove the regions - any idea how to automate this?
 
 ###Merge first measure and first case tables
 firsts<- merge(firstmeasure, firstcase, by.x = "iso", by.y= "iso")
 firsts<-firsts
 
 # Clean the environment
 #rm(list=setdiff(ls(), c("measures","cases","deaths")))