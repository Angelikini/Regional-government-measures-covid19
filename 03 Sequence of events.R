###Relationship of countries in terms of time the measures were adopted?

##Libraries
library(ggplot2)
library(xlsx)


##Task 1: Timeline of when the first measure was adopted

  datesorted<-df[order(df$date_implemented),] #sort gm data based on date and save in a new object
  datesortedu<- datesorted[match(unique(datesorted$country), datesorted$country),] #I kept only the first measure adoption for each country
  datesortedu[!is.na(datesortedu$date_implemented), ]  #I decided to remove the rows with NA in the date_implemented. Alternatively I could have used the date of entry as a proxy.

  #Assign colours to regions
  region_range <- c("Asia", "Europe", "Americas", "Africa","Middle east","Pacific")
  region_colors <- c("#d11141", "#00b159","#00aedb","#f37735","#ffc425", "#000000")
  datesortedu$region <- factor(datesortedu$region, levels=region_range, ordered=TRUE)
  
  #Assign positions and direction for the plot so measures occuring at the same date won't clash
  datesortedu$direction<-rep(c(1,-1),length.out=length(datesortedu$date_implemented))
  datesortedu$positions<-rep(c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5,),length.out=length(datesortedu$date_implemented))
  text_offset <- 0.05
  datesortedu$date_count <- ave(datesortedu$date_implemented==datesortedu$date_implemented, datesortedu$date_implemented, FUN=cumsum)
  datesortedu$text_position <- (datesortedu$date_count * text_offset * datesortedu$direction) + datesortedu$positions
  
  
  #create a new framework with the months
  month_date_range <- seq(min(datesortedu$date_implemented), max(datesortedu$date_implemented), by='month')
  month_format <- format(month_date_range, '%b')
  month_df <- data.frame(month_date_range, month_format)
  
  day_date_range <- seq(min(datesortedu$date_implemented), max(datesortedu$date_implemented), by='day')
  day_format <- format(day_date_range, '%d')
  day_df <- data.frame(day_date_range, day_format)
  
  #create the plot
  timeline_plot<-ggplot(datesortedu,aes(x=date_implemented,y=0, col=region, label=country))
  timeline_plot+labs(col="Countries")+
  scale_color_manual(values=region_colors, labels=region_range, drop = FALSE)+
  theme_classic()+
  geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
  geom_segment(data=datesortedu[datesortedu$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
  
  ## Save the dataset in excel
  write.xlsx(datesortedu,"~/GitHub/covid-data-ODI-submittion/firstmeasurebycountry.xlsx")
  
    
  #Create a table with first measure by type
  firstmeasuretype<-unique(datesortedu$category)
  
  
##Task 2: Sequence of measures

  #Useful resources
    #https://stackoverflow.com/questions/52170556/finding-the-order-and-sequence-of-events-in-r
    
    