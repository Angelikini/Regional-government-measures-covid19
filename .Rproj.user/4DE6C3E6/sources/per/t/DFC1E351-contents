#Relationship of countries in terms of time the measures were adopted?

##Load packages
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(xlsx)){
  install.packages("xlsx")
  library(xlsx)
}
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require(lubridate)){
  install.packages("lubridate")
  library(lubridate)
}

##Task 1: Timeline of when the first measure was adopted (global)

  datesorted<-measures[order(measures$date_implemented),] #sort gm data based on date and save in a new object
  firstmeasure<- datesorted[match(unique(datesorted$country), datesorted$country),] #I kept only the first measure adoption for each country
  firstmeasure<-firstmeasure[!is.na(firstmeasure$date_implemented), ]  #I decided to remove the rows with NA in the date_implemented. Alternatively I could have used the date of entry as a proxy.

  ###Assign colours to regions
  region_range <- c("Asia", "Europe", "Americas", "Africa","Middle East","Pacific")
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
    ggsave(filename="First measure timeline.png",width=25)
  
  ### Save the dataset in excel
  write.xlsx(firstmeasure,"~/GitHub/covid-data-ODI-submittion/firstmeasurebycountry.xlsx")
  
##Task 2: Regional timelines
  
  ###Africa
  fmafrica<-subset(firstmeasure, region=="Africa") #subset africa
  africa_range <- c("Western Africa","Northern Africa","Middle Africa","Eastern Africa","Southern Africa") #assign colours to subregion
  africa_colors <- c("#da6b2f","#c25f2a","#aa5325","#91471f","#793b1a")
  fmafrica$subregion <- factor(fmafrica$subregion, levels=africa_range, ordered=TRUE)
  fmafrica<-fmafrica %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmafrica$positions <- ave(fmafrica$direction, cumsum(c(0, diff(fmafrica$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmafrica$date_count <- ave(fmafrica$date_implemented==fmafrica$date_implemented, fmafrica$date_implemented, FUN=cumsum)
  fmafrica$text_position <- (fmafrica$date_count * text_offset * fmafrica$direction) + fmafrica$positions
  timeline_plot_africa<-ggplot(fmafrica,aes(x=date_implemented,y=0, col=subregion, label=country))
  timeline_plot_africa+labs(col="Countries")+
    scale_color_manual(values=africa_colors, labels=africa_range, drop = FALSE)+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmafrica[fmafrica$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure africa timeline.png",width=25)
  
  ###Asia
  fmasia<-subset(firstmeasure, region=="Asia") #subset africa
  asia_range <- c("Eastern Asia","Southeastern Asia","Southern Asia","Central Asia") #assign colours to subregion
  asia_colors <- c("#bc0f3a","#a70d34","#920b2d","#7d0a27")
  fmasia$subregion <- factor(fmasia$subregion, levels=asia_range, ordered=TRUE)
  fmasia<-fmasia %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmasia$positions <- ave(fmasia$direction, cumsum(c(0, diff(fmasia$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmasia$date_count <- ave(fmasia$date_implemented==fmasia$date_implemented, fmasia$date_implemented, FUN=cumsum)
  fmasia$text_position <- (fmasia$date_count * text_offset * fmasia$direction) + fmasia$positions
  timeline_plot_africa<-ggplot(fmasia,aes(x=date_implemented,y=0, col=subregion, label=country))
  timeline_plot_africa+labs(col="Countries")+
    scale_color_manual(values=asia_colors, labels=asia_range, drop = FALSE)+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmasia[fmasia$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure asia timeline.png",width=25)
  
  ###Europe
  fmeurope<-subset(firstmeasure, region=="Europe") #subset africa
  europe_range <- c("Eastern Europe","Northern Europe","Southern Europe","Western Europe") #assign colours to subregion
  europe_colors <- c("#009f50","#008d47","#007b3e","#006a35")
  fmeurope$subregion <- factor(fmeurope$subregion, levels=europe_range, ordered=TRUE)
  fmeurope<-fmeurope %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmeurope$positions <- ave(fmeurope$direction, cumsum(c(0, diff(fmeurope$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmeurope$date_count <- ave(fmeurope$date_implemented==fmeurope$date_implemented, fmeurope$date_implemented, FUN=cumsum)
  fmeurope$text_position <- (fmeurope$date_count * text_offset * fmeurope$direction) + fmeurope$positions
  timeline_plot_africa<-ggplot(fmeurope,aes(x=date_implemented,y=0, col=subregion, label=country))
  timeline_plot_africa+labs(col="Countries")+
    scale_color_manual(values=europe_colors, labels=europe_range, drop = FALSE)+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmeurope[fmeurope$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure europe timeline.png",width=25)
  
  ###America
  fmamerica<-subset(firstmeasure, region=="Americas") #subset africa
  america_range <- c("Central America","North America","South America","Caribbean") #assign colours to subregion
  america_colors <- c("#009cc5","#008baf","#007999","#006883")
  fmamerica$subregion <- factor(fmamerica$subregion, levels=america_range, ordered=TRUE)
  fmamerica<-fmamerica %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmamerica$positions <- ave(fmamerica$direction, cumsum(c(0, diff(fmamerica$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmamerica$date_count <- ave(fmamerica$date_implemented==fmamerica$date_implemented, fmamerica$date_implemented, FUN=cumsum)
  fmamerica$text_position <- (fmamerica$date_count * text_offset * fmamerica$direction) + fmamerica$positions
  timeline_plot_africa<-ggplot(fmamerica,aes(x=date_implemented,y=0, col=subregion, label=country))
  timeline_plot_africa+labs(col="Countries")+
    scale_color_manual(values=america_colors, labels=america_range, drop = FALSE)+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmamerica[fmamerica$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure americas timeline.png",width=25)
  
  ###Middle east
  fmmiddleeast<-subset(firstmeasure, region=="Middle East") #subset africa
  middle_east_range <- c("Western Asia","Southern Asia","Southeastern Asia") #assign colours to subregion
  middle_east_colors <- c("#e5b021","#cc9c1d","#b28919")
  fmmiddleeast$subregion <- factor(fmmiddleeast$subregion, levels=middle_east_range, ordered=TRUE)
  fmmiddleeast<-fmmiddleeast %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmmiddleeast$positions <- ave(fmmiddleeast$direction, cumsum(c(0, diff(fmmiddleeast$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmmiddleeast$date_count <- ave(fmmiddleeast$date_implemented==fmmiddleeast$date_implemented, fmmiddleeast$date_implemented, FUN=cumsum)
  fmmiddleeast$text_position <- (fmmiddleeast$date_count * text_offset * fmmiddleeast$direction) + fmmiddleeast$positions
  timeline_plot_africa<-ggplot(fmmiddleeast,aes(x=date_implemented,y=0, col=subregion, label=country))
  timeline_plot_africa+labs(col="Countries")+
    scale_color_manual(values=middle_east_colors, labels=middle_east_range, drop = FALSE)+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmmiddleeast[fmmiddleeast$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure middle east timeline.png",width=25)
  
  ###Pacific
  fmpacific<-subset(firstmeasure, region=="Pacific") #subset africa
  pacific_range <- c("Micronesia","Polynesia","Melanesia","Australia and New Zealand") #assign colours to subregion
  pacific_colors <- c("#323232","#4c4c4c","#666666","#7f7f7f")
  fmpacific$subregion <- factor(fmpacific$subregion, levels=pacific_range, ordered=TRUE)
  fmpacific<-fmpacific %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmpacific$positions <- ave(fmpacific$direction, cumsum(c(0, diff(fmpacific$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmpacific$date_count <- ave(fmpacific$date_implemented==fmpacific$date_implemented, fmpacific$date_implemented, FUN=cumsum)
  fmpacific$text_position <- (fmpacific$date_count * text_offset * fmpacific$direction) + fmpacific$positions
  timeline_plot_africa<-ggplot(fmpacific,aes(x=date_implemented,y=0, col=subregion, label=country))
  timeline_plot_africa+labs(col="Countries")+
    scale_color_manual(values=pacific_colors, labels=pacific_range, drop = FALSE)+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmpacific[fmpacific$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure pacific timeline.png",width=25)
  
##Task 3: Timeline per regional political body
  
  ###First_measures for regional bodies
  firstmeasure_regionalbodies<-separate_rows(firstmeasure,regionalbodies,sep = ", ")
    
  ###African Union
  fmafricanunion<-subset(firstmeasure_regionalbodies, regionalbodies=="African Union") #subset african Union
  fmafricanunion<-fmafricanunion %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmafricanunion$positions <- ave(fmafricanunion$direction, cumsum(c(0, diff(fmafricanunion$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmafricanunion$date_count <- ave(fmafricanunion$date_implemented==fmafricanunion$date_implemented, fmafricanunion$date_implemented, FUN=cumsum)
  fmafricanunion$text_position <- (fmafricanunion$date_count * text_offset * fmafricanunion$direction) + fmafricanunion$positions
  timeline_plot_african_union<-ggplot(fmafricanunion,aes(x=date_implemented,y=0, label=country))
  timeline_plot_african_union+labs(col="Countries")+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmafricanunion[fmafricanunion$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure african union timeline.png",width=25)
  
  ###Arab League
  fmarableague<-subset(firstmeasure_regionalbodies, regionalbodies=="Arab League") #subset arab league
  fmarableague<-fmarableague %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmarableague$positions <- ave(fmarableague$direction, cumsum(c(0, diff(fmarableague$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmarableague$date_count <- ave(fmarableague$date_implemented==fmarableague$date_implemented, fmarableague$date_implemented, FUN=cumsum)
  fmarableague$text_position <- (fmarableague$date_count * text_offset * fmarableague$direction) + fmarableague$positions
  timeline_plot_arab_league<-ggplot(fmarableague,aes(x=date_implemented,y=0, label=country))
  timeline_plot_arab_league+labs(col="Countries")+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmarableague[fmarableague$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure arab league timeline.png",width=25)
  
  ###ASEAN
  fmasean<-subset(firstmeasure_regionalbodies, regionalbodies=="Association of Southeast Asian Nations (ASEAN)") #subset arab league
  fmasean<-fmasean %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmasean$positions <- ave(fmasean$direction, cumsum(c(0, diff(fmasean$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmasean$date_count <- ave(fmasean$date_implemented==fmasean$date_implemented, fmasean$date_implemented, FUN=cumsum)
  fmasean$text_position <- (fmasean$date_count * text_offset * fmasean$direction) + fmasean$positions
  timeline_plot_asean<-ggplot(fmasean,aes(x=date_implemented,y=0, label=country))
  timeline_plot_asean+labs(col="Countries")+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmasean[fmasean$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure asean timeline.png",width=25)
  
  ###European Union
  fmeu<-subset(firstmeasure_regionalbodies, regionalbodies=="European Union") #subset arab league
  fmeu<-fmeu %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmeu$positions <- ave(fmeu$direction, cumsum(c(0, diff(fmeu$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmeu$date_count <- ave(fmeu$date_implemented==fmeu$date_implemented, fmeu$date_implemented, FUN=cumsum)
  fmeu$text_position <- (fmeu$date_count * text_offset * fmeu$direction) + fmeu$positions
  timeline_plot_eu<-ggplot(fmeu,aes(x=date_implemented,y=0, label=country))
  timeline_plot_eu+labs(col="Countries")+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmeu[fmeu$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure EU timeline.png",width=25)
  
  ###OAS
  fmoas<-subset(firstmeasure_regionalbodies, regionalbodies=="Organization of American States (OAS)") #subset arab league
  fmoas<-fmoas %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmoas$positions <- ave(fmoas$direction, cumsum(c(0, diff(fmoas$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmoas$date_count <- ave(fmoas$date_implemented==fmoas$date_implemented, fmoas$date_implemented, FUN=cumsum)
  fmoas$text_position <- (fmoas$date_count * text_offset * fmoas$direction) + fmoas$positions
  timeline_plot_oas<-ggplot(fmoas,aes(x=date_implemented,y=0, label=country))
  timeline_plot_oas+labs(col="Countries")+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmoas[fmoas$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure OAS timeline.png",width=25)
  
  ###SAARC
  fmsaarc<-subset(firstmeasure_regionalbodies, regionalbodies=="South Asian Association for Regional Cooperation (SAARC)") #subset arab league
  fmsaarc<-fmsaarc %>% mutate(direction = if_else(as.double(str_sub(date_implemented, -1)) %% 2 == 0, -1, 1)) #Assign positions and direction for the plot so measures occuring at the same date won't clash
  fmsaarc$positions <- ave(fmsaarc$direction, cumsum(c(0, diff(fmsaarc$direction)) != 0), FUN = function(x) x*seq(1, by = 0.5, length.out = length(x)))  
  fmsaarc$date_count <- ave(fmsaarc$date_implemented==fmsaarc$date_implemented, fmsaarc$date_implemented, FUN=cumsum)
  fmsaarc$text_position <- (fmsaarc$date_count * text_offset * fmsaarc$direction) + fmsaarc$positions
  timeline_plot_saarc<-ggplot(fmsaarc,aes(x=date_implemented,y=0, label=country))
  timeline_plot_saarc+labs(col="Countries")+
    theme_classic()+
    geom_hline(yintercept=0,color = "black", size=0.3)+ # Plot horizontal black line for timeline
    geom_segment(data=fmsaarc[fmsaarc$date_count == 1,], aes(y=positions,yend=0,xend=date_implemented), color='black', size=0.2)+ # Plot vertical segment lines for countries
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
    ggsave(filename="First measure SAARC timeline.png",width=25)
  
##Task 4: Create a table with first measure by category
  firstmeasurecat<-firstmeasure$category
  firstmeasurecat<-as.data.frame(table(firstmeasurecat))
  firstmeasurecat<-firstmeasurecat[order(firstmeasurecat$Freq,decreasing = TRUE),]
  write.xlsx(firstmeasurecat,"~/GitHub/covid-data-ODI-submittion/firstmeasurebycategory.xlsx")

##Task 5: Create a table with first measure by type of measure
  firstmeasuretype<-firstmeasure$measure
  firstmeasuretype<-as.data.frame(table(firstmeasuretype))
  firstmeasuretype<-firstmeasuretype[order(firstmeasuretype$Freq,decreasing = TRUE),]
  write.xlsx(firstmeasuretype,"~/GitHub/covid-data-ODI-submittion/firstmeasurebytype.xlsx")
  
##Task 6: Identify distance between first case and first measure

 ###Create first case per country table
 casesod <- cases[,-(1:5),drop=FALSE]#subset the date columns
 firstcasedate<-names(casesod[-1])[apply(casesod[-1] != 0, 1, which.max)]#pivot the table so that I keep the first date a case was reported for each country
 firstcase<-as.data.frame(firstcasedate)#create the new table
 firstcase$country<-cases$country#add country names
 firstcase$iso<-cases$ISO3#add iso3
 firstcase$province<-cases$Province.State#add region names
 names(firstcase)[1]<-"fcdate"#rename the date column
 firstcase$fcdate <- gsub("^.{0,1}", "", firstcase$fcdate)#remove the X before the date
 firstcase$fcdate<- as.Date(firstcase$fcdate, "%m.%d.%y")#set the date format
 firstcase<-firstcase[-c(9:14,16,40:49,51:55,59:71,73:91,102:103,119:129,191:194,253:262),,drop=FALSE]#keep only the first case per country and remove the regions - any idea how to automate this?
 
 ###Merge first measure and first case tables
 firsts<- merge(firstmeasure, firstcase, by.x = "iso", by.y= "iso") #merge datasets
 firsts<-firsts[,c(2,11,1,10,4,9,12,22,24)] #keep only necessary columns
 names(firsts)[2]<-"country" #rename column
 names(firsts)[5]<-"date_measure_implemented" #rename column
 names(firsts)[6]<-"measure_category" #rename column
 names(firsts)[8]<-"first_case_date" #rename column
 names(firsts)[9]<-"first_case_province" #rename column
 fmc<-firsts[,c(2,5,8)] #subset dates only
 fmc$difference<-(fmc$date_measure_implemented - fmc$first_case_date) #calculate the difference between date of first case and date of first measure
 
 ###plot histogram
 ggplot(data=fmc, aes(difference)) + 
   geom_histogram()+
   ggsave(filename="Firstcasevsfirstmeasure_histogram.png")

 ###save table
 write.xlsx(fmc,"~/GitHub/covid-data-ODI-submittion/first_measure_first_case_comparison.xlsx")


 
#Clean the environment
 rm(list=setdiff(ls(), c("measures","cases","deaths")))
 
 