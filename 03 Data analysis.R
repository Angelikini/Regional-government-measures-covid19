#Plots per region

#install libraries
library("ggplot2")

#creating graphs

#number of measures categories per continent
jpeg(file="categories per continent.jpeg")
a<-ggplot(df,aes(region,group=category,fill=region))
a+geom_bar(position="dodge")+facet_grid(.~category)+theme_bw()+labs(y="",x="")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()

#number of measures per continent per categories
jpeg(file="measures per continent and category.jpeg",width=1000,height=2000)
b<-ggplot(df,aes(region,group=measure,fill=region))
b+geom_bar(position="dodge")+facet_grid(rows=vars(measure),cols=vars(category))+theme_bw()+labs(y="",x="")+theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
dev.off()