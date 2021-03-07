#Sequence of measures

##Useful resources
#https://stackoverflow.com/questions/52170556/finding-the-order-and-sequence-of-events-in-r
#https://medium.com/event-sequence-analysis/case-study-on-event-sequence-analysis-c99a0c52b3d2

##Install packages

if(!require(TraMineR)){
  install.packages("TraMineR")
  library(TraMineR)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}

if(!require(zoo)){
  install.packages("zoo")
  library(zoo)
}

## Tranform the data in the wide format
m<-measures[,c(12,13,9:11,3,8,14)]#subset data
m<-m[!is.na(m$date_implemented), ]#remove rows where date implemented is missing
category<-pivot_wider(m,id_cols = c("country","iso","region","regionalbodies"),names_from=date_implemented,values_from=category,names_sort = TRUE)#category dataset
measure<-pivot_wider(m,id_cols = c("country","iso","region","regionalbodies"),names_from=date_implemented,values_from=measure,names_sort = TRUE,values_fill = 1)#measure dataset

##Measure
###Extract sequences
is.na(measure) <- measure == "NULL"

mseq<-seqconc(measure,5:345)