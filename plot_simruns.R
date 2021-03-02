rm(list=ls())
#library(FLCore)
#library(FLasher)
#library(plyr)
detach(package:reshape) # might need this if not restarted R so reshape2 can be used properly
library(reshape2)

library(tidyr)

setwd("C:/Users/LukeB/Documents/sim_sbar")
source("plot_funs.R")

load("res_5iters_allsce.Rdata")
#quick sanity check that the right number of iters are being stored
dim(tmpres[[1]]$stk_c$mwts)
dim(tmpres[[1]]$stk_c$schnualt)

#i<-tmpres[[17]]
ls()

#dim(sce)[1]
bigdat_ls<-lapply(tmpres[1:dim(sce)[1]],function(i){
  
  
  tt<-lapply(i[3:5],res_extract)
 dat<- dplyr::bind_rows(tt, .id = "HD")
 dat$sce_id <- i$sce_id
 #dat$scenario <- as.character(i$scenario)
 
return(dat)
})

bigdat<-dplyr::bind_rows(bigdat_ls)
head(bigdat)
#====================================================
#currently we have the 96 scenarios from sce dataframe and the three Harvest Dynamics (HD) for each scenario. 96*3 = 288 scenarios. 288* 5 iters = 1440

f_dat<-merge(bigdat, sce, by = "sce_id" , by.y =0)
rm(list=c("bigdat","bigdat_ls","tmpres"))# remove so free up space
ls()
head(f_dat)
str(f_dat)
f_dat$HD<-factor(f_dat$HD)

#checks
length(levels(factor(f_dat$sce_id)))
length(unique(with(f_dat, interaction(sce_id, HD, drop=TRUE))))   # gives correct number of 288
length(unique(with(f_dat, interaction(sce_id, HD,iter, drop=TRUE))))   # gives correct number 1440

####=====lets deal with non convergence etc. first==============
length(which(is.na(f_dat$value))) ##plenty of rows
na_f_dat<-f_dat[which(is.na(f_dat$value)),] #dataframe of just nas
length(unique(with(na_f_dat, interaction(sce_id, HD, iter,type, drop=TRUE))))  #1646 assessment runs have no results, that's more than a third
length(unique(with(na_f_dat, interaction(sce_id, HD,iter, drop=TRUE)))) #955 iters that have some nas 
length(unique(with(na_f_dat, interaction(sce_id, HD, drop=TRUE)))) #across 207 of the 288 scenarios

dim(na_f_dat)
head(na_f_dat[,])
na_f_dat<-na_f_dat[,c(-3,-5:-6)] #remove columns of no interest
na_f_dat<-na_f_dat[!duplicated(na_f_dat),] #remove duplciated rows
dim(na_f_dat) # 1646 assessment runs
head(na_f_dat[,])

#need a better way to summarise....TO DO
tapply(na_f_dat$iter,na_f_dat[,c("sce_id","HD")],length)
tapply(na_f_dat$iter,na_f_dat[,c("sce_id","type")],length)

#====================================================================
#Comparison plots
library(ggplot2)
#f_dat<-f_dat[which(!is.na(f_dat$value)),]

##comparing NM, LH and HD
tmpdat<-f_dat[f_dat$var == "stk_no"  & f_dat$sel == "kn0" & f_dat$ar == "nocor" & f_dat$sr =="recsd0.1" & f_dat$ts =="long" ,]

p<-ggplot(tmpdat,aes(x=year,y=value, col= factor(type))) + geom_point(alpha = 0.1) + facet_grid(lh+HD~nm, scales = "free_y",labeller=label_both) #
p+stat_summary(aes(group=type,col=factor(type)), fun=median, geom="line")

##comparing sel, LH and HD
tmpdat<-f_dat[f_dat$var == "stk_no"  & f_dat$nm =="cons" & f_dat$ar == "nocor" & f_dat$sr =="recsd0.1" & f_dat$ts =="long" ,]

p<-ggplot(tmpdat,aes(x=year,y=value, col= factor(type))) + geom_point(alpha = 0.1) + facet_grid(lh+HD~sel, scales = "free_y",labeller=label_both) #
p+stat_summary(aes(group=type,col=factor(type)), fun=median, geom="line")



##comparing sel, LH and HD of short time series
tmpdat<-f_dat[f_dat$var == "stk_no"  & f_dat$nm =="cons" & f_dat$ar == "nocor" & f_dat$sr =="recsd0.1" & f_dat$ts =="short" ,]

p<-ggplot(tmpdat,aes(x=year,y=value, col= factor(type))) + geom_point(alpha = 0.1) + facet_grid(lh+HD~sel, scales = "free_y",labeller=label_both) #
p+stat_summary(aes(group=type,col=factor(type)), fun=median, geom="line")



##comparing sel, LH and HD short times series----just looking at stocks
tmpdat<-f_dat[f_dat$var == "stk_no" & f_dat$type =="real values" & f_dat$nm =="cons" & f_dat$ar == "nocor" & f_dat$sr =="recsd0.1" & f_dat$ts =="short" ,]

p<-ggplot(tmpdat,aes(x=year,y=value, col= factor(type))) + geom_point(alpha = 0.1) + facet_grid(lh+HD~sel, scales = "free_y",labeller=label_both) #
p+stat_summary(aes(group=type,col=factor(type)), fun=median, geom="line")


##comparing sel, LH and HD short times series----just looking at stocks
tmpdat<-f_dat[f_dat$var == "stk_no" & f_dat$type =="real values" & f_dat$nm =="cons" &f_dat$HD =="stk_rc" & f_dat$ar == "nocor" & f_dat$sr =="recsd0.1"  ,]

p<-ggplot(tmpdat,aes(x=year,y=value, col= factor(type))) + geom_point(alpha = 0.1) + facet_grid(lh+ts~sel, scales = "free_y",labeller=label_both) #
p+stat_summary(aes(group=type,col=factor(type)), fun=median, geom="line")

