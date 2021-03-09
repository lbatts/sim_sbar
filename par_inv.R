#===================================
# Investigating parameters estimation results of simulation testing
# Author : LB
#Distributed under GPL-3 or later
#Feb 2021
#===================================

rm(list=ls())
#
detach(package:reshape) # might need this if not restarted R so reshape2 can be used properly
library(reshape2)
library(tidyr)
library(dplyr)

setwd("C:/Users/LukeB/Documents/sim_sbar")
source("plot_funs.R")

load("res_10iters_allsce.Rdata")
#i<-tmpres[[24]]
ls()

#dim(sce)[1]
bigdat_ls<-lapply(tmpres[1:dim(sce)[1]],function(i){

  print(i$sce_id)
  tt<-lapply(i[3:5],par_extract)
  dat<- dplyr::bind_rows(tt, .id = "HD")
  dat$sce_id <- i$sce_id
  #dat$scenario <- as.character(i$scenario)

  return(dat)
})

bigdat<-dplyr::bind_rows(bigdat_ls)
head(bigdat)
#====================================================
#currently we have the 96 scenarios from sce dataframe and the three Harvest Dynamics (HD) for each scenario. 96*3 = 288 scenarios. 288* 5 iters = 2880
head(sce)
f_dat<-merge(bigdat, sce, by.x  = "sce_id" , by.y =0,all.x=T)

head(bigdat)
subset(f_dat,f_dat$sce_id==24 & f_dat$type=="CSA")
subset(bigdat,bigdat$sce_id==24 & bigdat$type=="CSA")

rm(list=c("bigdat","bigdat_ls","tmpres"))# remove so free up space
ls()
head(f_dat)
str(f_dat)
f_dat$HD<-factor(f_dat$HD)
#f_dat$type<-factor(f_dat$type)

dim(f_dat)
#checks
length(levels(factor(f_dat$sce_id)))
length(unique(with(f_dat, interaction(sce_id, HD, drop=TRUE))))   # gives correct number of 288
length(unique(with(f_dat, interaction(sce_id, HD,iter, drop=TRUE))))   # gives correct number 1440
length(unique(with(f_dat, interaction(sce_id, HD,iter,type, drop=TRUE))))   # gives correct number


####=====lets deal with non convergence etc. first==============
head(f_dat)
f_dat$uniqrun <- with(f_dat[,], interaction(sce_id, HD,iter,type, drop=TRUE))
f_dat$errors[is.na(f_dat$errors) == TRUE] <-"converged"

f_dat[f_dat$uniqrun=="17.stk_ow.3.CSA",]

f_dat$errors[f_dat$var == "q" & f_dat$est <= 1e-10] <- "unrealistic estimates"
levels(factor(f_dat$errors))
f_dat[f_dat$errors == "unrealistic estimates",]
f_dat[470:490,]
f_dat$errors[which(f_dat$uniqrun %in% f_dat$uniqrun[f_dat$errors == "unrealistic estimates"])] <- "unrealistic estimates"

conver<-f_dat[,c(1:3,6,10:17)]
dim(conver)
conver<-conver[!duplicated(conver[,c(12)]),]
levels(factor(conver$type))
subset(conver,conver$sce_id==24 & conver$type=="CSA")



length(unique(with(conver, interaction(sce_id, HD, iter,type, drop=TRUE))))
length(unique(with(conver, interaction(sce_id, HD,iter, drop=TRUE))))
length(unique(with(conver, interaction(sce_id, HD, drop=TRUE))))
conver %>%
  group_by(type) %>%
  count()


tmp<-conver %>%
    group_by(errors,type) %>%
  count
sum(tmp$n)


tmp<-conver %>%
  filter(type=="CSA") %>%
  group_by(errors,type) %>%
  count
sum(tmp$n)

tmp<-conver %>%
  filter(type=="schnub0") %>%
  group_by(errors,type) %>%
  count
sum(tmp$n)

tmp<-conver %>%
  filter(type=="schnualt") %>%
  group_by(errors,type) %>%
  count

sum(tmp$n)

head(conver)

save(conver, file="conver_relaxq.RData")
table(conver$errors) # mostly "did not converge"
head(conver)
table(conver[,c(1,5)])

conver %>%
  group_by(sel)%>%
  count(errors =="converged")
 #massive issues with selelctivity that is logistic and dome

conver %>%
  group_by(sel,nm)%>%
  count(errors=="converged")


conver %>%
  group_by(sel,ts)%>%
  count(errors=="converged")

conver %>%
  group_by(sel,lh)%>%
  count(errors=="converged")

#Interesting
conver %>%
  group_by(sel,type)%>%
  count(errors=="converged")

