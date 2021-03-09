#===================================
# Plotting results of simulation testing
# Author : LB
#Distributed under GPL-3 or later
#Feb 2021
#===================================

rm(list=ls())
#library(FLCore)
#library(FLasher)
#library(plyr)
detach(package:reshape) # might need this if not restarted R so reshape2 can be used properly
library(reshape2)

library(tidyr)

setwd("C:/Users/LukeB/Documents/sim_sbar")
source("plot_funs.R")

load("res_10iters_allsce.Rdata")
#quick sanity check that the right number of iters are being stored
dim(tmpres[[1]]$stk_c$mwts)
dim(tmpres[[1]]$stk_c$schnualt)

#i<-tmpres[[2]]
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
f_dat$uniqrun <- with(f_dat[,], interaction(sce_id, HD,iter,type, drop=TRUE))

#checks
length(levels(factor(f_dat$sce_id)))
length(unique(with(f_dat, interaction(sce_id, HD, drop=TRUE))))   # gives correct number of 288
length(unique(with(f_dat, interaction(sce_id, HD,iter, drop=TRUE))))   # gives correct number 1440 if 5 iters
length(unique(f_dat$uniqrun))#should be previous number times 4 as there are real values as a "type" here
levels(factor(f_dat$type))

f_dat[f_dat$var=="f" & f_dat$type=="Schnute orig" & f_dat$year==1,] <-NA


load("conver_relaxq.RData")
levels(factor(conver$type))
str(conver) #
conver$type[conver$type=="schnualt"] <- levels(factor(f_dat$type))[c(4)]
conver$type[conver$type=="schnub0"] <- levels(factor(f_dat$type))[c(3)]
conver$uniqrun <- with(conver, interaction(sce_id, HD,iter,type, drop=TRUE))

library(xtable)
tab<-conver %>%
  group_by(sel,type)%>%
  count(errors =="converged")

conver %>%
  count(errors =="converged")



colnames(tab)[3]<-"converged"

print(xtable(dcast(tab, sel+ type  ~ converged)),include.rownames=FALSE)

f_dat<-merge(f_dat, conver[,c(4,12)], by = "uniqrun",all.x=T)
dim(f_dat)
f_dat$errors<-ifelse(f_dat$type == "real values","rv",f_dat$errors)

f_dat[700:730,]

plot_dat<- f_dat %>%
  filter(errors %in% c("converged","rv"))

levels(factor(plot_dat$type))
levels(factor(plot_dat$errors))
#====================================================================
#Comparison plots
library(ggplot2)
#f_dat<-f_dat[which(!is.na(f_dat$value)),]
#
# tmp2<-conver %>%
#   filter(sel != "kn0",lh == "mon",ts=="long") %>%
#         group_by(sel, HD,nm) %>%
#   count(errors == "converged")
#
# tmp2

head(plot_dat)
dev.off()
##comparing

for(selh in levels(sce$sel)){
  for(tsh in levels(sce$ts)){
    for(varh in  c("stk_no","f")){

      tmpdat <- plot_dat %>%
  filter(var == varh,sel == selh,ts ==tsh)

      if(varh=="f"){
  ylab <- "fishing mortality"
  }else ylab <- "stock numbers"

#dev.off()
setEPS()
postscript(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/her",tsh,selh,varh,".eps"), horizontal = FALSE, onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)


p<-ggplot(tmpdat[tmpdat$lh=="her",],aes(x=year,y=value, col= factor(type))) + facet_grid(nm+sr~HD+ar, scales = "free_y",labeller=label_both) #
print(p+stat_summary(aes(group=type,col=factor(type)), fun=median, geom="line") + ylab(ylab)+theme_classic())
dev.off()
#
setEPS()
postscript(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/mon",tsh,selh,varh,".eps"), horizontal = FALSE, onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
p<-ggplot(tmpdat[tmpdat$lh=="mon",],aes(x=year,y=value, col= factor(type))) + facet_grid(nm+sr~HD+ar, scales = "free_y",labeller=label_both) #
print(p+stat_summary(aes(group=type,col=factor(type)), fun=median, geom="line")+ ylab(ylab)+theme_classic())
dev.off()

    }
  }
}
