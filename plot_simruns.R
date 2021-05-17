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
library(dplyr)

#setwd("C:/Users/LukeB/Documents/sim_sbar")
setwd("/home/luke/Documents/sim_sbar")
source("plot_funs.R")
#load("res_10iters_allsce.Rdata")
#
# plot(tmpres[[1]]$stk_c$mwts[2,,2],type="b",col="green",ylim=c(0,max(tmpres[[1]]$stk_c$mwts[2,,2])))
# lines(tmpres[[1]]$stk_c$mwts[1,,2],col="blue")
# lines(tmpres[[1]]$stk_c$mwts[3,,2],col="red")
#
# lines(tmpres[[5]]$stk_c$mwts[1,,2],col="blue",lty=2)
# lines(tmpres[[5]]$stk_c$mwts[3,,2],col="red",lty=2)
# lines(tmpres[[5]]$stk_c$mwts[2,,2],col="green",lty=2)
#load("res_10iters_sce3_7_n.RData")
load("res_100iters_allsce_new_2.RData")


#quick sanity check that the right number of iters are being stored
dim(tmpres[[1]]$stk_c$nocut$mwts)

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
dim(bigdat)
#====================================================
#currently we have the 48 scenarios from sce dataframe and the three Harvest Dynamics (HD) for each scenario. 48*3 = 144 scenarios. 144* 10 iters = 1440
#library(data.table)
data_table_1 <- data.table::data.table(bigdat, key="sce_id")
sce$sce_id<-as.integer(row.names(sce))
str(sce)

data_table_2 <- data.table::data.table(sce, key="sce_id")
dim(bigdat)
f_dat<-merge(data_table_1, data_table_2)
dim(f_dat)
#f_dat<-merge(bigdat, sce, by.x  = "sce_id" , by.y =0,all.x=T)
head(f_dat)
#f_dat<-as.data.frame(f_dat)
ls()
rm(list=c("bigdat","bigdat_ls","tmpres", "par_extract","par_extract_inner","res_extract","res_extract_inner","data_table_1","data_table_2", "error_extract","error_extract_inner"))# remove so free up space
ls()
head(f_dat)
str(f_dat)
f_dat$HD<-factor(f_dat$HD)
f_dat$aorif<-factor(f_dat$aorif)

length(levels(factor(f_dat$sce_id)))
length(unique(with(f_dat, interaction(sce_id, HD, drop=TRUE))))   # gives correct number of 144
length(unique(with(f_dat, interaction(sce_id, HD,iter, drop=TRUE))))   # gives correct number 144 * iters
length(unique(with(f_dat, interaction(sce_id, HD,iter,type, drop=TRUE))))   # gives correct number 144 * iters * 6 (5 assessments and 1 real values)
#### get real values in their own columns
head(f_dat)
#tmp<-subset(f_dat,f_dat$sce_id==5 & f_dat$HD=="stk_c" & f_dat$iter==10)
# dim(tmp)
# table(tmp$type)
#tmp11<-reshape2::dcast(f_dat, sce_id+HD+aorif+year+iter+lh+ts+sel+ar+sr+var~ type, value.var="value")

tmp1<-data.table::dcast.data.table(f_dat, sce_id+HD+aorif+year+iter+lh+ts+sel+ar+sr+var~ type, value.var="value")
#tmp2<-dcast(f_dat, sce_id+HD+aorif+iter+lh+ts+sel+ar+sr+var~ type, value.var="se")
dim(tmp1)
head(tmp1)

tmp1.2<-melt(tmp1,id.vars = c(1:11,17),variable.name = "assessment")
#tmp2.2<-melt(tmp2,id.vars = c(1:10,12),variable.name = "assessment")[,-11]
dim(tmp1.2)
#all.equal(tmp1.2[,1:10],(tmp2.2)[,1:10])
#f_dat<-cbind(tmp1.2,tmp2.2[,12])
#colnames(f_dat)[13:14]<-c("est","se")
f_dat<-tmp1.2
f_dat[1:20,]
rm(list=c("tmp1","tmp1.2"))

load("conver.RData")
levels(factor(conver$assessment))
levels(factor(f_dat$assessment))

str(conver) #
conver$uniqrun <- with(conver, interaction(sce_id, HD,iter,assessment,aorif, drop=TRUE))
f_dat$uniqrun <- with(f_dat[,], interaction(sce_id, HD,iter,assessment,aorif, drop=TRUE))
dim(conver)
head(conver[,16:18])

data_table_1 <- data.table::data.table(f_dat, key="uniqrun")
data_table_2 <- data.table::data.table(conver, key="uniqrun")
f_dat<-merge(data_table_1, data_table_2[,c(1,16:18)])

rm(list=c("data_table_1","data_table_2","conver","sce"))


head(f_dat)
table(f_dat$assessment)
plot_dat<-f_dat %>%
     filter(filter=="realistic_estimates")%>%
  mutate(assessment = forcats::fct_relevel(assessment, 
                                           "CSA","ASOEM-cw","ASOEM-ubw","SOPEM-cw","SOPEM-ubw"))

levels(plot_dat$assessment) <- c("CSA",expression("S1"["c"]),expression("S1"["u"]),expression("S0"["c"]),expression("S0"["u"]))


levels(factor(plot_dat$assessment))
levels(factor(plot_dat$Error))
#unique(with(plot_dat, interaction(sel,aorif, drop=TRUE)))  

#====================================================================
#Comparison plots
library(ggplot2)
library(ggh4x)
#f_dat<-f_dat[which(!is.na(f_dat$value)),]
#   tmpdat <- plot_dat %>%
# filter(sce_id == "8")
library(scales)
library(RColorBrewer)
myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(plot_dat$assessment)
colScale <- scale_colour_manual(name = "Assessment",values = myColors,labels = parse_format())

pal <- rgb(0.374, 0.183, 0.528)
show_col(pal)
show_col(myColors)
# tmp2

head(plot_dat)
colnames(plot_dat)[7:11]<-c("LH","TS","SEL","AR","SR")
table(plot_dat$LH)
levels(plot_dat$LH)<- c("large demersal","small pelagic") 
levels(plot_dat$HD)<- c("c","ow","rc") 

dev.off()
##comparing knife edge only
#stat_summary(aes(y=rv,shape="Real values",linetype="Real values"),col=1, fun=median, geom="line")

width_measured <- 8.5
height_measured <- 11.5
colnames(plot_dat)[14]<-"Assessment"
#for(selh in levels(plot_dat$sel)){
  for(tsh in levels(plot_dat$TS)){
    for(varh in  c("stk_no","f")){
      # selh<-"kn0"
      # tsh<-"long"
      #tsh<-"short"
      # varh<-"f"
      #varh<-"stk_no"
      # 
      
      dataMedian <- plot_dat%>%
        filter(var == varh,TS ==tsh)%>%
        group_by(LH,SEL,year,SR,AR,HD,Assessment)%>%
        summarise(MD = round(median(value),2))
      
      #which(dataMedian$MD==max(dataMedian$MD))
      #dataMedian[2459,]
      
      # 
      # max(dataMedian$MD[dataMedian$LH==levels(plot_dat$LH)[1] & dataMedian$SEL=="kn0" ],na.rm=T)
      # max(dataMedian$MD[dataMedian$LH==levels(plot_dat$LH)[1] & dataMedian$SEL=="logistic" ],na.rm=T)
      # max(dataMedian$MD[dataMedian$LH==levels(plot_dat$LH)[1] & dataMedian$SEL=="dome" ],na.rm=T)
      #   
      #dataMedian <- dataMedian[dataMedian$Assessment!="\"S1\"[\"c\"]" & dataMedian$SEL !="dome",]
     
      selh<-"kn0"
      tmpdat <- plot_dat %>%
        filter(var == varh,SEL == selh,TS ==tsh)
      
 if(varh=="stk_no"){
    ylab <- "Stock numbers"
      tmp_dataMedian<- dataMedian %>%
      filter(SEL==selh)
    her_mxy<-ceiling(max(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[2]],na.rm=T))
    her_mny<-floor(min(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[2]],na.rm=T))
    her_brks<-c(signif(her_mxy/3,2),(signif(her_mxy/3,2))*2,(signif(her_mxy/3,2))*3)
  
    
    mon_mxy<-ceiling(max(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[1]],na.rm=T))
    mon_mny<-floor(min(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[1]],na.rm=T))
    mon_brks<-c(signif(mon_mxy/3,2),(signif(mon_mxy/3,2))*2,(signif(mon_mxy/3,2))*3)
    
 }else{
     her_brks<- mon_brks <- c(0.25,0.5,0.75,1)
     ylab <- "Fishing mortality"}
    
p1<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[2],],aes(x=year,y=value, col= Assessment)) + facet_nested(SR+AR~LH+HD, labeller=label_both) #

  p1 <- p1+stat_summary(aes(y=rv,linetype="Real values"),col=1,size=1, fun=median, geom="line",alpha=1)+stat_summary(aes(group=Assessment,col=Assessment), fun=median, geom="line",alpha=0.9) + ylab(ylab)+theme_classic()+colScale+scale_linetype_manual("",values=c("Real values"="dotted"))+ guides(linetype = guide_legend(order = 2),col = guide_legend(order = 1))+scale_y_continuous(breaks=her_brks)+xlab("Year")
  
p2<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[1],],aes(x=year,y=value, col= Assessment)) + facet_nested(SR+AR~LH+HD, labeller=label_both) #

  p2 <- p2+stat_summary(aes(y=rv,linetype="Real values"),col=1,size=1, fun=median, geom="line",alpha=1)+stat_summary(aes(col=Assessment), fun=median, geom="line",alpha=0.9) + ylab(ylab)+theme_classic()+colScale+scale_linetype_manual("",values=c("Real values"="dotted"))+ guides(linetype = guide_legend(order = 2),col = guide_legend(order = 1))+scale_y_continuous(breaks=mon_brks)+xlab("Year")

  selh<-"logistic"
  tmpdat <- plot_dat %>%
    filter(var == varh,SEL == selh,TS ==tsh)
  
  if(varh=="stk_no"){
    ylab <- "Stock numbers"
    tmp_dataMedian<- dataMedian %>%
      filter(SEL==selh)
    her_mxy<-ceiling(max(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[2]],na.rm=T))
    her_mny<-floor(min(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[2]],na.rm=T))
    her_brks<-c(signif(her_mxy/3,2),(signif(her_mxy/3,2))*2,(signif(her_mxy/3,2))*3)
    
    
    mon_mxy<-ceiling(max(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[1]],na.rm=T))
    mon_mny<-floor(min(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[1]],na.rm=T))
    mon_brks<-c(signif(mon_mxy/3,2),(signif(mon_mxy/3,2))*2,(signif(mon_mxy/3,2))*3)
    
  }else{
      her_brks<- mon_brks <- c(0.25,0.5,0.75,1)
    ylab <- "Fishing mortality"}
  
  p3<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[2],],aes(x=year,y=value, col= Assessment)) + facet_nested(SR+AR~LH+HD, labeller=label_both) #
  
  p3 <- p3+stat_summary(aes(y=rv,linetype="Real values"),col=1,size=1, fun=median, geom="line",alpha=1)+stat_summary(aes(group=Assessment,col=Assessment), fun=median, geom="line",alpha=0.9) + ylab(ylab)+theme_classic()+colScale+scale_linetype_manual("",values=c("Real values"="dotted"))+ guides(linetype = guide_legend(order = 2),col = guide_legend(order = 1))+scale_y_continuous(breaks=her_brks)+xlab("Year")
  
  p4<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[1],],aes(x=year,y=value, col= Assessment)) + facet_nested(SR+AR~LH+HD, labeller=label_both) #
  
  p4 <- p4+stat_summary(aes(y=rv,linetype="Real values"),col=1,size=1, fun=median, geom="line",alpha=1)+stat_summary(aes(group=Assessment,col=Assessment), fun=median, geom="line",alpha=0.9) + ylab(ylab)+theme_classic()+colScale+scale_linetype_manual("",values=c("Real values"="dotted"))+ guides(linetype = guide_legend(order = 2),col = guide_legend(order = 1))+scale_y_continuous(breaks=mon_brks)+xlab("Year")
  
  
  selh<-"dome"
  tmpdat <- plot_dat %>%
    filter(var == varh,SEL == selh,TS ==tsh, Assessment!="\"S1\"[\"c\"]")
  

  if(varh=="stk_no"){
    ylab <- "Stock numbers"
    
    dataMedian <- subset(dataMedian,dataMedian$Assessment!="\"S1\"[\"c\"]" & dataMedian$SEL =="dome"|dataMedian$SEL =="logistic"|dataMedian$SEL =="kn0")
    
    tmp_dataMedian<- dataMedian %>%
      filter(SEL==selh)
    her_mxy<-ceiling(max(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[2]],na.rm=T))
    her_mny<-floor(min(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[2]],na.rm=T))
    her_brks<-c(signif(her_mxy/3,2),(signif(her_mxy/3,2))*2,(signif(her_mxy/3,2))*3)
    
    
    mon_mxy<-ceiling(max(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[1]],na.rm=T))
    mon_mny<-floor(min(tmp_dataMedian$MD[tmp_dataMedian$LH==levels(plot_dat$LH)[1]],na.rm=T))
    mon_brks<-c(signif(mon_mxy/3,2),(signif(mon_mxy/3,2))*2,(signif(mon_mxy/3,2))*3)
    
  }else{
    
        her_brks<- mon_brks <- c(0.25,0.5,0.75,1)
    ylab <- "Fishing mortality"}
  
  p5<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[2],],aes(x=year,y=value, col= Assessment)) + facet_nested(SR+AR~LH+HD, labeller=label_both) #
  
  p5 <- p5+stat_summary(aes(y=rv,linetype="Real values"),col=1,size=1, fun=median, geom="line",alpha=1)+stat_summary(aes(group=Assessment,col=Assessment), fun=median, geom="line",alpha=0.9) + ylab(ylab)+theme_classic()+colScale+scale_linetype_manual("",values=c("Real values"="dotted"))+ guides(linetype = guide_legend(order = 2),col = guide_legend(order = 1))+scale_y_continuous(breaks=her_brks)+xlab("Year")
  
  p6<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[1],],aes(x=year,y=value, col= Assessment)) + facet_nested(SR+AR~LH+HD, labeller=label_both) #
  
  p6 <- p6+stat_summary(aes(y=rv,linetype="Real values"),col=1,size=1, fun=median, geom="line",alpha=1)+stat_summary(aes(group=Assessment,col=Assessment), fun=median, geom="line",alpha=0.9) + ylab(ylab)+theme_classic()+colScale+scale_linetype_manual("",values=c("Real values"="dotted"))+ guides(linetype = guide_legend(order = 2),col = guide_legend(order = 1))+scale_y_continuous(breaks=mon_brks)+xlab("Year")
  
  
  pp<-ggpubr::ggarrange(p2,p1,p4,p3,p6,p5, ncol=2, nrow=3, common.legend = TRUE, legend="bottom",labels = c("A", "B","C","D","E","F"))
  pp
  
  #flpath<-paste0("C:/Users/LukeB/OneDrive - GMIT/latex_p2/4sims/bothsp",tsh,varh,"time.pdf")
  flpath<-paste0("/home/luke/Documents/latex_p2/4sims/bothsp",tsh,varh,"time.pdf")
  dev.off()
  pdf(file= flpath,  width=width_measured, height=height_measured,pointsize=12)
  print(pp)
  dev.off()
  
  
    }
}


























# 
# varh<-"f"
# tmpdat <- plot_dat %>%
#   filter(var == varh,sel == selh,ts ==tsh)
# 
# ylab <- "Fishing mortality"
# 
# p1<-ggplot(tmpdat[tmpdat$lh=="her",],aes(x=year,y=value, col= assessment)) + facet_nested(sr+ar~HD, labeller=label_both,scales="free_x") #
# 
# p1<-p1+stat_summary(aes(y=rv,linetype="Real values"),col=1,size=1, fun=median, geom="line",alpha=1)+stat_summary(aes(group=assessment,col=assessment), fun=median, geom="line",alpha=0.9) + ylab(ylab)+theme_classic()+colScale+scale_linetype_manual("",values=c("Real values"="dotted"))+ guides(linetype = guide_legend(order = 2),col = guide_legend(order = 1))
# 
# p2<-ggplot(tmpdat[tmpdat$lh=="mon",],aes(x=year,y=value, col= assessment)) + facet_nested(sr+ar~HD, labeller=label_both) #
# 
# p2<-p2+stat_summary(aes(y=rv,linetype="Real values"),col=1,size=1, fun=median, geom="line",alpha=1)+stat_summary(aes(group=assessment,col=assessment), fun=median, geom="line",alpha=0.9) + ylab(ylab)+theme_classic()+colScale+scale_linetype_manual("",values=c("Real values"="dotted"))+ guides(linetype = guide_legend(order = 2),col = guide_legend(order = 1))
# 


#check<-subset(plot_dat,sce_id==3)
#save(check,file="check.RData")





##======================================================
#
#Relative error plots
#============================================================

#dcast(tab, sel+ assessment  ~ converged)
head(plot_dat)
vars<-c("f" ,"stk_no")
datf <- plot_dat %>%
  filter(var %in% vars )
head(datf)

datf$re<- (datf$value - datf$rv)/datf$rv

dim(datf)
head(datf)

tmpdat <- datf %>%
    filter(sel == "kn0",ts =="long", sr=="recsd0.1" & ar=="nocor",var=="f")

      ylab <- "fishing mortality"

    # setEPS()
    # postscript(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/her",tsh,selh,varh,".eps"), horizontal = FALSE, onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
#dev.new()
    p<-ggplot(tmpdat,aes(x=year,y=re)) +
      geom_point(alpha=0.1)+
      geom_hline(yintercept = 0,linetype="dashed")+
      facet_grid(assessment~HD ,labeller=label_both) +
            geom_smooth(stat = 'summary', alpha = 0.2, fill = 'grey', color = 'grey',
            fun.data = median_hilow, fun.args = list(conf.int = .95))+
      geom_smooth(stat = 'summary', alpha = 0.2, fill = 'grey21', color = 'black',
                  fun.data = median_hilow, fun.args = list(conf.int = .5))

p+ ylab(ylab)+theme_classic()


#install.packages("ggh4x")
library(ggh4x)

for(srh in unique(datf$sr)){
  for(arh in unique(datf$ar)){

tmpdat <- datf %>%
  filter(ts =="long",lh =="mon",sel == "dome", sr==srh & ar==arh)
arrrg<-c("nocor","cor")
srrrrr<-c("srlow","srhi")
i <- paste0(arrrg[which(arh==unique(datf$ar))],srrrrr[which(srh==unique(datf$sr))])

setEPS()
 postscript(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/rel_mon",i,"dome.eps"), horizontal = FALSE, onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
p<-ggplot(tmpdat,aes(x=year,y=re)) +
  #geom_point(alpha=0.1)+
  facet_nested(assessment~HD+var) +
  geom_smooth(stat = 'summary', alpha = 1, size =0.7,fill = 'grey80', color = 'grey',
              fun.data = median_hilow, fun.args = list(conf.int = .95))+
  geom_smooth(stat = 'summary', alpha = 1, size =0.7, fill = 'grey51', color = 'black',
              fun.data = median_hilow, fun.args = list(conf.int = .5))+
  geom_hline(yintercept = 0,linetype="dashed")+
  ggtitle(paste0("Anglerfish with dome-shaped selection, ",srh, "and",arh))

print(p+ ylab("Relative error")+theme_classic())
dev.off()
  }
}
