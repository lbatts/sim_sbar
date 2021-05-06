


##====================================================================================
#plotting index and catch
library(ggplot2)
rm(list=ls())
setwd("C:/Users/LukeB/Documents/sim_sbar")
load("mon78_schnute_res.RData")
years<-2003:2018
subgg<-obs.srep3[rownames(obs.srep3)=="logpred_survey"|rownames(obs.srep3)=="lnc",]#,"phat","rhat","f_calc"),]
datgg<-data.frame(param=rownames(subgg),lnest=subgg[,1],est=exp(subgg[,1]),lnse=subgg[,2],upper=NA,lower=NA)
datgg$param<-as.character(datgg$param)

datgg$ver<-c(rep(c("IBTS (CPUE)","IE-IAMS (CPUE)","SP-PORC (CPUE)"),times=16))
datgg$ver2<-"S0"
datgg$upper<-exp(datgg$lnest + 2*datgg$lnse)
datgg$lower<-exp(datgg$lnest - 2*datgg$lnse)
datgg$year<-c(rep(years,each=3))
datgg$a4a<-c(c(obs))
datgg[datgg$year==2003,2:6]<-NA

subgg.1<-obs.srep4[rownames(obs.srep4)=="logpred_survey"|rownames(obs.srep4)=="lnc",]#,"phat","rhat","f_calc"),]
datgg.1<-data.frame(param=rownames(subgg.1),lnest=subgg.1[,1],est=exp(subgg.1[,1]),lnse=subgg.1[,2],upper=NA,lower=NA)
datgg.1$param<-as.character(datgg.1$param)

datgg.1$ver<-c(rep("Catch (kg)", times=16),rep(c("IBTS (CPUE)","IE-IAMS (CPUE)","SP-PORC (CPUE)"),times=16))

datgg.1$ver2<-"S1"
datgg.1$upper<-exp(datgg.1$lnest + 2*datgg.1$lnse)
datgg.1$lower<-exp(datgg.1$lnest - 2*datgg.1$lnse)
datgg.1$year<-c(rep(years,times=1),rep(years,each=3))
datgg.1$a4a<-c( catch_kg1,c(obs))
#datgg.1[datgg.1$year==2003,2:6]<-NA

datgg<-rbind(datgg,datgg.1)

datgg$lnupper<-datgg$lnest + 2*datgg$lnse
datgg$lnlower<-datgg$lnest - 2*datgg$lnse
datgg$ver2<-factor(datgg$ver2)
levels(datgg$ver2) <- c(expression("S0"["u"]),expression("S1"["u"]))
#plot
s=.5
t=1
p1<-ggplot(data=datgg, aes(x=year, y=est))+ geom_ribbon(aes(ymin=lower, ymax=upper),fill="lightgrey",color="lightgrey")+geom_line(aes(colour="Predicted index"),size=s) +geom_point(aes(y = a4a, col= "Observed index"),size=t,shape=8)+ geom_line()+geom_line(aes(y = a4a, col= "Observed index"),size=s,linetype="dashed")+ scale_colour_manual(name='', values=c('Predicted index'='black', 'Observed index'='black'))+scale_fill_manual(name='', values=c('Predicted index'=1, 'Observed index'=NA))+guides(colour = guide_legend(override.aes = list(linetype=c(2,1), shape=c(8,NA ))))+theme(text = element_text(size=20))+facet_grid(ver~ver2,scales="free_y",labeller = label_parsed)

p1<-p1+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background =element_rect(color="black",fill="lightgrey"))+labs(y="", x="Year")

p1

dev.off()

setEPS()
postscript("C:/Users/LukeB/OneDrive - GMIT/latex_p2/4sims/schn_mon_obspred.eps", horizontal = FALSE, onefile = FALSE, paper = "special",width=8, height=8,pointsize=12)
p1
dev.off()

rm(list=ls())
load("mon78_csa_res.RData")
years<-2003:2018
subgg<-obs.srep[rownames(obs.srep)=="logpred_survey"|rownames(obs.srep)=="lnc",]#,"phat","rhat","f_calc"),]
  
  datgg<-data.frame(param=rownames(subgg),lnest=subgg[,1],est=exp(subgg[,1]),lnse=subgg[,2],upper=NA,lower=NA)
  datgg$param<-as.character(datgg$param)
  
  datgg$ver<-c(rep("Catch (Numbers)", times=16),rep(c("IE-IGFS/EVHOE recruits (CPUE)","IE-IGFS/EVHOE post-recruits (CPUE)","IE-IAMS post-recruits (CPUE)","SP-PORC post-recruits (CPUE)"),times=16))
  
  datgg$upper<-exp(datgg$lnest + 2*datgg$lnse)
  datgg$lower<-exp(datgg$lnest - 2*datgg$lnse)
  datgg$year<-c(rep(years,times=1),rep(years,each=4))
  datgg$a4a<-c( catch.no,c(obs))
  
  datgg$lnupper<-datgg$lnest + 2*datgg$lnse
  datgg$lnlower<-datgg$lnest - 2*datgg$lnse
  
  #plot
  s=.5
  t=1
  p1<-ggplot(data=datgg, aes(x=year, y=est))+ geom_ribbon(aes(ymin=lower, ymax=upper),fill="lightgrey",color="lightgrey")+geom_line(aes(colour="Predicted index"),size=s) +geom_point(aes(y = a4a, col= "Observed index"),size=t,shape=8)+ geom_line()+geom_line(aes(y = a4a, col= "Observed index"),size=s,linetype="dashed")+ scale_colour_manual(name='', values=c('Predicted index'='black', 'Observed index'='black'))+scale_fill_manual(name='', values=c('Predicted index'=1, 'Observed index'=NA))+guides(colour = guide_legend(override.aes = list(linetype=c(2,1), shape=c(8,NA ))))+theme(text = element_text(size=20))+facet_wrap(~ver,scales="free_y",ncol = 1)
  
  p1<-p1+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background =element_rect(color="black",fill="lightgrey"))+labs(y="", x="Year")
  
  p1
  
  
  
  setEPS()
  postscript("C:/Users/LukeB/OneDrive - GMIT/latex_p2/4sims/csa_mon_obspred.eps", horizontal = FALSE, onefile = FALSE, paper = "special",width=5, height=8,pointsize=12)
  p1
  dev.off()
  

