#===================================
# Plotting anglerfish assessment results
# Author : LB
#Distributed under GPL-3 or later
#May 2021
#===================================


##====================================================================================
#plotting index and catch
library(ggplot2)
rm(list=ls())
#setwd("C:/Users/LukeB/Documents/sim_sbar")
setwd("/home/luke/Documents/sim_sbar")
load("mon78_schnute_res.Rdata")
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
p1<-ggplot(data=datgg, aes(x=year, y=est))+ geom_ribbon(aes(ymin=lower, ymax=upper),fill="lightgrey",color="lightgrey")+geom_line(aes(colour="Predicted index"),size=s) +geom_point(aes(y = a4a, col= "Observed index"),size=t,shape=8)+ geom_line()+geom_line(aes(y = a4a, col= "Observed index"),size=s,linetype="dashed")+ scale_colour_manual(name='', values=c('Predicted index'='black', 'Observed index'='black'))+scale_fill_manual(name='', values=c('Predicted index'=1, 'Observed index'=NA))+guides(colour = guide_legend(override.aes = list(linetype=c(2,1), shape=c(8,NA ))))+theme(text = element_text(size=20))+facet_grid(ver~ver2,scales="free",labeller = label_parsed)

p1<-p1+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background =element_rect(color="black",fill="lightgrey"))+labs(y="", x="Year")

p1

# dev.off()
# 
# setEPS()
# postscript("C:/Users/LukeB/OneDrive - GMIT/latex_p2/4sims/schn_mon_obspred.eps", horizontal = FALSE, onefile = FALSE, paper = "special",width=8, height=8,pointsize=12)
# p1
# dev.off()

#rm(list=ls())
load("mon78_csa_res.RData")
years<-2003:2018
subgg<-obs.srep[rownames(obs.srep)=="logpred_survey"|rownames(obs.srep)=="lnc",]#,"phat","rhat","f_calc"),]
  
  datgg<-data.frame(param=rownames(subgg),lnest=subgg[,1],est=exp(subgg[,1]),lnse=subgg[,2],upper=NA,lower=NA)
  datgg$param<-as.character(datgg$param)
  
  datgg$ver<-c(rep("Catch (Numbers)", times=16),rep(c("IBTS recruits (CPUE)","IBTS post-recruits (CPUE)","IE-IAMS post-recruits (CPUE)","SP-PORC post-recruits (CPUE)"),times=16))
  
  datgg$upper<-exp(datgg$lnest + 2*datgg$lnse)
  datgg$lower<-exp(datgg$lnest - 2*datgg$lnse)
  datgg$year<-c(rep(years,times=1),rep(years,each=4))
  datgg$a4a<-c( catch.no,c(obs))
  
  datgg$lnupper<-datgg$lnest + 2*datgg$lnse
  datgg$lnlower<-datgg$lnest - 2*datgg$lnse
  
  #plot
  s=.5
  t=1
  p2<-ggplot(data=datgg, aes(x=year, y=est))+ geom_ribbon(aes(ymin=lower, ymax=upper),fill="lightgrey",color="lightgrey")+geom_line(aes(colour="Predicted index"),size=s) +geom_point(aes(y = a4a, col= "Observed index"),size=t,shape=8)+ geom_line()+geom_line(aes(y = a4a, col= "Observed index"),size=s,linetype="dashed")+ scale_colour_manual(name='', values=c('Predicted index'='black', 'Observed index'='black'))+scale_fill_manual(name='', values=c('Predicted index'=1, 'Observed index'=NA))+guides(colour = guide_legend(override.aes = list(linetype=c(2,1), shape=c(8,NA ))))+theme(text = element_text(size=20))+facet_wrap(~ver,scales="free_y",ncol = 2)
  
  p2<-p2+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"),strip.background =element_rect(color="black",fill="lightgrey"))+labs(y="", x="Year")
  
  p2
  
  pp<-ggpubr::ggarrange(p1,p2, ncol=1, nrow=2, common.legend = TRUE, legend="right",labels = c("A", "B"))
  
  pp
  
  setEPS()
  postscript("C:/Users/LukeB/OneDrive - GMIT/latex_p2/4sims/both_mon_obspred.eps", horizontal = FALSE, onefile = FALSE, paper = "special",width=10, height=12,pointsize=12)
  pp
  dev.off()
  
  #===================================================================================
  #comparison
  rm(list=ls())
library(ggplot2)
  load("mon78_schnute_res.Rdata")
  
  subgg<-obs.srep4[rownames(obs.srep4)=="lnN"|rownames(obs.srep4)=="logf_calc"|rownames(obs.srep4)=="lnPR"|rownames(obs.srep4)=="lnR",]#,"phat","rhat","f_calc"),]
  
  datgg<-data.frame(param=rownames(subgg),lnest=subgg[,1],est=exp(subgg[,1]),lnse=subgg[,2],upper=NA,lower=NA)
  
  datgg$param<-as.character(datgg$param)
  
  datgg$ver<-"S1"#c(rep("Biomass",64),rep("Numbers",48))
  years<-2003:2018
  datgg$upper<-exp(datgg$lnest + 2*datgg$lnse)
  datgg$lower<-exp(datgg$lnest - 2*datgg$lnse)
  datgg$year<-rep(years,times=4)
  datgg$a4a<-c( f,trueno*1e3,trueprevexno*1e3,truerecno*1e3)
  
  
  subgg.1<-obs.srep3[rownames(obs.srep3)=="lnN"|rownames(obs.srep3)=="lnPR"|rownames(obs.srep3)=="lnR",]#,"phat","rhat","f_calc"),]
  
  datgg.1<-data.frame(param=rownames(subgg.1),lnest=subgg.1[,1],est=exp(subgg.1[,1]),lnse=subgg.1[,2],upper=NA,lower=NA)
  
  datgg.1$param<-as.character(datgg.1$param)
  
  datgg.1$ver<-"S0"#c(rep("Biomass",64),rep("Numbers",48))
  
  datgg.1$upper<-exp(datgg.1$lnest + 2*datgg.1$lnse)
  datgg.1$lower<-exp(datgg.1$lnest - 2*datgg.1$lnse)
  #datgg.1<-rbind(datgg.1[1,],datgg.1)
  datgg.1[c(17,33),2:6]<-NA
  datgg.1$year<-rep(years,times=3)
  datgg.1$a4a<-c(trueno*1e3,trueprevexno*1e3,truerecno*1e3)
  
  
  plot(datgg.1[datgg.1$param=="lnN",3],type="b",ylim=c(0,7e9))
  
  lines(datgg.1[datgg.1$param=="lnPR",3]+datgg.1[datgg.1$param=="lnR",3],type="b",col=2)
  
  
  
  
  
  load("mon78_csa_res.RData")
  
  subgg.2<-obs.srep[rownames(obs.srep)=="lnbhat"|rownames(obs.srep)=="logf_calc"|rownames(obs.srep)=="lnphat"|rownames(obs.srep)=="logrhat",]#,"phat","rhat","f_calc"),]
  
  datgg.2<-data.frame(param=rownames(subgg.2),lnest=subgg.2[,1],est=exp(subgg.2[,1]),lnse=subgg.2[,2],upper=NA,lower=NA)
  
  datgg.2$param<-as.character(datgg.2$param)
  
  datgg.2$ver<-"CSA"#c(rep("Biomass",64),rep("Numbers",48))
  
  datgg.2$upper<-exp(datgg.2$lnest + 2*datgg.2$lnse)
  datgg.2$lower<-exp(datgg.2$lnest - 2*datgg.2$lnse)
  #datgg.2<-rbind(datgg.2[1,],datgg.2)
  #datgg.2[1,2:6]<-NA
  datgg.2$year<-rep(years,times=4)
  datgg.2$a4a<-c(truerecno*1e3,f,trueprevexno*1e3,trueno*1e3)
  
  datgg<-rbind(datgg,datgg.1,datgg.2)
  
  datgg$lnupper<-datgg$lnest + 2*datgg$lnse
  datgg$lnlower<-datgg$lnest - 2*datgg$lnse
  
  datgg[63:65,]
  
  datgg[datgg$est==is.na(datgg$est),]
  datgg$upper[datgg$est==0]<-NA
  datgg$lower[datgg$est==0]<-NA
  datgg$lnupper[datgg$est==0]<-NA
  datgg$lnlower[datgg$est==0]<-NA
  datgg$lnest[datgg$est==0]<-NA
  datgg$est[datgg$est==0]<-NA
  
  
  dim(datgg)
  datgg$ver<-factor(datgg$ver)
  levels(datgg$ver) <- c("CSA",expression("S0"["u"]),expression("S1"["u"]))
  
  #levels(factor(datgg$param))
  datgg$param[datgg$param=="lnN" | datgg$param=="lnbhat"]<-("Population numbers")
  datgg$param[datgg$param=="logf_calc"]<-("Fishing mortality")
  datgg$param[datgg$param=="lnPR" | datgg$param=="lnphat"]<-("Previously exploited/ post-recruit numbers")
  datgg$param[datgg$param=="lnR"| datgg$param=="logrhat"]<-("Recruitment numbers")
  
  datgg$param<-factor(datgg$param)
  levels(datgg$param)<-c(expression("Fishing mortality"[""]),expression("Population numbers"[""]),expression("Post-recruit numbers"[""]), expression("Recruitment numbers"[""]))
  levels(datgg$param)[1]
  
  #plot
  s=.5
  t=1
  p1<-ggplot(data=datgg[datgg$param!=levels(datgg$param)[1],], aes(x=year, y=est))+ geom_ribbon(aes(ymin=lower, ymax=upper),fill="lightgrey",color="lightgrey")+geom_line(aes(colour="Age-based model estimates"),size=s) +geom_point(aes(y = a4a, col= "Assessment model estimates"),size=t,shape=8)+ geom_line()+geom_line(aes(y = a4a, col= "Assessment model estimates"),size=s,linetype="dashed")+ scale_colour_manual(name='', values=c('Age-based model estimates'='black', 'Assessment model estimates'='black'))+scale_fill_manual(name='', values=c('Age-based model estimates'=1, 'Assessment model estimates'=NA))+guides(colour = guide_legend(override.aes = list(linetype=c(2,1), shape=c(8,NA ))))+theme(text = element_text(size=20))+facet_grid(param~ver,scales="free_y",labeller = label_parsed)
  
  p1<-p1+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.text.y = element_text(size=8), axis.line = element_line(colour = "black"),strip.background =element_rect(color="black",fill="lightgrey"))+labs(y="", x="Year")+coord_cartesian(ylim=c(0,3e8))
  
  p1

  #plot
  s=.5
  t=1
  p2<-ggplot(data=datgg[datgg$param==levels(datgg$param)[1],], aes(x=year, y=est))+ geom_ribbon(aes(ymin=lower, ymax=upper),fill="lightgrey",color="lightgrey")+geom_line(aes(colour="Age-based model estimates"),size=s) +geom_point(aes(y = a4a, col= "Assessment model estimates"),size=t,shape=8)+ geom_line()+geom_line(aes(y = a4a, col= "Assessment model estimates"),size=s,linetype="dashed")+ scale_colour_manual(name='', values=c('Age-based model estimates'='black', 'Assessment model estimates'='black'))+scale_fill_manual(name='', values=c('Age-based model estimates'=1, 'Assessment model estimates'=NA))+guides(colour = guide_legend(override.aes = list(linetype=c(2,1), shape=c(8,NA ))))+theme(text = element_text(size=20))+facet_grid(param~ver,scales="free_y",labeller = label_parsed)
  
  p2<-p2+theme_bw()+theme(panel.border = element_blank(), panel.grid.major = element_blank(),panel.grid.minor = element_blank(),strip.text.y = element_text(size=8), axis.line = element_line(colour = "black"),strip.background =element_rect(color="black",fill="lightgrey"))+labs(y="", x="Year")
  
  p2
  
  pp<-ggpubr::ggarrange(p1,p2, ncol=2, nrow=1, common.legend = TRUE, legend="bottom",labels = c("A", "B"))
  
  
  
  dev.off()
  pdf("C:/Users/LukeB/OneDrive - GMIT/latex_p2/4sims/mon_comp1.pdf",  paper = "special",width=9, height=5,pointsize=14)
  pp
  dev.off()
  
  
  
  
  
  
