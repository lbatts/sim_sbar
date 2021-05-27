#===================================
# Plotting anglerfish self-tests
# Author : LB
#Distributed under GPL-3 or later
#May 2021
#===================================

rm(list=ls())
library(ggplot2)
setwd("/home/luke/Documents/sim_sbar")
load("mon_78_res4sim.RData")


obs3.srep<-summary(TMB::sdreport(obj3))
assess_3_N<-data.frame(estimate=obs3.srep[row.names(obs3.srep)=="N" ,"Estimate"],year=2003:2018,sim="datafit")

set.seed(24)
sim3<-obj3$simulate(opt3$par,complete = TRUE)
obs3<-sim3$obs_ind
cat3<-sim3$obs_catch

par(mfrow=c(2,2))
plot(c(obs[1,]), type = "o",ylim=c(0,10))
  lines(c(obs3[1,]),type="o",col="red")

  plot(c(obs[2,]), type = "o",ylim=c(0,200))
  lines(c(obs3[2,]),type="o",col="red")
  plot(c(obs[3,]), type = "o",ylim=c(0,40))
  lines(c(obs3[3,]),type="o",col="red")
  
# plot(c(catch_kg1), type = "o",ylim=c(0,1e8))
# lines(c(cat3),type="o",col=1,lwd=3) ## gives a similar Index

sim <- replicate(100, {
  simdata <- obj3$simulate(par=opt3$par)
  
  obj_tmp<-schnute_orig(version = 2,catchkg = catch_kg1, indiceskg = simdata$obs_ind, ts = c(0.875,1,0.875), mwts = ubw_mean_wts, tsp = 0, rho = ubw_rho, W = ubw_W , start_q = c(2e-8,2e-8,2e-8), start_indexsigma = c(0.1,0.1,0.1), start_sigma = exp(-0.25), fix_sigma = T, fix_indexsigma = FALSE,ind_l_wt=c(1,1,1))
  
  
  opt_tmp<-nlminb(obj_tmp$par, obj_tmp$fn, obj_tmp$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
  print(opt_tmp$convergence)
  if(opt_tmp$convergence==0){
    obs.srep_tmp<-summary(TMB::sdreport(obj_tmp))
    obs.srep_tmp[row.names(obs.srep_tmp)=="N" ,"Estimate"]
  }else{
    rep(NA,length(catch_kg1))
  }
  
})

dim(sim)
df <- data.frame(estimate=as.vector(sim), year=2003:2018,sim=rep(1:100,each=16))

os<-ggplot(df, aes(y=estimate,x=year,group=sim))+geom_line(col="gray")+geom_line(data=assess_3_N,col=1,size=1)+coord_cartesian(ylim=c(0,1e9))+theme_classic()+labs(y="Stock numbers", x="Year")#+geom_line(data=truth,col="orange",size=1.5)

## =================================================================
##
# Adapted Schnute
#============================

obs4.srep<-summary(TMB::sdreport(obj4))
assess_4_N<-data.frame(estimate=obs4.srep[row.names(obs4.srep)=="N" ,"Estimate"],year=2003:2018,sim="datafit")

set.seed(24)
sim4<-obj4$simulate(opt4$par,complete = TRUE)
obs4<-sim4$obs_ind
cat4<-sim4$obs_catch

par(mfrow=c(2,2))
plot(c(obs[1,]), type = "o",ylim=c(0,10))
lines(c(obs4[1,]),type="o",col="red")

plot(c(obs[2,]), type = "o",ylim=c(0,200))
lines(c(obs4[2,]),type="o",col="red")
plot(c(obs[3,]), type = "o",ylim=c(0,40))
lines(c(obs4[3,]),type="o",col="red")

 plot(c(catch_kg1), type = "o",ylim=c(0,1e8))
 lines(c(cat4),type="o",col=2,lwd=3) ## gives a similar Index

sim <- replicate(100, {
  simdata <- obj4$simulate(par=opt4$par)
  
  obj_tmp<-schnute_obserror(version=2,catchkg = simdata$obs_catch, indiceskg = simdata$obs_ind , ts = c(0.875,1,0.875), mwts = ubw_mean_wts, tsp = 0, rho = ubw_rho, W = ubw_W ,  start_indexsigma = c(0.1,0.1,0.1),
                            start_sigma = exp(-0.25),start_q = rep(2e-8,3), start_f_calc = 0.5, start_catchsigma = 0.1,
                            fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                            fix_catchsigma = T,ind_l_wt=c(1,1,1))
  
  
  opt_tmp<-nlminb(obj_tmp$par, obj_tmp$fn, obj_tmp$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
  print(opt_tmp$convergence)
  if(opt_tmp$convergence==0){
    obs.srep_tmp<-summary(TMB::sdreport(obj_tmp))
    obs.srep_tmp[row.names(obs.srep_tmp)=="N" ,"Estimate"]
  }else{
    rep(NA,length(catch_kg1))
  }
  
})

dim(sim)
df <- data.frame(estimate=as.vector(sim), year=2003:2018,sim=rep(1:100,each=16))
df[which(is.na(df)),]
as<-ggplot(df, aes(y=estimate,x=year,group=sim))+geom_line(col="gray")+geom_line(data=assess_4_N,col=1,size=1)+theme_classic()+labs(y="Stock numbers", x="Year")#+coord_cartesian(ylim=c(0,1e9))#+geom_line(data=truth,col="orange",size=1.5)

#=========================================================================
#
# CSA simulate
#
load("mon_78_res4sim_csa.RData")
att<-data.frame(survey=c(1,1,2,3),type=c(1,2,2,2))
timing=c(0.875,1,0.875)
cv=c(0.1,0.1,0.1)
selrec<-matrix(1,nrow=1,ncol=16)# number of rows is number of rec survey indices

obs.srep<-summary(TMB::sdreport(obj))
assess__N<-data.frame(estimate=obs.srep[row.names(obs.srep)=="bhat" ,"Estimate"],year=2003:2018,sim="datafit")

set.seed(24)
sim<-obj$simulate(opt$par,complete = TRUE)
obs_sim<-sim$obs_ind
cat<-sim$obs_catch

par(mfrow=c(2,2))
plot(c(obs[1,]), type = "o",ylim=c(0,8))
lines(c(obs_sim[1,]),type="o",col="red")
plot(c(obs[2,]), type = "o",ylim=c(0,7))
lines(c(obs_sim[2,]),type="o",col="red")
plot(c(obs[3,]), type = "o",ylim=c(0,120))
lines(c(obs_sim[3,]),type="o",col="red")
plot(c(obs[4,]), type = "o",ylim=c(0,12))
lines(c(obs_sim[4,]),type="o",col="red")

plot(c(catch.no), type = "o",ylim=c(0,5e7))
lines(c(cat),type="o",col=2,lwd=3) ## gives a similar Index

sim <- replicate(100, {
  simdata <- obj$simulate(par=opt$par)
  
obj_tmp<- csa(catch_n = simdata$obs_catch, indices_no = simdata$obs_ind, indices_att = att, ts = timing , selrec = selrec, start_q = rep(2e-8,3), start_surveycv = 0.1, start_catchcv = 0.1, start_nmort = 0.25, start_f_calc = 0.5,fix_nmort = T, fix_prec0 = F, fix_surveycv = F, fix_catchcv = T)
  
  
  opt_tmp<-nlminb(obj_tmp$par, obj_tmp$fn, obj_tmp$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
  print(opt_tmp$convergence)
  if(opt_tmp$convergence==0){
    obs.srep_tmp<-summary(TMB::sdreport(obj_tmp))
    obs.srep_tmp[row.names(obs.srep_tmp)=="bhat" ,"Estimate"]
  }else{
    rep(NA,length(catch.no))
  }
  
})

dim(sim)
df <- data.frame(estimate=as.vector(sim), year=2003:2018,sim=rep(1:100,each=16))
df[which(is.na(df)),]

cs<-ggplot(df, aes(y=estimate,x=year,group=sim))+geom_line(col="gray")+geom_line(data=assess__N,col=1,size=1)+theme_classic()+labs(y="Stock numbers", x="Year")#+ggtitle("CSA")+theme(plot.title = element_text(hjust = 0.5,)))     #+geom_line(data=truth,col="orange",size=1.5) 
pp<-ggpubr::ggarrange(cs,os,as, ncol=1, nrow=3,labels=c("A","B","C"))
pp
dev.off()
pdf(file= "/home/luke/Documents/latex_p2/4sims/mon_simulate.pdf",width=6, height=8,pointsize=12)
pp
dev.off()





