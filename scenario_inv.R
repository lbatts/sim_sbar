#========
#Code for Cóilín
#
#Investigating a scenarion in more detail
#=====


rm(list=ls())
library(FLCore)
library(FLasher)
library(sbar)

load("scenario8_stkstored.RData")

#"res" is a list of lists of various bits and pieces that make up the results of a scenario. For the purposes of investigating I've saved the stock and idx objects here

res$scenario #the scenario
plot(res$stk_ow$stk) # plot the one way stock object, Each scenario has three HDs within the list

#lapply(res$stk_ow,names)

res$stk_ow$schnualt_par #some parameter estimates/convergence notifcations of Schnute Original are stored here 
 
#all ten iterations went to singular convergence

#lets look at a particular iteration in the one way stk in more detail
it<-7
stk<-iter(res$stk_ow$stk,7)
idx<-iter(res$stk_ow$idx,7)

catn_err <- catch.n(idx)# catch at age with error applied is stored in the catch.n slot of index
catb_err <- catn_err * catch.wt(stk)

#visualise this

graphics::plot(colSums(catch.n(stk)),main= "total catch no",type="b")
lines(colSums(catn_err),col=2)
legend("bottomright",lty=c(1,1),col=c(1,2),legend=c("catch no", "catch no with error"),cex=0.7)

#
graphics::plot(colSums(catch(stk)),main= "total catch biomass",type="b")
lines(colSums(catb_err),col=2)
legend("bottomright",lty=c(1,1),col=c(1,2),legend=c("catch biomass", "catch biomass with error"),cex=0.7)
#

#mean weights
#we can look at full age range first and see
f_range<-1:9
pr_range<-2:9
no.years<-40
years <- 1:no.years

  Y2<-c(quantSums(catch.wt(stk)[1,]*catch.n(stk)[1,])/quantSums(catch.n(stk)[1,]))
Z2<-c(quantSums(catch.wt(stk)[pr_range,]*catch.n(stk)[pr_range,])/quantSums(catch.n(stk)[pr_range,]))
X2<-c(quantSums(catch.wt(stk)[f_range,]*catch.n(stk)[f_range,])/quantSums(catch.n(stk)[f_range,]))

mean_wts<-res$stk_ow$mwts[,,it]
Y<-mean_wts[1,]
Z<-mean_wts[2,]
X<-mean_wts[3,]

#par(mfrow=c(1,2))
#model X -> Z progression with linear model
plot(y=Z[2:no.years],x=X[1:no.years-1],xlim=c(0,max(Z[2:no.years])),ylim=c(0,max(Z[2:no.years])),pch=19,xaxs="i",yaxs="i",bty="l",xlab=expression(paste("X"[t]," (kg)")),ylab=expression(paste("Z"[t+1]," (kg)")))
points(y=Z2[2:no.years],x=X2[1:no.years-1],col=2)
mod<-lm(Z[2:no.years]~X[1:no.years-1])
x=seq(0,100,by=0.01)
lines(x=x,y=(coef(mod)[1] + coef(mod)[2]*x),col=1)
legend(x=0.04,y=0.04, legend =c("mean weights from catch","mean weights from catch with error",expression(paste("Linear model prediction",(Z[t+1]==italic(W) + rho*X[t])))), col = c(2,1,1), lty = c(NA,NA,1),pch=c(19,19,NA),cex=.7,box.lty=0)


W1<-coef(mod)[1]
rho1<-coef(mod)[2]
#visualise - red lines are growth from X -> Z


plot(y=Y,x=years,type="l",col=4,ylim=c(0,max(Z[2:no.years])),pch=19,xaxs="i",yaxs="i",bty="l",xlab="Year",ylab="Mean weight (kg)")
lines(y=Z,x=years,col=2)
lines(y=X,x=years,col=3)

xtmp1<-W1 + rho1*X
#
for(i in 1:no.years){
  lines(y=c(X[i],xtmp1[i]),x=c(years[i],years[i+1]),col=1,lty=2)
  
}

legend(x=2,y=0.04, legend = c("Growth","Y (Recruits)", "Z (Previously exploited biomass)", "X (Total biomass)"), col = c(1,4,2,3), lty = c(2,1,1,1),cex=0.5,box.lty=0)

#whilst here lets just demonstrate what I was trying to explain about mean weights shifting with selection

load("kn0_mwts.RData")
kn0_mwts # these mean weights are from the same scenario factors except with knife-edged selection. Seed is the same as scenario 8 and mwts are from stk_ow iter ==7

points(kn0_mwts[1,],col="blue",pch=5,type="b")
points(kn0_mwts[2,],col="red",pch=5,type="b")
points(kn0_mwts[3,],col="green",pch=5,type="b")
#===========================
#Move onto assessments
mort<-mean(m(stk),na.rm=T)
#Schnute first
catch_kg1<-c(colSums(catb_err))
index<-colSums(index(idx)*catch.wt(idx))
obs<-matrix(NA,nrow=1,ncol=no.years)
obs[1,]<-index 
sigma_st <- exp(-mort)

##======================================
# Schnute alt/original run
#=====================================
#use sbar package
obj<-schnute_peb("y1_no_f",catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho1, W = W1 , start_q = 1e-08, start_indexsigma = 0.1,
                  start_sigma = sigma_st, start_f_calc = 0.5, start_catchsigma = 0.1,
                  fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                  fix_catchsigma = T)
obj$fn()# 1113440

#for more flexibility in set up

data_tmb <- list(
  obs_catch = catch_kg1,
  obs_ind = obs,
  indices_class = c(2),
  indices_ts = 0,
  mean_wts=mean_wts,
  nu=0,
  SRcode=2,
  spawn_prop=rep(1,length(catch_kg1)))

params <- list(
  logq = log(1e-8),
  logindex_sigma = log(0.1),
  logrec_param = log(c(1,1)),
  logitsigma = stats::qlogis(sigma_st),
  logrho=log(rho1),
  logW=log(W1),
  logf_calc = log(rep(0.5,40)),
  logcatch_sigma = log(0.1))
#library(tmb)
# #load dll
# dyn.load(dynlib("C:/Users/LukeB/Documents/2stage_dd/alt_schnute"))

obj <- TMB::MakeADFun(
  data = c(model = "alt_schnute",data_tmb),
  parameters = params,
  map = list(
    logindex_sigma = factor(1),
    logW = factor(NA),
    logrho = factor(NA),
    logitsigma = factor(NA),
    logf_calc= factor(c(NA,1:(no.years-1))),
    logcatch_sigma = factor(NA),
    logrec_param = factor(rep(NA,2))
  ),
  hessian = TRUE,
  silent = TRUE,
  DLL = "sbar_TMBExports")

obj$fn() # should be 1113440, same as from sbar

(opt <- nlminb(start=obj$par,objective=obj$fn,gradient=obj$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10)))

obj$gr(opt$par)
library(TMBhelper)
check<-NA
check<-fit_tmb(obj)
check$Convergence_check
check$max_gradient
check2<-Check_Identifiable(obj) #

obs.rep <- TMB::sdreport(obj)
obs.srep <- summary(obs.rep)
qest<-exp(obs.srep[rownames(obs.srep)=="logq","Estimate"]) #
qest #q should be 1e-6

##======================================
# Schnute B0 estimate run
#=====================================
obj2<-schnute_peb("B0",catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho1, W = W1 , start_q = 1e-08, start_indexsigma = 0.1,
                  start_sigma = sigma_st, start_f_calc = 0.5, start_catchsigma = 0.1,
                  fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                  fix_catchsigma = T)
obj2$fn()


params <- list(
  logq = log(1e-8),
  logB0 = log(5*max(catch_kg1)),
  logindex_sigma = log(0.1),
  logrec_param = log(c(1,1)),
  logitsigma = stats::qlogis(sigma_st),
  logrho=log(rho1),
  logW=log(W1),
  logf_calc = log(rep(0.5,40)),
  logcatch_sigma = log(0.1))

obj <- TMB::MakeADFun(
  data = c(model = "schnute_new_V2",data_tmb),
  parameters = params,
  map = list(
    logB0=factor(1),
    logindex_sigma = factor(1),
    logW = factor(NA),
    logrho = factor(NA),
    logitsigma = factor(NA),
    logcatch_sigma = factor(NA),
    logrec_param = factor(rep(NA,2))
  ),
  hessian = TRUE,
  silent = TRUE,
  DLL = "sbar_TMBExports")

obj$fn()==obj2$fn()



##==============CSA##======================
#
catch.no<-colSums(catn_err, na.rm=T) #remember this is catch at age with error
obs<-matrix(NA,nrow=2,ncol=no.years)
att<-data.frame(survey=c(1,1),type=c(1,2))
selrec<-matrix(1,nrow=1,ncol=no.years)# this where you can input your own selctivity for recruits

obs[1,]<-c(index(idx)[1,])
obs[2,]<-colSums(index(idx)[pr_range,])

#args(csa)
obj3<- csa(catch_n = catch.no, indices_no = obs, indices_att = att, ts = 0 , selrec = selrec, start_q = 1e-8, start_surveycv = 0.1, start_catchcv = 0.1, start_nmort = mort, start_f_calc = 0.5,fix_nmort = T, fix_prec0 = F, fix_surveycv = F, fix_catchcv = T)
obj3$fn()


data_tmb <- list(
  obs_catch = c(catch.no),
  obs_ind = obs,
  indices_class = as.matrix(att),
  indices_ts = 0,
  sr =selrec)


params <- list(
  logitqhat = stats::qlogis(1e-8),
  log_surveycv = log(0.1),
  log_catchcv = log(0.1),
  logphat1 = log(4*max(catch.no)),
  logrhat = log(rep(2*max(catch.no), no.years)),
  logf_calc = log(rep(0.5,no.years)),
  lognmort = log(mort),
  logitsrx = stats::qlogis(rep(1,no.years))
)


obj <- TMB::MakeADFun(
  data = c(model = "csa",data_tmb),
  parameters = params,
  map = list(
    logphat1=factor(1),
    log_surveycv = factor(1),
    log_catchcv = factor(NA),
    lognmort = factor(NA),
    logitsrx = factor(rep(NA,no.years))#,
  ),
  hessian = TRUE,
  silent = TRUE,
  DLL = "sbar_TMBExports")

obj$fn()==obj3$fn()



##=================================================================================================================
#
# The same iteration but with age 0 removed
#
#=============================================================================================================


catn_err <- catch.n(idx)[2:9,]# catch at age with error applied is stored in the catch.n slot of index
catb_err <- catn_err * catch.wt(stk)[2:9,]

#visualise this

graphics::plot(colSums(catch.n(stk)[2:9,]),main= "total catch no",type="b")
lines(colSums(catn_err),col=2)
legend("bottomright",lty=c(1,1),col=c(1,2),legend=c("catch no", "catch no with error"),cex=0.7)

#
graphics::plot(colSums(catch.n(stk)[2:9,]*catch.wt(stk)[2:9,]),main= "total catch biomass",type="b")
lines(colSums(catb_err),col=2)
legend("bottomright",lty=c(1,1),col=c(1,2),legend=c("catch biomass", "catch biomass with error"),cex=0.7)
#

#mean weights
#we can look at full age range first and see
f_range<-2:9
pr_range<-3:9
no.years<-40
years <- 1:no.years

Y2<-c(quantSums(catch.wt(stk)[2,]*catch.n(stk)[2,])/quantSums(catch.n(stk)[2,]))
Z2<-c(quantSums(catch.wt(stk)[pr_range,]*catch.n(stk)[pr_range,])/quantSums(catch.n(stk)[pr_range,]))
X2<-c(quantSums(catch.wt(stk)[f_range,]*catch.n(stk)[f_range,])/quantSums(catch.n(stk)[f_range,]))

#mean_wts<-res$stk_ow$mwts[,,it]

Y<-c(quantSums(catch.wt(stk)[2,]*catch.n(idx)[2,])/quantSums(catch.n(idx)[2,]))
Z<-c(quantSums(catch.wt(stk)[pr_range,]*catch.n(idx)[pr_range,])/quantSums(catch.n(idx)[pr_range,]))
X<-c(quantSums(catch.wt(stk)[f_range,]*catch.n(idx)[f_range,])/quantSums(catch.n(idx)[f_range,]))

mean_wts2<-mean_wts
mean_wts2[1,]<-Y
mean_wts2[2,]<-Z
mean_wts2[3,]<-X

#par(mfrow=c(1,2))
#model X -> Z progression with linear model
plot(y=Z[2:no.years],x=X[1:no.years-1],xlim=c(0,max(Z[2:no.years])),ylim=c(0,max(Z[2:no.years])),pch=19,xaxs="i",yaxs="i",bty="l",xlab=expression(paste("X"[t]," (kg)")),ylab=expression(paste("Z"[t+1]," (kg)")))
points(y=Z2[2:no.years],x=X2[1:no.years-1],col=2)
mod<-lm(Z[2:no.years]~X[1:no.years-1])
x=seq(0,100,by=0.01)
lines(x=x,y=(coef(mod)[1] + coef(mod)[2]*x),col=1)
legend(x=0.04,y=0.04, legend =c("mean weights from catch","mean weights from catch with error",expression(paste("Linear model prediction",(Z[t+1]==italic(W) + rho*X[t])))), col = c(2,1,1), lty = c(NA,NA,1),pch=c(19,19,NA),cex=.7,box.lty=0)


W1<-coef(mod)[1]
rho1<-coef(mod)[2]
#visualise - red lines are growth from X -> Z


plot(y=Y,x=years,type="l",col=4,ylim=c(0,max(Z[2:no.years])),pch=19,xaxs="i",yaxs="i",bty="l",xlab="Year",ylab="Mean weight (kg)")
lines(y=Z,x=years,col=2)
lines(y=X,x=years,col=3)

xtmp1<-W1 + rho1*X
#
for(i in 1:no.years){
  lines(y=c(X[i],xtmp1[i]),x=c(years[i],years[i+1]),col=1,lty=2)
  
}

legend(x=2,y=0.04, legend = c("Growth","Y (Recruits)", "Z (Previously exploited biomass)", "X (Total biomass)"), col = c(1,4,2,3), lty = c(2,1,1,1),cex=0.5,box.lty=0)

#===========================
#Move onto assessments again
mort<-mean(m(stk),na.rm=T)
#Schnute first
catch_kg1<-c(colSums(catb_err))
index<-colSums(index(idx)[f_range,]*catch.wt(stk)[f_range,])
obs<-matrix(NA,nrow=1,ncol=no.years)
obs[1,]<-index 
sigma_st <- exp(-mort)

##======================================
# Schnute alt/original run
#=====================================
#use sbar package
obj1<-schnute_peb("y1_no_f",catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts2, tsp = 0, rho = rho1, W = W1 , start_q = 1e-08, start_indexsigma = 0.1,
                 start_sigma = sigma_st, start_f_calc = 0.5, start_catchsigma = 0.1,
                 fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                 fix_catchsigma = T)
obj1$fn()# 291399.3

#for more flexibility in set up

data_tmb <- list(
  obs_catch = catch_kg1,
  obs_ind = obs,
  indices_class = c(2),
  indices_ts = 0,
  mean_wts=mean_wts2,
  nu=0,
  SRcode=2,
  spawn_prop=rep(1,length(catch_kg1)))

params <- list(
  logq = log(1e-8),
  logindex_sigma = log(0.1),
  logrec_param = log(c(1,1)),
  logitsigma = stats::qlogis(sigma_st),
  logrho=log(rho1),
  logW=log(W1),
  logf_calc = log(rep(0.5,40)),
  logcatch_sigma = log(0.1))

obj <- TMB::MakeADFun(
  data = c(model = "alt_schnute",data_tmb),
  parameters = params,
  map = list(
    logindex_sigma = factor(1),
    logW = factor(NA),
    logrho = factor(NA),
    logitsigma = factor(NA),
    logf_calc= factor(c(NA,1:(no.years-1))),
    logcatch_sigma = factor(NA),
    logrec_param = factor(rep(NA,2))
  ),
  hessian = TRUE,
  silent = TRUE,
  DLL = "sbar_TMBExports")

obj1$fn()==obj$fn() # should be 291399.3, same as from sbar

(opt <- nlminb(start=obj$par,objective=obj$fn,gradient=obj$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10)))

#this converges 
obj$gr(opt$par)
library(TMBhelper)
check<-NA
check<-fit_tmb(obj)
check$Convergence_check
check$max_gradient
check2<-Check_Identifiable(obj) #

obs.rep <- TMB::sdreport(obj)
obs.srep <- summary(obs.rep)
qest<-exp(obs.srep[rownames(obs.srep)=="logq","Estimate"]) #
qest #q should be 1e-6

stk_no<-(obs.srep[rownames(obs.srep)=="biomass","Estimate"])/X #
plot(stk_no~years,ylim = c(0,max(stk_no)))
lines(colSums(stock.n(stk)[f_range,]),col=2)
#looks a bit like a parabola!

##======================================
# Schnute B0 estimate run
#=====================================

params <- list(
  logq = log(1e-8),
  logB0 = log(5*max(catch_kg1)),
  logindex_sigma = log(0.1),
  logrec_param = log(c(1,1)),
  logitsigma = stats::qlogis(sigma_st),
  logrho=log(rho1),
  logW=log(W1),
  logf_calc = log(rep(0.5,40)),
  logcatch_sigma = log(0.1))

obj <- TMB::MakeADFun(
  data = c(model = "schnute_new_V2",data_tmb),
  parameters = params,
  map = list(
    logB0=factor(1),
    logindex_sigma = factor(1),
    logW = factor(NA),
    logrho = factor(NA),
    logitsigma = factor(NA),
    logcatch_sigma = factor(NA),
    logrec_param = factor(rep(NA,2))
  ),
  hessian = TRUE,
  silent = TRUE,
  DLL = "sbar_TMBExports")



(opt <- nlminb(start = obj$par,
               objective = obj$fn,
               gradient = obj$gr,
               control = list(trace = 1,
                              iter.max = 10000,
                              eval.max = 10000,
                              rel.tol = 1e-10)))

obj$gr(opt$par)#

obs.rep <- TMB::sdreport(obj)
obs.srep <- summary(obs.rep)
library(TMBhelper)
Check_Identifiable(obj)

qhat<-exp(obs.srep[rownames(obs.srep)=="logq","Estimate"])
qhat


stk_no_schnub0<-(obs.srep[rownames(obs.srep)=="biomass","Estimate"])/X #

plot(stk_no~years,ylim = c(0,max(stk_no)),type="b")
lines(stk_no_schnub0,type="b",col=3)
lines(colSums(stock.n(stk)[f_range,]),col=2)

##==============CSA##======================
#
catch.no<-colSums(catn_err, na.rm=T) #remember this is catch at age with error
obs<-matrix(NA,nrow=2,ncol=no.years)
att<-data.frame(survey=c(1,1),type=c(1,2))
selrec<-matrix(1,nrow=1,ncol=no.years)# this where you can input your own selctivity for recruits

obs[1,]<-c(index(idx)[2,])
obs[2,]<-colSums(index(idx)[pr_range,])

data_tmb <- list(
  obs_catch = c(catch.no),
  obs_ind = obs,
  indices_class = as.matrix(att),
  indices_ts = 0,
  sr =selrec)


params <- list(
  logitqhat = stats::qlogis(1e-8),
  log_surveycv = log(0.1),
  log_catchcv = log(0.1),
  logphat1 = log(4*max(catch.no)),
  logrhat = log(rep(2*max(catch.no), no.years)),
  logf_calc = log(rep(0.5,no.years)),
  lognmort = log(mort),
  logitsrx = stats::qlogis(rep(1,no.years))
)


obj <- TMB::MakeADFun(
  data = c(model = "csa",data_tmb),
  parameters = params,
  map = list(
    logphat1=factor(1),
    log_surveycv = factor(1),
    log_catchcv = factor(NA),
    lognmort = factor(NA),
    logitsrx = factor(rep(NA,no.years))#,
  ),
  hessian = TRUE,
  silent = TRUE,
  DLL = "sbar_TMBExports")



(opt <- nlminb(start = obj$par,
                   objective = obj$fn,
                   gradient = obj$gr,
                   control = list(trace = 1,
                                  iter.max = 10000,
                                  eval.max = 10000,
                                  rel.tol = 1e-10)))

obj$gr(opt$par)#

obs.rep <- TMB::sdreport(obj)
obs.srep <- summary(obs.rep)

library(TMBhelper)
Check_Identifiable(obj)
qhat<-plogis(obs.srep[rownames(obs.srep)=="logitqhat","Estimate"])
qhat


stk_no_csa<-(obs.srep[rownames(obs.srep)=="bhat","Estimate"]) #

plot(stk_no~years,ylim = c(0,max(stk_no)),type="b")
lines(stk_no_schnub0,type="b",col=3)
lines(colSums(stock.n(stk)[f_range,]),col=2)
lines(stk_no_csa,ylim = c(0,max(stk_no_csa)),type="b",col=4)
#at least csa is picking up the right trend!
