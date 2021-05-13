library(dplyr)
library(tidyr)
#install.packages("FLa4a",repos="http://flr-project.org/R",dependencies=TRUE)

library(FLCore)
#install.packages('patchwork')
#library(patchwork)
library(sbar)
rm(list=ls())
#setwd("C:/Users/LukeB/Documents/sim_sbar")
setwd("/home/luke/Documents/sim_sbar")
K=0.1075
Linf=171
t0=-1e-6
ages<-seq(.5,to=20.5,by=1)
length<- Linf * (1-exp(-K*(ages-t0))) 
par_a<- 0.0000303; par_b <- 2.82
waa<-par_a*length^par_b

fit_waa <- lm(waa[2:ceiling(max(ages)/2)] ~ waa[1:(ceiling(max(ages)/2)-1)])
ubw_W <- an(coef(fit_waa)[1])
ubw_rho <- an(coef(fit_waa)[2])

plot(waa[-length(waa)], waa[-1])
abline(c(ubw_W, ubw_rho))

load("mon78_a4a.Rdata")
a4afit<-stock+fit1
rm(stock)
load("mon78_input.Rdata")

#checks

index(tun.sel$FR_IE_IBTS)[1:3,]-index(tun$FR_IE_IBTS)[1:3,]
index(tun.sel$IE_MONKSURVEY)[1:5,]-index(tun$IE_MONKSURVEY)[2:6,]
index(tun.sel$`SP-PORC`)[1:5,]-index(tun$`SP-PORC`)[3:7,]




no.years<-length(years)
starty <- 2003-1986+1
endy <-starty + no.years-1
years<-ac(2003:2018)
###plot mean weights etc

catchkg1<-colSums(catch.n(a4afit)[,years]*catch.wt(a4afit)[,years],na.rm=T)


cw_Y<-c(colSums(catch.n(a4afit)[1,years]*catch.wt(a4afit)[1,years])/colSums(catch.n(a4afit)[1,years]))
cw_Z<-c(colSums(catch.n(a4afit)[2:8,years]*catch.wt(a4afit)[2:8,years])/colSums(catch.n(a4afit)[2:8,years]))
cw_X<-c(colSums(catch.n(a4afit)[1:8,years]*catch.wt(a4afit)[1:8,years])/colSums(catch.n(a4afit)[1:8,years]))



dev.off()
plot(y=cw_Y,x=years,type="l",ylim=c(0,5))

lines(y=cw_Z,x=years,col=2)
lines(y=cw_X,x=years,col=3)

legend("topright", legend = c("Y (recruit mean wt)", "Z (exploit. biomass mean wt)", "X Total biomass mean wt"), col = 1:3, lty = 1,cex=0.5)

plot(y=cw_Z[2:no.years],x=cw_X[1:no.years-1],xlim=c(0,max(cw_Z)))
mod<-lm(cw_Z[2:no.years]~cw_X[1:no.years-1])
x=seq(0,100,by=0.01)
lines(x=x,y=(coef(mod)[1] + coef(mod)[2]*x))
cw_W<-coef(mod)[1]
cw_rho<-coef(mod)[2]




plot(cw_X~years,type="l", lty=2,col=1, ylim=c(0,3))
lines(cw_Z~years,col=1)

xtmp1<-cw_W + cw_rho*cw_X

for(i in 1:no.years){
  lines(y=c(cw_X[i],xtmp1[i]),x=c(years[i],years[i+1]),col=2,lty=2)
  
}



cw_mean_wts<-matrix(NA,ncol=no.years,nrow=3)
cw_mean_wts[1,]<-cw_Y
cw_mean_wts[2,]<-cw_Z
cw_mean_wts[3,]<-cw_X

#use catch wt from stock as not set up plus group yet
ubw_Y<-c(colSums(index(tun$FR_IE_IBTS)[1,years]*catch.wt(stock)[1,years],na.rm=T)/colSums(index(tun$FR_IE_IBTS)[1,years],na.rm=T))
ubw_Z<-c(colSums(index(tun$FR_IE_IBTS)[2:10,years]*catch.wt(stock)[2:10,years],na.rm=T)/colSums(index(tun$FR_IE_IBTS)[2:10,years],na.rm=T))
ubw_X<-c(colSums(index(tun$FR_IE_IBTS)[1:8,years]*catch.wt(a4afit)[1:8,years],na.rm=T)/colSums(index(tun$FR_IE_IBTS)[1:8,years],na.rm=T))


dev.off()
plot(y=ubw_Y,x=years,type="b",ylim=c(0,3))

lines(y=ubw_Z,x=years,col=2,type="b")
lines(y=ubw_X,x=years,col=3,type="b")


lines(y=cw_Y,x=years,col=1)
lines(y=cw_Z,x=years,col=2)
lines(y=cw_X,x=years,col=3)
legend("topright", legend = c("Y (recruit mean wt)", "Z (exploit. biomass mean wt)", "X Total biomass mean wt"), col = 1:3, lty = 1,cex=0.5)

# plot(y=Z[2:no.years],x=X[1:(no.years-1)],xlim=c(0,max(Z,na.rm=T)),ylim=c(0,max(Z,na.rm=T)))
# mod<-lm(Z[2:no.years]~X[1:(no.years-1)])
# x=seq(0,100,by=0.01)
# lines(x=x,y=(coef(mod)[1] + coef(mod)[2]*x))
# lines(x=x,y=x,col=2)
# 
# W2<-coef(mod)[1]
# rho2<-coef(mod)[2]

plot(ubw_X~years,type="l", lty=2,col=1, ylim=c(0,6))
lines(ubw_Z~years,col=1)

xtmp1<-ubw_W + ubw_rho*ubw_X

for(i in 1:no.years){
  lines(y=c(ubw_X[i],xtmp1[i]),x=c(years[i],years[i+1]),col=2,lty=2)
  
}

ubw_mean_wts<-matrix(NA,ncol=no.years,nrow=3)
ubw_mean_wts[1,]<-ubw_Y
ubw_mean_wts[2,]<-ubw_Z
ubw_mean_wts[3,]<-ubw_X

#give 2017 an average of previous three years and year
ubw_mean_wts[,15]<-rowSums(ubw_mean_wts[,c(12:14,16)])/4

plot(y=ubw_mean_wts[1,],x=years,type="l",ylim=c(0,5))
lines(y=ubw_mean_wts[2,],x=years,col=2)
lines(y=ubw_mean_wts[3,],x=years,col=3)

lines(y=cw_mean_wts[2,],x=years,col=2,lty=2)
lines(y=cw_mean_wts[3,],x=years,col=3,lty=2)
lines(y=cw_mean_wts[1,],x=years,col=1,lty=2)

legend("topright", legend = c("Y (recruit mean wt)", "Z (exploit. biomass mean wt)", "X Total biomass mean wt"), col = 1:3, lty = 1,cex=0.5)
dev.off()

#compare W and rho
cw_W;ubw_W
cw_rho;ubw_rho

plot(y=cw_Z[2:no.years],x=cw_X[1:(no.years-1)],xlim=c(0,max(c(cw_Z,ubw_Z),na.rm=T)),ylim=c(0,max(c(cw_Z,ubw_Z),na.rm=T)),col=2)

points(y=ubw_Z[2:no.years],x=ubw_X[1:(no.years-1)])
x=seq(0,100,by=0.01)
lines(x=x,y=(ubw_W + ubw_rho*x))
lines(x=x,y=(cw_W + cw_rho*x),col=2)
lines(x=x,y=x,col=3,lty=2)

#points(x=W1/(1-rho1),y=W1/(1-rho1),col="blue",pch=19)


#==========================================================================================
#ibts.whole$CPUE.bio

#============================================================================================

###check these are simailr to a4a 
#load("mon78_a4a.RData")

#the a4a fit uses as tuning indices:
#ages 0-2 for FR_IE_IBTS
#ages 1-5 for monk and this is alos offset by a year and timing is set at 1(ratrher than 0.125)
#ages 2-6 for SP-PORC


#=========================================================================================
#   Run SChnute 2
#=============================================================================================
library(reshape2)
tmp<-harvest(a4afit)
tmp<-melt(tmp)

ggplot(tmp,aes(y=value,x=year,group=age,col=factor(age)))+geom_point()
ggplot(tmp,aes(y=value,x=age))+geom_point()

ccplot(data~age, data=FLCohort(stock@catch.n), type="l")##selectivity of age 1 is low indcates we are violating key assumption of model
mon_y<-ac(2006:2018)
mon_a<-ac(0:8)
sp_y<-ac(2003:2018)

no.surv=3
obs<-matrix(NA,nrow=no.surv,ncol=no.years)
obs[1,]<-c(colSums(index(tun$FR_IE_IBTS)[,years]*catch.wt(stock)[,years],na.rm=T))
obs[2,4:16] <-c(colSums(index(tun$IE_MONKSURVEY)[mon_a,mon_y]*catch.wt(stock)[mon_a,mon_y],na.rm=T))
obs[3,]<-c(colSums(index(tun$`SP-PORC`)[,sp_y]*catch.wt(stock)[,sp_y],na.rm=T))

obs[obs==0]<-NA

catch_kg1<-c(1e3*catch(a4afit)[,years])
par(mfrow=c(2,2))
plot(catch_kg1~years,col=2,type="b",main="catch (kg)")#,ylim=c(0,15000))
lines(1e3*c(catch(a4afit)[,years])~years,col=3)#preidcted from a4a


plot(obs[1,]~years,type="b",main="IBTS")
plot(obs[2,]~years,type="b",main="monk")
plot(obs[3,]~years,type="b",main="SP-porc")



nu=0

obj1<-schnute_orig(version = 2,catchkg = catch_kg1, indiceskg = obs, ts = c(0.875,1,0.875), mwts = cw_mean_wts, tsp = 0, rho = cw_rho, W = cw_W , start_q = c(2e-8,2e-8,2e-8), start_indexsigma = c(0.1,0.1,0.1), start_sigma = exp(-0.25), fix_sigma = TRUE, fix_indexsigma = F,ind_l_wt=c(1,1,1))

obj1$fn()
# 
opt1 <- nlminb(start=obj1$par,objective=obj1$fn,gradient=obj1$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))  
# 
obj1$gr(opt1$par)
library(TMBhelper)
check<-NA
check<-fit_tmb(obj1) # cant use when using lower and upper bounds
check$Convergence_check
check$max_gradient

check2<-Check_Identifiable(obj1) # 

#obs.rep <- 
obs.srep1 <- summary(TMB::sdreport(obj1))
dev.off()

plot(1000*c(colSums(a4afit@stock.n)),type="b",ylim=c(0,max(1000*c(colSums(a4afit@stock.n)))))  
plot(obs.srep1[row.names(obs.srep1)=="biomass","Estimate"],col=2,type="b")


obj2<-schnute_obserror(version=2,catchkg = catch_kg1, indiceskg = obs, ts = c(0.875,1,0.875), mwts = cw_mean_wts, tsp = 0, rho = cw_rho, W = cw_W ,  start_indexsigma = c(0.1,0.1,0.1),
                       start_sigma = exp(-0.25),start_q = rep(2e-8,3), start_f_calc = 0.5, start_catchsigma = 0.1,
                       fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                       fix_catchsigma = T,ind_l_wt=c(1,1,1))

obj2$fn()

opt2 <- nlminb(start=obj2$par,objective=obj2$fn,gradient=obj2$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))  


obj2$gr(opt2$par)
library(TMBhelper)
check<-NA
check<-fit_tmb(obj2) # cant use when using lower and upper bounds
check$Convergence_check
check$max_gradient

check2<-Check_Identifiable(obj2) # 

#obs.rep <- 
obs.srep2 <- summary(TMB::sdreport(obj2))

plot(1000*c(colSums(a4afit@stock.n[,years],na.rm=T)),type="b",ylim=c(0,max(1000*c(colSums(a4afit@stock.n[,years],na.rm=T)))))  
lines(obs.srep2[row.names(obs.srep2)=="N","Estimate"],col=2,type="b")


plot(1000*c(colSums(a4afit@stock.n[,years]*a4afit@catch.wt[,years],na.rm=T)),type="b",ylim=c(0,max(1000*c(colSums(a4afit@stock.n[,years]*a4afit@catch.wt[,years],na.rm=T)))))  

lines(obs.srep2[row.names(obs.srep2)=="biomass","Estimate"],col=2,type="b")




obj3<-schnute_orig(version = 2,catchkg = catch_kg1, indiceskg = obs, ts = c(0.875,1,0.875), mwts = ubw_mean_wts, tsp = 0, rho = ubw_rho, W = ubw_W , start_q = c(2e-8,2e-8,2e-8), start_indexsigma = c(0.1,0.1,0.1), start_sigma = exp(-0.25), fix_sigma = T, fix_indexsigma = FALSE,ind_l_wt=c(1,1,1))

obj3$fn()


opt3 <- nlminb(start=obj3$par,objective=obj3$fn,gradient=obj3$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))  


obj3$gr(opt3$par)
library(TMBhelper)
check<-NA
check<-fit_tmb(obj3) # cant use when using lower and upper bounds
check$Convergence_check
check$max_gradient

check3<-Check_Identifiable(obj3) # 

#obs.rep <- 
obs.srep3 <- summary(TMB::sdreport(obj3))

plot(1000*c(colSums(a4afit@stock.n[,years],na.rm=T)),type="b",ylim=c(0,max(obs.srep3[row.names(obs.srep3)=="N","Estimate"],1000*c(colSums(a4afit@stock.n[,years],na.rm=T)))))  
lines(obs.srep3[row.names(obs.srep3)=="N","Estimate"],col=2,type="b")


plot(1000*c(colSums(a4afit@stock.n[,years]*a4afit@catch.wt[,years],na.rm=T)),type="b",ylim=c(0,max(obs.srep3[row.names(obs.srep3)=="biomass","Estimate"],1000*c(colSums(a4afit@stock.n[,years]*a4afit@catch.wt[,years],na.rm=T)))))  
lines(obs.srep3[row.names(obs.srep3)=="biomass","Estimate"],col=2,type="b")



obj4<-schnute_obserror(version=2,catchkg = catch_kg1, indiceskg = obs, ts = c(0.875,1,0.875), mwts = ubw_mean_wts, tsp = 0, rho = ubw_rho, W = ubw_W ,  start_indexsigma = c(0.1,0.1,0.1),
                       start_sigma = exp(-0.25),start_q = rep(2e-8,3), start_f_calc = 0.5, start_catchsigma = 0.1,
                       fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                       fix_catchsigma = T,ind_l_wt=c(1,1,1))

obj4$fn()

opt4 <- nlminb(start=obj4$par,objective=obj4$fn,gradient=obj4$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))  


obj4$gr(opt4$par)
library(TMBhelper)
check<-NA
check<-fit_tmb(obj4) # cant use when using lower and upper bounds
check$Convergence_check
check$max_gradient

check4<-Check_Identifiable(obj4) # 

#obs.rep <- 
obs.srep4 <- summary(TMB::sdreport(obj4))


plot(1000*c(colSums(a4afit@stock.n[,years],na.rm=T)),type="b",ylim=c(0,max(obs.srep4[row.names(obs.srep4)=="N","Estimate"],1000*c(colSums(a4afit@stock.n[,years],na.rm=T)))))  
lines(obs.srep4[row.names(obs.srep4)=="N","Estimate"],col=2,type="b")


plot(1000*c(colSums(a4afit@stock.n[,years]*a4afit@catch.wt[,years],na.rm=T)),type="b",ylim=c(0,max(obs.srep4[row.names(obs.srep4)=="biomass","Estimate"],1000*c(colSums(a4afit@stock.n[,years]*a4afit@catch.wt[,years],na.rm=T))))) 

lines(obs.srep4[row.names(obs.srep4)=="biomass","Estimate"],col=2,type="b")







obs.srep<-obs.srep4
mean_wts<-ubw_mean_wts

qest<-plogis(obs.srep[rownames(obs.srep)=="logitq","Estimate"])
indexsigmaest<-exp(obs.srep[rownames(obs.srep)=="logindex_sigma","Estimate"])
#catchsigmaest<-exp(obs.srep[rownames(obs.srep)=="logcatch_sigma","Estimate"])
B0<-exp(obs.srep[rownames(obs.srep)=="logB0","Estimate"])
sigmaest<-plogis(obs.srep[rownames(obs.srep)=="logitsigma","Estimate"])
rhoest<-exp(obs.srep[rownames(obs.srep)=="logrho","Estimate"])
West<-exp(obs.srep[rownames(obs.srep)=="logW","Estimate"])
recparam<-exp(obs.srep[rownames(obs.srep)=="logrec_param","Estimate"])

qest;indexsigmaest;B0; rhoest

recparam

biomass.tmb<-c(obs.srep[rownames(obs.srep)=="biomass","Estimate"])

plot(biomass.tmb,type="b")

ssb.tmb<-c(obs.srep[rownames(obs.srep)=="ssb","Estimate"])
catchpred.tmb<-(obs.srep[rownames(obs.srep)=="catch_pred","Estimate"])
rec.tmb<-(obs.srep[rownames(obs.srep)=="rec_bio","Estimate"])
recno.tmb<-(obs.srep[rownames(obs.srep)=="rec_no","Estimate"])
post_rec.tmb<-c(obs.srep[rownames(obs.srep)=="post_rec","Estimate"])
omega.tmb<-c(obs.srep[rownames(obs.srep)=="omega","Estimate"])
f_calc.tmb<-exp(obs.srep[rownames(obs.srep)=="logf_calc","Estimate"])
mu.tmb<-(obs.srep[rownames(obs.srep)=="mu","Estimate"])
xprog.tmb<-(obs.srep[rownames(obs.srep)=="xprog","Estimate"])


pred_surv<-exp(matrix(obs.srep[rownames(obs.srep) == "logpred_survey", "Estimate"], ncol = no.years,nrow=no.surv))

par(mfrow=c(3,1),mar=c(4,4,1,1),lwd=1.5,cex=0.65)

plot(y=obs[1,],x=years,col=1,type="b",main="index 1 ")

lines(y=pred_surv[1,],x=years,col=2,lty=2,type="b")

legend("topright",lty=c(1,2),col=c(1,2),legend=c("data","predicted"),cex=0.8,bty="n",seg.len=0.5)



plot(y=obs[2,],x=years,col=1,type="l",main="index 2 ")
lines(y=pred_surv[2,],x=years,col=2,lty=1,type="b")

legend("topleft",lty=c(1,2),col=c(1,2),legend=c("data","predicted"),cex=0.8,bty="n",seg.len=0.5)


plot(y=obs[3,],x=years,col=1,type="l",main="index 2 ")
lines(y=pred_surv[3,],x=years,col=2,lty=2,type="b")

legend("topleft",lty=c(1,2),col=c(1,2),legend=c("data","predicted"),cex=0.8,bty="n",seg.len=0.5)



plot(y=catch_kg1,ylim=c(0,max(catchpred.tmb)),x=years,main="catch",col=1,type="l",lty=1,lwd=1)
lines(y=catchpred.tmb,x=years,col=2,lty=2,lwd=1,type="b")
legend("topleft",lty=c(1,2),col=c(1,2),legend=c("data","predicted"),cex=0.8,bty="n",seg.len=0.5)



plot(y=catch_kg1/mean_wts[3,],ylim=c(0,max(catchpred.tmb/mean_wts[3,])),x=years,main="catch",col=1,type="l",lty=1,lwd=1)
lines(y=catchpred.tmb/mean_wts[3,],x=years,col=2,lty=2,lwd=1,type="b")
legend("topleft",lty=c(1,2),col=c(1,2),legend=c("data","predicted"),cex=0.8,bty="n",seg.len=0.5)


truef<-c(colSums(harvest(a4afit)[,years])/8)
f<-c((colSums(harvest(a4afit)[,years]*stock.n(a4afit)[,years])/colSums(stock.n(a4afit)[,years]))) #
#falt<-c((colSums(harvest(a4afit)*stock.n(a4afit)*catch.wt(a4afit))/colSums(stock.n(a4afit)*catch.wt(a4afit)))[years]) #

truerec<-c(stock.n(a4afit)[1,years]*catch.wt(a4afit)[1,years])
truerecno<-c(stock.n(a4afit)[1,years])
trueprevex<-c(colSums(stock.n(a4afit)[2:8,years]*catch.wt(a4afit)[2:8,years]))
trueprevexno<-c(colSums(stock.n(a4afit)[2:8,years]))#c(stock(hke_gsa_xy_tru
truebio<-c(colSums(stock.n(a4afit)[,years]*catch.wt(a4afit)[,years]))#c(stock(a4afit))
trueno<-c(colSums(stock.n(a4afit)[,years]))

par(mfrow=c(2,2),mar=c(4,4,1,1),lwd=1.5,cex=0.65)
plot(rec.tmb,col="red",type="l",main="estimated recuitment (biomass)")#,ylim=c(0,10000))
lines(1e3*truerec ,col="blue",lty=2,type="b")
legend("topleft",lty=c(1,2),col=c("red","blue"),legend=c("Schnute","a4a"),cex=0.8,bty="n",seg.len=2)

plot(rec.tmb/mean_wts[1,],col="red",type="l",main="estimated recuitment (numbers)")#,ylim=c(0,100000))
lines(1e3*stock.n(a4afit)[1,years] ,col="blue",lty=2,type = "b")
legend("topleft",lty=c(1,2),col=c("red","blue"),legend=c("Schnute","a4a"),cex=0.8,bty="n",seg.len=2)

plot(rec.tmb,col="red",type="l",main="estimated recuitment (biomass)")#,ylim=c(0,10000))
lines(mean_wts[1,]*1e3*stock.n(a4afit)[1,years],col="blue",lty=2,type="b")
legend("topleft",lty=c(1,2),col=c("red","blue"),legend=c("Schnute","a4a"),cex=0.8,bty="n",seg.len=2)


plot(post_rec.tmb,col="red",type="l",main="estimated post recruits (biomass)",ylim=c(0,max(1e3*trueprevex ,c(post_rec.tmb,trueprevex))))
lines(1e3*trueprevex ,col="blue",lty=2)
legend("topright",lty=c(1,2),col=c("red","blue"),legend=c("Schnute","a4a"),cex=0.8,bty="n",seg.len=2)

plot(post_rec.tmb,col="red",type="l",main="estimated post recruits (biomass)",ylim=c(0,max((1e3*trueprevexno*mean_wts[2,]),c(post_rec.tmb,trueprevex))))
lines(1e3*trueprevexno*mean_wts[2,] ,col="blue",lty=2)
legend("topright",lty=c(1,2),col=c("red","blue"),legend=c("Schnute","a4a"),cex=0.8,bty="n",seg.len=2)





plot(post_rec.tmb/mean_wts[2,],col="red",type="l",main="estimated post recruits (numbers)")#,ylim=c(0,40000))
lines(1e3*trueprevexno ,col="blue",lty=2)
legend("topright",lty=c(1,2),col=c("red","blue"),legend=c("Schnute","a4a"),cex=0.8,bty="n",seg.len=2)



# plot(post_rec.tmb,col="red",type="l",main="estimated post recruits (biomas)")#,ylim=c(0,40000))
# lines(1e3*trueprevexno*mean_wts[2,] ,col="blue",lty=2)
# legend("topright",lty=c(1,2),col=c("red","blue"),legend=c("Schnute","a4a"),cex=0.8,bty="n",seg.len=2)

# 
# plot(whole$catch.av.wts,type="b")
# lines(c(colSums(catch.n(stock)[1:8,years]*catch.wt(stock)[1:8,years],na.rm=T)/colSums(catch.n(stock)[1:8,years],na.rm=T)),type="b",col=3)
# 







plot(biomass.tmb,col="red",type="l",main="estimated biomass",ylim=c(0,max((1e3*truebio),c(biomass.tmb,truebio))))#,ylim=c(0,50000))#,ylim=c(20000,100000))
lines(1e3*truebio ,col="blue",lty=2)
legend("topright",lty=c(1,2),col=c("red","blue"),legend=c("Schnute","a4a"),cex=0.8,bty="n",seg.len=2)

plot(biomass.tmb/mean_wts[3,],col="red",type="l",main="estimated numbers")#,ylim=c(0,550000))#,ylim=c(20000,100000))
lines(1e3*trueno ,col="blue",lty=2)
legend("topright",lty=c(1,2),col=c("red","blue"),legend=c("Schnute","a4a"),cex=0.8,bty="n",seg.len=2)


plot(f_calc.tmb,col="red",type="l",main="estimated f",ylim=c(.1,0.9))
lines(truef ,col="blue",lty=2)
#lines(falt ,col="green",lty=2)
lines(f ,col="darkblue",lty=2)

legend("topright",lty=c(1,2),col=c("red","blue"),legend=c("Schnute","a4a"),cex=0.8,bty="n",seg.len=2)

plot(mu.tmb,col="red",type="l",main="estimated f")#,ylim=c(20000,100000))



which(biomass.tmb ==max(biomass.tmb))
which(truebio ==max(truebio))




#save(obs.srep3,obs.srep4,f,truebio,trueno,trueprevex,trueprevexno,truerec,truerecno,catch_kg1,obs,file="mon78_schnute_res.Rdata")

B0
c(colSums(stock.n(a4afit)[,"2003"]*catch.wt(a4afit)[,"2003"]))*1e3

dev.off()

plot(biomass.tmb,col="red",type="l",main="estimated biomass",ylim=c(0,max((1e3*truebio),biomass.tmb)))#,ylim=c(20000,100000))
lines(1e3*truebio ,col="blue",lty=2)
legend("topright",lty=c(1,2),col=c("red","blue"),legend=c("Schnute","a4a"),cex=0.8,bty="n",seg.len=2)

#redo!!!
#save(list=ls(),file="FINAL_schnute_mon_sigmafix.RData")
# library(corrplot)
# colp <- colorRampPalette(rev(c("#67001F", "#B2182B", "#D6604D", 
#                                "#F4A582", "#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
#                                "#4393C3", "#2166AC", "#053061"))) ## intiutively think cold is negative and blue
# 
# corrplot(cov2cor(obs.rep$cov.fixed), method = "ellipse", type = "upper", col = colp(200), addCoef.col = "black", diag = T,number.cex=0.25,tl.cex = 0.6 )
# 
# dev.off()
# setEPS()
# postscript("C:/Users/LukeB/OneDrive - GMIT/Desktop/Desktop/DD_assesssment_ etc/v1MON_corpar_sel.eps", horizontal = FALSE, onefile = FALSE, paper = "special",width=6, height=6,pointsize=12)
# 
# dim(obs.rep$cov.fixed)
# head(obs.rep$cov.fixed)
# dms<-c(1:5,13,20:23)
# corrplot(cov2cor(obs.rep$cov.fixed[dms,dms]), method = "ellipse", type = "upper", col = colp(200), addCoef.col = "black", diag = T,number.cex=0.9,tl.cex = 0.6)
# dev.off()
# 



##======================================================================================================
#
#
#  CSA implementation
#======================================================================================================

catch.no<-c(colSums(1e3*catch.n(a4afit)[,years]))

no.surv = 4

obs<-matrix(NA,nrow=no.surv,ncol=no.years)

att<-data.frame(survey=c(1,1,2,3),type=c(1,2,2,2))
timing=c(0.875,1,0.875)
cv=c(0.1,0.1,0.1)

obs[1,]<-c(colSums(index(tun$FR_IE_IBTS)[1,years],na.rm=T))
obs[2,]<-c(colSums(index(tun$FR_IE_IBTS)[2:10,years],na.rm=T))
obs[3,4:16] <-c(colSums(index(tun$IE_MONKSURVEY)[mon_a,mon_y],na.rm=T))
obs[4,]<-c(colSums(index(tun$`SP-PORC`)[,sp_y],na.rm=T))
obs[obs==0]<-NA
selrec<-matrix(1,nrow=1,ncol=no.years)# number of rows is number of rec survey indices

obj<- csa(catch_n = catch.no, indices_no = obs, indices_att = att, ts = timing , selrec = selrec, start_q = rep(2e-8,3), start_surveycv = 0.1, start_catchcv = 0.1, start_nmort = 0.25, start_f_calc = 0.5,fix_nmort = T, fix_prec0 = F, fix_surveycv = F, fix_catchcv = T)

obj$fn()


opt <- nlminb(start=obj$par,objective=obj$fn,gradient=obj$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))

obs.srep<-summary(TMB::sdreport(obj))
obj$gr(opt$par)#
obj$gr(opt$par)[1:5]

qhat<-plogis(obs.srep[rownames(obs.srep)=="logitqhat","Estimate"])
fcalc<-exp(obs.srep[rownames(obs.srep)=="logf_calc","Estimate"])
nmort<-exp(obs.srep[rownames(obs.srep)=="lognmort","Estimate"])
rhat<-exp(obs.srep[rownames(obs.srep)=="logrhat","Estimate"])
phat1<-exp(obs.srep[rownames(obs.srep)=="logphat1","Estimate"])
phat<-(obs.srep[rownames(obs.srep)=="phat","Estimate"])
ccalc<-(obs.srep[rownames(obs.srep)=="c_calc","Estimate"])
survey_cv_est<-exp(obs.srep[rownames(obs.srep)=="log_surveycv","Estimate"])

qhat;phat1
nmort
survey_cv_est

pred_surv<-exp(matrix(obs.srep[rownames(obs.srep) == "logpred_survey", "Estimate"], ncol = no.years,nrow=no.surv))



sd_surv<-matrix(obs.srep[rownames(obs.srep) == "sdsurv", "Estimate"], ncol = no.years,nrow=no.surv)

par(mfrow=c(3,2),mar=c(4,4,1,1),lwd=1.5,cex=0.65)

plot(y=obs[1,],x=years,col=1,type="l",main="index 1 of recruits")
lines(y=pred_surv[1,],x=years,col=2,lty=2)
legend("topleft",lty=c(1,2),col=c(1,2),legend=c("data","predicted"),cex=0.8,bty="n",seg.len=0.5)

plot(x=years,y=obs[2,],col=1,type="l",main="index 1 of survival/growth biomass")
lines(x=years,y=pred_surv[2,],col=2,lty=2)
legend("topleft",lty=c(1,2),col=c(1,2),legend=c("data","predicted"),cex=0.8,bty="n",seg.len=0.5)

plot(x=years,y=obs[3,],col=1,type="l",main="index 2 of survival/growth biomass")
lines(x=years,y=pred_surv[3,],col=2,lty=2)
legend("topleft",lty=c(1,2),col=c(1,2),legend=c("data","predicted"),cex=0.8,bty="n",seg.len=0.5)


plot(y=obs[4,],x=years,col=1,type="l",main="index 3 of survival/growth")
lines(y=pred_surv[4,],x=years,col=2,lty=2)
legend("topleft",lty=c(1,2),col=c(1,2),legend=c("data","predicted"),cex=0.8,bty="n",seg.len=0.5)


plot(y=catch.no,x=years,col=1,type="l",main="catch")
lines(y=ccalc,x=years,col=2,lty=2)
legend("topleft",lty=c(1,2),col=c(1,2),legend=c("data","predicted"),cex=0.8,bty="n",seg.len=0.5)


dev.off()

plot(y=trueno*1e3,x=years,type="l")#,ylim=c(60000,160000))
lines(y=(phat+rhat),x=years,col=2,lty=2)
legend("topleft",lty=c(1,2),col=c(1,2),legend=c("a4a abundance","predicted abundance from CSA"),cex=0.8,bty="n",seg.len=0.5)

plot(y=trueprevexno*1e3,x=years,type="l")
lines(y=(phat),x=years,col=2,lty=2)
legend("topleft",lty=c(1,2),col=c(1,2),legend=c("a4a ages 1 +","predicted post-rec from CSA"),cex=0.8,bty="n",seg.len=0.5)


plot(y=truerecno*1e3,x=years,type="l")#,ylim=c(10000,90000))
lines(y=(rhat),x=years,col=2,lty=2)
legend("topleft",lty=c(1,2),col=c(1,2),legend=c("a4a age 0","predicted rec from CSA"),cex=0.8,bty="n",seg.len=0.5)


##overall f comparison

plot(fcalc,x=years,type="l",ylim=c(0,0.5))
lines(y=(f),x=years,col=2,lty=2)

lines(y=(truef),x=years,col=2,lty=2)

#redo!!!
#save(catch.no,obs.srep,obs,file="mon78_csa_res.RData")

