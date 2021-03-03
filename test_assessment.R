
devtools::install_github("lbatts/sbar")
library(sbar)
load("testdata.RData")
starty = 20
endy = 60
no.years=endy-starty +1
tp<-starty:endy

par(mfrow=c(1,2))
#model X -> Z progression with linear model
plot(y=Z[2:no.years],x=X[1:no.years-1],xlim=c(0,max(Z[2:no.years])),ylim=c(0,max(Z[2:no.years])),pch=19,xaxs="i",yaxs="i",bty="l",xlab=expression(paste("X"[t]," (kg)")),ylab=expression(paste("Z"[t+1]," (kg)")))
mod<-lm(Z[2:no.years]~X[1:no.years-1])
x=seq(0,100,by=0.01)
lines(x=x,y=(coef(mod)[1] + coef(mod)[2]*x),col=2)



legend("topleft", legend =expression(paste("Linear model prediction",(Z[t+1]==italic(W) + rho*X[t]))), col = c(2), lty = c(1),cex=.8,box.lty=0)


W1<-coef(mod)[1]
rho1<-coef(mod)[2]

#visualise - red lines are growth from X -> Z
plot(y=Y,x=tp,type="l",col=4,ylim=c(0,max(Z[2:no.years])),pch=19,xaxs="i",yaxs="i",bty="l",xlab="Year",ylab="Mean weight (kg)")
lines(y=Z,x=tp,col=2)
lines(y=X,x=tp,col=3)

xtmp1<-W1 + rho1*X

for(i in 1:no.years){
  lines(y=c(X[i],xtmp1[i]),x=c(tp[i],tp[i+1]),col=1,lty=2)
  
}

legend("topright", legend = c("Growth","Y (Recruits)", "Z (Previously exploited biomass)", "X (Total biomass)"), col = c(1,4,2,3), lty = c(2,1,1,1),cex=0.8,box.lty=0)

dev.off()

mean_wts<-matrix(NA,ncol=no.years,nrow=3)
mean_wts[1,]<-Y
mean_wts[2,]<-Z
mean_wts[3,]<-X
#check
plot(y=mean_wts[1,],x=tp,type="l",ylim=c(0,5))
lines(y=mean_wts[2,],x=tp,col=2)
lines(y=mean_wts[3,],x=tp,col=3)
legend("topright", legend = c("Y (recruit mean wt)", "Z (exploit. biomass mean wt)", "X Total biomass mean wt"), col = 1:3, lty = 1,cex=0.5)

no.surv=1
obs<-matrix(NA,nrow=no.surv,ncol=no.years)
obs[1,]<-index_3 # this index has 0.3 sdlog variability from true biomass

args(schnute_peb)

#save(Y,Z,X,catch_kg1,index_3,file="testdata.RData")
#start_B0 = 5*(max(catch_kg1))

args(schnute_peb)

tmp<-schnute_peb("y1_no_f",catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho1, W = W1 , start_q = 1e-08, start_indexsigma = 0.1,start_sigma = exp(-0.1), start_f_calc = 0.5, start_catchsigma = 0.1, start_B0 = 5*max(catch_kg1),fix_sigma = FALSE, fix_B0 = FALSE, fix_indexsigma = FALSE,fix_catchsigma = TRUE)


tmp$fn() ## this shoul be 70637.95


(opt1 <- nlminb(start=tmp$par,objective=tmp$fn,gradient=tmp$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10)))

opt1$objective # should be -89.14535


srep<-TMB::sdreport(tmp)
res<-summary(srep)

