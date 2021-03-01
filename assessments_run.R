library(TMBhelper)
library(doParallel)
rm(list=ls())

iters=1
load("sims.RData")
#i=sims[[1]]

# no_cores <- detectCores() - 1  
# registerDoParallel(cores=no_cores)  
# cl <- makeCluster(no_cores)  
# clusterEvalQ(cl = cl, expr = {library(sbar);library(FLa4a)})

#result <- parLapply(cl, 10:10000, getPrimeNumbers)  

t<-NA
t[1]<-Sys.time()

#assess<-parLapply(cl,sims[1:3], function(i) {
  assess2<-lapply(sims[101:150], function(i) {
    
  
  # Normal error with CV=20%
  # x$catchnumE <- FLQuant(aperm(apply(catchnum, 1:5, function(x) rnorm(iters, x, x* 0.20)),
  #                           c(2,3,4,5,6,1)), dimnames=dimnames(catchnum))
  assessments <- list()
  catchnum <- catch.n(i$stk)
  assessments$catchnum <- catchnum
  assessments$catchnumE <- FLQuant(apply(catchnum, 1:5, function(i) rnorm(iters, i, i* 0.20)), dimnames=dimnames(catchnum))
  
  ##set mean weights for schnute model
  pr_range <-2:(i$stk@range["max"]+1)
  f_range <- 1:(i$stk@range["max"]+1)
  no.years <-max(an(dimnames(i$stk)$year))
  tp <-an(dimnames(i$stk)$year)
  
  # plot(i$stk)
  #   
   Y<-c(colSums(catch.wt(i$stk)[1,]*assessments$catchnumE[1,])/colSums(assessments$catchnumE[1,]))
   Z<-c(colSums(catch.wt(i$stk)[pr_range,]*assessments$catchnumE[pr_range,])/colSums(assessments$catchnumE[pr_range,]))
   X<-c(colSums(catch.wt(i$stk)[f_range,]*assessments$catchnumE[f_range,])/colSums(assessments$catchnumE[f_range,]))
  # 
  # 
  # par(mfrow=c(1,2))
  # #model X -> Z progression with linear model
  # plot(y=Z[2:no.years],x=X[1:no.years-1])#,xlim=c(0,max(Z[2:no.years])),ylim=c(0,max(Z[2:no.years])),pch=19,xaxs="i",yaxs="i",bty="l",xlab=expression(paste("X"[t]," (kg)")),ylab=expression(paste("Z"[t+1]," (kg)")))
   mod<-lm(Z[2:no.years]~X[1:no.years-1])
  # x=seq(0,100,by=0.01)
  # lines(x=x,y=(coef(mod)[1] + coef(mod)[2]*x),col=2)
  # 
  # 
  # 
  # legend("topleft", legend =expression(paste("Linear model prediction",(Z[t+1]==italic(W) + rho*X[t]))), col = c(2), lty = c(1),cex=.8,box.lty=0)
  # 
  # 
   W1<-coef(mod)[1]
   rho1<-coef(mod)[2]
  # #visualise - red lines are growth from X -> Z
  # plot(y=Y,x=tp,type="l",col=4,ylim=c(0,max(Z[2:no.years])),pch=19,xaxs="i",yaxs="i",bty="l",xlab="Year",ylab="Mean weight (kg)")
  # lines(y=Z,x=tp,col=2)
  # lines(y=X,x=tp,col=3)
  # 
  # xtmp1<-W1 + rho1*X
  # 
  # for(i in 1:no.years){
  #   lines(y=c(X[i],xtmp1[i]),x=c(tp[i],tp[i+1]),col=1,lty=2)
  # 
  # }
  # 
  # legend("topright", legend = c("Growth","Y (Recruits)", "Z (Previously exploited biomass)", "X (Total biomass)"), col = c(1,4,2,3), lty = c(2,1,1,1),cex=0.8,box.lty=0)
  # 
  # #dev.off()

  mean_wts<-matrix(NA,ncol=no.years,nrow=3)
  mean_wts[1,]<-Y
  mean_wts[2,]<-Z
  mean_wts[3,]<-X
  #check
  # plot(y=mean_wts[1,],x=tp,type="l",ylim=c(0,5))
  # lines(y=mean_wts[2,],x=tp,col=2)
  # lines(y=mean_wts[3,],x=tp,col=3)
  # legend("topright", legend = c("Y (recruit mean wt)", "Z (exploit. biomass mean wt)", "X Total biomass mean wt"), col = 1:3, lty = 1,cex=0.5)
  # 
  catch_kg1<-colSums(assessments$catchnumE*catch.wt(i$stk))
  # plot(catch_kg1,type="b")
  # lines(i$stk@catch,col=2)
  index<-colSums(index(i$idx)*catch.wt(i$stk))
  obs<-matrix(NA,nrow=1,ncol=no.years)
  obs[1,]<-index # this index has 0.3 sdlog variability from true biomass
  
  #args(schnute_peb)
  #start_B0 = sb0
  sigma_st <- exp(-mean(m(i$stk)[,1],na.rm=T))
  
  obj<-schnute_peb("y1_no_f",catch = catch_kg1, indices = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho1, W = W1 , start_q = 1e-08, start_indexsigma = 0.1,
                   start_sigma = sigma_st, start_f_calc = 0.5, start_catchsigma = 0.1,
                   fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                   fix_catchsigma = T)
  
  
  if(!is.na(obj$fn())){
    
    
    (opt <- nlminb(start=obj$par,objective=obj$fn,gradient=obj$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10)))
    
    # obj$gr(opt$par)
    # obj$gr(opt$par)[1:3]#
    # check<-NA
    # check<-fit_tmb(obj) 
    # check$Convergence_check
    # check$max_gradient
    # 
    # #check2<-Check_Identifiable(obj) # 
    # 
    assessments$y1_no_f$obs.srep <- tryCatch({
     summary(TMB::sdreport(obj))
    },
    error=function(cond) {
      message(paste("URL does not seem to exist:", url))
      message("Here's the original error message:")
      message(cond)
      # Choose a return value in case of error
      return(NA)
    },
    warning=function(cond) {
      message(paste("URL caused a warning:", url))
      message("Here's the original warning message:")
      message(cond)
      # Choose a return value in case of warning
      return(NULL)
    }
    )
    
  }else{
    
    assessments$y1_no_f$obs.srep <- "try different starting pars"
   
  }
    
    ##==============CSA##======================
  
  catch.no<-colSums(assessments$catchnum, na.rm=T)
  
  no.surv = 2 # as survey is split into rec and post-rec
  no.years = length(catch.no)
  
  obs<-matrix(NA,nrow=no.surv,ncol=no.years)
  
  att<-data.frame(survey=c(1,1),type=c(1,2))
  sr<-matrix(1,nrow=1,ncol=no.years)# number of rows is number of rec survey indices
  
  
  rec_obs_no<-c(index(i$idx)[1,])
  postrec_obs_no<-colSums(index(i$idx)[2:21,])
  
  
  obs[1,]<-rec_obs_no
  obs[2,]<-postrec_obs_no
  
  #args(csa)
  #start_prec0 = 110000, start_rec = 50000
  
 # check sr!!!!!!!
  obj<- csa(catch_n = catch.no, indices=obs, indices_att = att, ts = 0 , sr = sr, start_q = 1e-8, start_surveycv = 0.1, start_catchcv = 0.1, start_nmort = mean(m(i$stk)[,1],na.rm=T), start_f_calc = 0.5,fix_nmort = T, fix_prec0 = F, fix_surveycv = F, fix_catchcv = T)
  
  
  if(!is.na(obj$fn())){
    
  
  (opt1 <- nlminb(start = obj$par,
                  objective = obj$fn,
                  gradient = obj$gr,
                  control = list(iter.max = 10000,
                                 eval.max = 10000,
                                 rel.tol = 1e-10)))
 
    obs.rep <- TMB::sdreport(obj)
    assessments$csa$obs.srep <- summary(obs.rep)
    
    
  }else{
    
    assessments$csa$obs.srep <- "try different starting pars"
    
  }
  
  
  
  print(names(assessments))
  return(assessments)

    })

t[2]<-Sys.time()
#stopCluster(cl)  

(t[2]-t[1])/60
