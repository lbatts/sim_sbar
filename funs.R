#===================================
# Functions for stock simulation
# Author : LB
#Distributed under GPL-3 or later
#Some code here is modified from https://github.com/iagomosqueira/stocksims, which was distributed under GPL-2 or later
#Feb 2021
#===================================
luke_gisla <-function(linf, k,len){
  
  logM<-0.55 - (1.61*log(len)) + (1.44*log(linf)) + log(k)
  return(exp(logM))
}

# ar1rlnorm {{{
ar1rlnorm <- function(rho, years, iters=1, margSD=0.6) {
  
  n <- length(years)
  rhosq <- rho ^ 2
  
  res <- matrix(rnorm(n*iters, mean=0, sd=margSD), nrow=n, ncol=iters)
  res <- apply(res, 2, function(x) {
    for(i in 2:n)
      x[i] <- sqrt(rhosq) * x[i-1] + sqrt(1-rhosq) * x[i]
    return(exp(x))
  })
  
  return(FLQuant(array(res, dim=c(1,n,1,1,1,iters)),
                 dimnames=list(year=years, iter=seq(1, iters))))
}

dimna<-function(stk,yv){
  dimna<-dimnames(stk) 
  dimna$year<- ac(yv)
  tmp<-FLStock(FLQuant(NA,dimnames=dimna))
  units(tmp)$harvest <-"f"
  tmp[,,,,,]<-trim(stk,year= 2:(max(yv)+1))[,,,,,]
  return(tmp)
}



caterror <- function(x){
  stk <- x$stk
  idx<- x$idx
  eps_c <- FLQuant(rlnorm(n = prod(dim(stk)), sdlog = 0.1),
                   dim = dim(stk), dimnames = dimnames(landings.n(stk)))
  catch.n(idx) <- catch.n(stk) * eps_c
  catch.wt(idx) <- catch.wt(stk)
  index.q(idx) <- stock.n(stk)*eps_c # just use these slots for storage 
  #effort(idx) <- quantSums(catch.n(idx)*catch.wt(stk))
  x$idx<-idx
  x$stk<-stk
  
  pr_range <-2:(stk@range["max"]+1)
  f_range <- 1:(stk@range["max"]+1)
  no.years <-max(an(dimnames(stk)$year))
  tp <-an(dimnames(stk)$year)
  iters<-dim(stk)[6]
  
  mwts_mat<- array(NA,dim = c(3,no.years,iters))
  
  Y<-c(quantSums(catch.wt(stk)[1,]*catch.n(idx)[1,])/quantSums(catch.n(idx)[1,]))
  Z<-c(quantSums(catch.wt(stk)[pr_range,]*catch.n(idx)[pr_range,])/quantSums(catch.n(idx)[pr_range,]))
  XX<-c(quantSums(catch.wt(stk)[f_range,]*catch.n(idx)[f_range,])/quantSums(catch.n(idx)[f_range,]))
  
  mwts_mat[1,tp,1:iters]<-Y
  mwts_mat[2,tp,1:iters]<-Z
  mwts_mat[3,tp,1:iters]<-XX
  
  x$nocut$mwts <-mwts_mat
  
  mwts_mat<- array(NA,dim = c(3,no.years,iters))
  
  Y<-c(quantSums(catch.wt(stk)[1,]*index.q(idx)[1,])/quantSums(index.q(idx)[1,]))
  Z<-c(quantSums(catch.wt(stk)[pr_range,]*index.q(idx)[pr_range,])/quantSums(index.q(idx)[pr_range,]))
  XX<-c(quantSums(catch.wt(stk)[f_range,]*index.q(idx)[f_range,])/quantSums(index.q(idx)[f_range,]))
  
  mwts_mat[1,tp,1:iters]<-Y
  mwts_mat[2,tp,1:iters]<-Z
  mwts_mat[3,tp,1:iters]<-XX
  
  x$nocut$mwts_nosel <-mwts_mat
  
  
  return(x)
}

#x<-res[[3]]
catlowerror <- function(x){
  stk <- x$stk
  idx<- x$idx
  eps_c <- FLQuant(rlnorm(n = prod(dim(stk)), sdlog = 0.0001),
                   dim = dim(stk), dimnames = dimnames(landings.n(stk)))
  catch.n(idx) <- catch.n(stk) * eps_c
  catch.wt(idx) <- catch.wt(stk)
  index.q(idx) <- stock.n(stk)*eps_c # just use these slots for storage 
  #effort(idx) <- quantSums(catch.n(idx)*catch.wt(stk))
  x$idx<-idx
  x$stk<-stk
  
  pr_range <-2:(stk@range["max"]+1)
  f_range <- 1:(stk@range["max"]+1)
  no.years <-max(an(dimnames(stk)$year))
  tp <-an(dimnames(stk)$year)
  iters<-dim(stk)[6]
  
  mwts_mat<- array(NA,dim = c(3,no.years,iters))
  
  Y<-c(quantSums(catch.wt(stk)[1,]*catch.n(idx)[1,])/quantSums(catch.n(idx)[1,]))
  Z<-c(quantSums(catch.wt(stk)[pr_range,]*catch.n(idx)[pr_range,])/quantSums(catch.n(idx)[pr_range,]))
  XX<-c(quantSums(catch.wt(stk)[f_range,]*catch.n(idx)[f_range,])/quantSums(catch.n(idx)[f_range,]))
  
  mwts_mat[1,tp,1:iters]<-Y
  mwts_mat[2,tp,1:iters]<-Z
  mwts_mat[3,tp,1:iters]<-XX
  
  x$nocut$mwts <-mwts_mat
  
  mwts_mat<- array(NA,dim = c(3,no.years,iters))
  
  Y<-c(quantSums(catch.wt(stk)[1,]*index.q(idx)[1,])/quantSums(index.q(idx)[1,]))
  Z<-c(quantSums(catch.wt(stk)[pr_range,]*index.q(idx)[pr_range,])/quantSums(index.q(idx)[pr_range,]))
  XX<-c(quantSums(catch.wt(stk)[f_range,]*index.q(idx)[f_range,])/quantSums(index.q(idx)[f_range,]))
  
  mwts_mat[1,tp,1:iters]<-Y
  mwts_mat[2,tp,1:iters]<-Z
  mwts_mat[3,tp,1:iters]<-XX
  
  x$nocut$mwts_nosel <-mwts_mat
  
  
  return(x)
}



caterror_cut <- function(x){
  stk <- x$stk
  idx<- x$idx
  
  pr_range <-3:(stk@range["max"]+1)
  f_range <- 2:(stk@range["max"]+1)
  no.years <-max(an(dimnames(stk)$year))
  tp <-an(dimnames(stk)$year)
  iters<-dim(stk)[6]
  
  mwts_mat<- array(NA,dim = c(3,no.years,iters))
  
  Y<-c(quantSums(catch.wt(stk)[2,]*catch.n(idx)[2,])/quantSums(catch.n(idx)[2,]))
  Z<-c(quantSums(catch.wt(stk)[pr_range,]*catch.n(idx)[pr_range,])/quantSums(catch.n(idx)[pr_range,]))
  XX<-c(quantSums(catch.wt(stk)[f_range,]*catch.n(idx)[f_range,])/quantSums(catch.n(idx)[f_range,]))
  
  mwts_mat[1,tp,1:iters]<-Y
  mwts_mat[2,tp,1:iters]<-Z
  mwts_mat[3,tp,1:iters]<-XX
  
  x$cut$mwts <-mwts_mat
  
  mwts_mat<- array(NA,dim = c(3,no.years,iters))
  
  Y<-c(quantSums(catch.wt(stk)[2,]*index.q(idx)[2,])/quantSums(index.q(idx)[2,]))
  Z<-c(quantSums(catch.wt(stk)[pr_range,]*index.q(idx)[pr_range,])/quantSums(index.q(idx)[pr_range,]))
  XX<-c(quantSums(catch.wt(stk)[f_range,]*index.q(idx)[f_range,])/quantSums(index.q(idx)[f_range,]))
  
  mwts_mat[1,tp,1:iters]<-Y
  mwts_mat[2,tp,1:iters]<-Z
  mwts_mat[3,tp,1:iters]<-XX
  
  x$cut$mwts_nosel <-mwts_mat
  
  return(x)
}

#xx<-res[[3]]

assessments<- function(xx){
  stk<-xx$stk
  pr_range <-2:(stk@range["max"]+1)
  f_range <- 1:(stk@range["max"]+1)
  no.years <-max(an(dimnames(stk)$year))
  tp <-an(dimnames(stk)$year)
  iters<-dim(stk)[6]
  mort<-mean(m(stk),na.rm=T)
  
  st_cthsigma <- 0.1
  st_survsig <- 0.1
  
  waa <- an(iterMeans(catch.wt(stk))[,"1"]) # #according to equation at the start of Schnute
  #fit <- lm(w[2:11] ~ w[1:10])# #according to equation at the start of Schnute
  fit_waa <- lm(waa[2:ceiling(max(f_range)/2)] ~ waa[1:(ceiling(max(f_range)/2)-1)])
  
  xx$nocut$schnub0 <- xx$nocut$schnualt <- xx$nocut$schnub02 <- xx$nocut$schnualt2 <- xx$nocut$csa <- array(NA,c(7,no.years,iters))
  
  xx$nocut$schnub0_par <- xx$nocut$schnualt_par <- xx$nocut$schnub02_par <- xx$nocut$schnualt2_par <-xx$nocut$csa_par <- array(NA,c((7+no.years),2,iters))
  
  xx$nocut$schnub0_errors <- xx$nocut$schnualt_errors <- xx$nocut$schnub02_errors <- xx$nocut$schnualt2_errors <-xx$nocut$csa_errors <- array(NA,c(1,2,iters))
  
  
  xx$nocut$waa_par$W <- an(coef(fit_waa)[1])
  xx$nocut$waa_par$rho <- an(coef(fit_waa)[2])
  
  xx$nocut$was_par<-array(NA,c(2,1,iters))
  
  xx$nocut$rv_par <- array(NA,c(10,2,iters))
  xx$nocut$rv <- array(NA,c(7,no.years,iters))
  
  xx$nocut$rv_par[1,1,]<-1e-6
  xx$nocut$rv_par[2,1,]<-NA
  xx$nocut$rv_par[3,1,]<-quantSums(stock.n(stk)[pr_range,1])
  xx$nocut$rv_par[4,1,]<-quantSums(stock.n(stk)[f_range,1]*catch.wt(stk)[f_range,1])
  xx$nocut$rv_par[5,1,]<-quantMeans(harvest(stk))[,1]
  xx$nocut$rv_par[6,1,]<-quantMeans(harvest(stk))[,(no.years/2)]
  xx$nocut$rv_par[7,1,]<-quantMeans(harvest(stk))[,no.years]
  xx$nocut$rv_par[8,1,]<-quantSums(stock.n(stk)[f_range,1])
  xx$nocut$rv_par[9,1,]<-quantSums(stock.n(stk)[f_range,(no.years/2)])
  xx$nocut$rv_par[10,1,]<-quantSums(stock.n(stk)[f_range,no.years])
  
  
  xx$nocut$rv[1,,]<-stock.n(stk)[1,]*catch.wt(stk)[1,]
  xx$nocut$rv[2,,]<-quantSums(stock.n(stk)[pr_range,]*catch.wt(stk)[pr_range,])
  xx$nocut$rv[3,,]<-quantSums(stock.n(stk)[f_range,]*catch.wt(stk)[f_range,])
  xx$nocut$rv[4,,]<-stock.n(stk)[1,]
  xx$nocut$rv[5,,]<-quantSums(stock.n(stk)[pr_range,])
  xx$nocut$rv[6,,]<-quantSums(stock.n(stk)[f_range,])
  xx$nocut$rv[7,,]<-quantMeans(harvest(stk))
  
  #plot(stk)
  for (j in 1:iters){
    print(j)
    idx<-iter(xx$idx,j)
    mean_wts<-xx$nocut$mwts[,,j]
    
    #print("linearmod")
    mod<-stats::lm(mean_wts[2,2:no.years]~mean_wts[3,1:no.years-1])
    W1<-coef(mod)[1]
    rho1<-coef(mod)[2]
    xx$nocut$was_par[1:2,,j] <-c(W1,rho1)
    
    
    if(suppressWarnings(!is.nan(log(rho1)))){
      catch_kg1<-c(colSums(catch.n(idx)*catch.wt(idx)))
      index<-colSums(index(idx)*catch.wt(idx))
      obs<-matrix(NA,nrow=1,ncol=no.years)
      obs[1,]<-index # this index has 0.3 sdlog variability from true biomass
      sigma_st <- exp(-mort)
      
      ##======================================
      # Schnute alt runs with was
      #=====================================
      #print("bf 1 sch")
      obj1<-schnute_orig(version = 2,catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho1, W = W1 , start_q = 1e-08, start_indexsigma = 0.1, start_sigma = sigma_st, fix_sigma = TRUE, fix_indexsigma = FALSE,ind_l_wt=1)
      print("af 1 sch")
      if(!is.na(obj1$fn())){
        
        schnualt<- tryCatch({
          
          
          opt1 <- nlminb(start=obj1$par,objective=obj1$fn,gradient=obj1$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
          
          if(opt1$convergence==0){
            
            summary(TMB::sdreport(obj1))
            
          }else
            
            1
        },
        error=function(cond) {
          message(paste("Something not working for this iter:", j, "and", id))
          message("Here's the original error message:")
          message(cond)
          # Choose a return value in case of error
          return(NULL)
        }
        )
        
        
        
        
      }else{
        
        schnualt <- "try different starting pars"
      }
      
      suppressWarnings(
        if(is.null(schnualt)){
          xx$nocut$schnualt_par[1:2,1,j]<-NA
          xx$nocut$schnualt_errors[1,1:2,j]<-c(1,"Error in optimisation")
          
        }else if(schnualt==1){
          xx$nocut$schnualt_par[1:2,1,j]<-NA
          xx$nocut$schnualt_errors[1,1:2,j]<-c(1,opt1$message)
          
        }else if(schnualt=="try different starting pars"){
          xx$nocut$schnualt_par[1:2,1,j]<-NA
          xx$nocut$schnualt_errors[1,1:2,j]<-c(1,"try different starting pars")
        }else{
          xx$nocut$schnualt_errors[1,1:2,j]<-c(0,opt1$message)
          xx$nocut$schnualt_par[1:2,,j]<-schnualt[c("qhat","index_sigma"),]
          
          xx$nocut$schnualt_par[8:(no.years+7),,j]<-schnualt[row.names(schnualt)=="N",]
          
          xx$nocut$schnualt[1,,j]<-schnualt[row.names(schnualt)=="rec_bio" ,"Estimate"]
          xx$nocut$schnualt[2,,j]<-schnualt[row.names(schnualt)=="post_rec" ,"Estimate"]
          xx$nocut$schnualt[3,,j]<-schnualt[row.names(schnualt)=="biomass" ,"Estimate"]
          xx$nocut$schnualt[4,,j]<-schnualt[row.names(schnualt)=="rec_no" ,"Estimate"]
          xx$nocut$schnualt[5,,j]<-schnualt[row.names(schnualt)=="PR" ,"Estimate"]
          xx$nocut$schnualt[6,,j]<-schnualt[row.names(schnualt)=="N" ,"Estimate"]
          #xx$nocut$schnualt[1,,j]<-schnualt[row.names(schnualt)=="f_calc" ,"Estimate"]
          
          
          
        })
      
      #print("after schnu alt")
      ##======================================
      # Schnute B0 estimate runs
      #=====================================
      print("bf 2 sch")
      obj2<-schnute_obserror(version=2,catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho1, W = W1 , start_q = 1e-08, start_indexsigma = 0.1,
                             start_sigma = sigma_st, start_f_calc = 0.5, start_catchsigma = st_cthsigma,
                             fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                             fix_catchsigma = T,ind_l_wt=1)
      print("af 2 sch")
      if(!is.na(obj2$fn())){
        
        schnub0<- tryCatch({
          
          
          opt2 <- nlminb(start=obj2$par,objective=obj2$fn,gradient=obj2$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
          
          if(opt2$convergence==0){
            
            summary(TMB::sdreport(obj2))
            
          }else
            
            1
          
        },
        error=function(cond) {
          message(paste("Something not working for this iter:", j, "and", id))
          message("Here's the original error message:")
          message(cond)
          # Choose a return value in case of error
          return(NULL)
        }
        )
        
        
        
        
      }else{
        
        schnub0 <- "try different starting pars"
      }
      
      suppressWarnings(
        if(is.null(schnub0)){
          
          xx$nocut$schnub0_errors[1,1:2,j]<-c(1,"Error in optimisation")
          xx$nocut$schnub0_par[1:3,1,j]<-NA
          
        }else if(schnub0==1){
          xx$nocut$schnub0_par[1:3,1,j]<-NA
          xx$nocut$schnub0_errors[1,1:2,j]<-c(1,opt2$message)
          
        }else if(schnub0=="try different starting pars"){
          xx$nocut$schnub0_par[1:3,1,j]<-NA
          xx$nocut$schnub0_errors[1,1:2,j]<-c(1,"try different starting pars")
        }else{
          xx$nocut$schnub0_errors[1,1:2,j]<-c(0,opt2$message)
          xx$nocut$schnub0_par[1:3,,j]<-schnub0[c("qhat","index_sigma","B0"),]
          
          xx$nocut$schnub0_par[5:7,,j]<-schnub0[row.names(schnub0)=="f_calc",][c(1,(no.years/2),no.years),]
          xx$nocut$schnub0_par[8:(no.years+7),,j]<-schnub0[row.names(schnub0)=="N",]
          
          
          xx$nocut$schnub0[1,,j]<-schnub0[row.names(schnub0)=="rec_bio" ,"Estimate"]
          xx$nocut$schnub0[2,,j]<-schnub0[row.names(schnub0)=="post_rec" ,"Estimate"]
          xx$nocut$schnub0[3,,j]<-schnub0[row.names(schnub0)=="biomass" ,"Estimate"]
          xx$nocut$schnub0[4,,j]<-schnub0[row.names(schnub0)=="rec_no" ,"Estimate"]
          xx$nocut$schnub0[5,,j]<-schnub0[row.names(schnub0)=="PR" ,"Estimate"]
          xx$nocut$schnub0[6,,j]<-schnub0[row.names(schnub0)=="N" ,"Estimate"]
          xx$nocut$schnub0[7,,j]<-schnub0[row.names(schnub0)=="f_calc" ,"Estimate"]
          
        })
      
    }else { xx$nocut$schnub0_par[1:3,1,j]<- xx$nocut$schnualt_par[1:2,1,j] <- NA  #this is end of if to negate against logrho errors
    xx$nocut$schnub0_errors[1,1:2,j] <- xx$nocut$schnualt_errors[1,1:2,j]<-c(1,"rho error")        }
    #===================================================================================================
    #
    # With waa W and rho
    #=========================================================================
    W <- xx$nocut$waa_par$W
    rho <- xx$nocut$waa_par$rho
    
    mean_wts<-xx$nocut$mwts_nosel[,,j]
    
    if(suppressWarnings(!is.nan(log(rho)))){
      
      ##======================================
      # Schnute alt runs
      #=====================================
      
      print("bf 1.2 sch")
      
      obj1.2<- schnute_orig(version = 2,catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho, W = W ,ind_l_wt=1, start_q = 1e-08, start_indexsigma = 0.1, start_sigma = sigma_st, fix_sigma = TRUE, fix_indexsigma = FALSE)
      
      print("af 1.2 sch")
      if(!is.na(obj1.2$fn())){
        
        schnualt2<- tryCatch({
          
          
          opt1.2 <- nlminb(start=obj1.2$par,objective=obj1.2$fn,gradient=obj1.2$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
          
          if(opt1.2$convergence==0){
            
            summary(TMB::sdreport(obj1.2))
            
          }else
            
            1
        },
        error=function(cond) {
          message(paste("Something not working for this iter:", j, "and", id))
          message("Here's the original error message:")
          message(cond)
          # Choose a return value in case of error
          return(NULL)
        }
        )
        
        
        
        
      }else{
        
        schnualt2 <- "try different starting pars"
      }
      
      suppressWarnings(
        if(is.null(schnualt2)){
          xx$nocut$schnualt2_par[1:2,1,j]<-NA
          xx$nocut$schnualt2_errors[1,1:2,j]<-c(1,"Error in optimisation")
          
        }else if(schnualt2==1){
          xx$nocut$schnualt2_par[1:2,1,j]<-NA
          xx$nocut$schnualt2_errors[1,1:2,j]<-c(1,opt1.2$message)
          
        }else if(schnualt2=="try different starting pars"){
          xx$nocut$schnualt2_par[1:2,1,j]<-NA
          xx$nocut$schnualt2_errors[1,1:2,j]<-c(1,"try different starting pars")
        }else{
          xx$nocut$schnualt2_errors[1,1:2,j]<-c(0,opt1.2$message)
          xx$nocut$schnualt2_par[1:2,,j]<-schnualt2[c("qhat","index_sigma"),]
          
          xx$nocut$schnualt2_par[8:(no.years+7),,j]<-schnualt2[row.names(schnualt2)=="N",]
          
          xx$nocut$schnualt2[1,,j]<-schnualt2[row.names(schnualt2)=="rec_bio" ,"Estimate"]
          xx$nocut$schnualt2[2,,j]<-schnualt2[row.names(schnualt2)=="post_rec" ,"Estimate"]
          xx$nocut$schnualt2[3,,j]<-schnualt2[row.names(schnualt2)=="biomass" ,"Estimate"]
          xx$nocut$schnualt2[4,,j]<-schnualt2[row.names(schnualt2)=="rec_no" ,"Estimate"]
          xx$nocut$schnualt2[5,,j]<-schnualt2[row.names(schnualt2)=="PR" ,"Estimate"]
          xx$nocut$schnualt2[6,,j]<-schnualt2[row.names(schnualt2)=="N" ,"Estimate"]
          
        })
      
      #print("after schnu alt")
      ##======================================
      # Schnute B0 estimate runs
      #=====================================
      print("bf 2.2 sch")
      obj2.2<-schnute_obserror(version=2,catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho, W = W ,ind_l_wt=1, start_q = 1e-08, start_indexsigma = 0.1,
                               start_sigma = sigma_st, start_f_calc = 0.5, start_catchsigma = st_cthsigma,
                               fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                               fix_catchsigma = T)
      print("af 2.2 sch")
      if(!is.na(obj2.2$fn())){
        
        schnub02<- tryCatch({
          
          
          opt2.2 <- nlminb(start=obj2.2$par,objective=obj2.2$fn,gradient=obj2.2$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
          
          if(opt2.2$convergence==0){
            
            summary(TMB::sdreport(obj2.2))
            
          }else
            
            1
          
        },
        error=function(cond) {
          message(paste("Something not working for this iter:", j, "and", id))
          message("Here's the original error message:")
          message(cond)
          # Choose a return value in case of error
          return(NULL)
        }
        )
        
        
        
        
      }else{
        
        schnub02 <- "try different starting pars"
      }
      
      suppressWarnings(
        if(is.null(schnub02)){
          
          xx$nocut$schnub02_errors[1,1:2,j]<-c(1,"Error in optimisation")
          xx$nocut$schnub02_par[1:3,1,j]<-NA
          
        }else if(schnub02==1){
          xx$nocut$schnub02_par[1:3,1,j]<-NA
          xx$nocut$schnub02_errors[1,1:2,j]<-c(1,opt2.2$message)
          
        }else if(schnub02=="try different starting pars"){
          xx$nocut$schnub02_par[1:3,1,j]<-NA
          xx$nocut$schnub02_errors[1,1:2,j]<-c(1,"try different starting pars")
        }else{
          xx$nocut$schnub02_errors[1,1:2,j]<-c(0,opt2.2$message)
          xx$nocut$schnub02_par[1:3,,j]<-schnub02[c("qhat","index_sigma","B0"),]
          
          xx$nocut$schnub02_par[5:7,,j]<-schnub02[row.names(schnub02)=="f_calc",][c(1,(no.years/2),no.years),]
          xx$nocut$schnub02_par[8:(no.years+7),,j]<-schnub02[row.names(schnub02)=="N",]
          
          
          xx$nocut$schnub02[1,,j]<-schnub02[row.names(schnub02)=="rec_bio" ,"Estimate"]
          xx$nocut$schnub02[2,,j]<-schnub02[row.names(schnub02)=="post_rec" ,"Estimate"]
          xx$nocut$schnub02[3,,j]<-schnub02[row.names(schnub02)=="biomass" ,"Estimate"]
          xx$nocut$schnub02[4,,j]<-schnub02[row.names(schnub02)=="rec_no" ,"Estimate"]
          xx$nocut$schnub02[5,,j]<-schnub02[row.names(schnub02)=="PR" ,"Estimate"]
          xx$nocut$schnub02[6,,j]<-schnub02[row.names(schnub02)=="N" ,"Estimate"]
          xx$nocut$schnub02[7,,j]<-schnub02[row.names(schnub02)=="f_calc" ,"Estimate"]
          
        })
      
    }else { xx$nocut$schnub02_par[1:3,1,j]<- xx$nocut$schnualt_par[1:2,1,j] <- NA  #this is end of if to negate against logrho errors
    xx$nocut$schnub02_errors[1,1:2,j] <- xx$nocut$schnualt_errors[1,1:2,j]<-c(1,"rho error")        }
    
    
    #print("after schnu b0")
    ##==============CSA##======================
    #
    #
    #
    catch.no<-colSums(catch.n(idx), na.rm=T)
    obs<-matrix(NA,nrow=2,ncol=no.years)
    att<-data.frame(survey=c(1,1),type=c(1,2))
    selrec<-matrix(1,nrow=1,ncol=no.years)# number of rows is number of rec survey indices
    
    obs[1,]<-c(index(idx)[1,])
    obs[2,]<-colSums(index(idx)[pr_range,])
    
    #args(csa)
    obj3<- csa(catch_n = catch.no, indices_no = obs, indices_att = att, ts = 0 , selrec = selrec, start_q = 1e-8, start_surveycv = st_survsig, start_catchcv = st_cthsigma, start_nmort = mort, start_f_calc = 0.5,fix_nmort = T, fix_prec0 = F, fix_surveycv = F, fix_catchcv = T)
    
    if(!is.na(obj3$fn())){
      
      csaest<- tryCatch({
        
        
        opt3 <- nlminb(start=obj3$par,objective=obj3$fn,gradient=obj3$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
        
        if(opt3$convergence == 0){
          
          summary(TMB::sdreport(obj3))
          
        }else
          
          1
        
      },
      error=function(cond) {
        message(paste("Something not working for this iter:", j, "and", id ))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NULL)
      }
      )
      
      
      
      
    }else{
      
      csaest <- "try different starting pars"
    }
    
    suppressWarnings(
      if(is.null(csaest)){
        xx$nocut$csa_par[1:3,1,j]<-NA
        xx$nocut$csa_errors[1,1:2,j]<-c(1,"obj evals to NaN")
        
      }else if(csaest==1){
        xx$nocut$csa_par[1:3,1,j]<-NA
        xx$nocut$csa_errors[1,1:2,j]<-c(1,opt3$message)
        
      }else if(csaest=="try different starting pars"){
        xx$nocut$csa_par[1:3,1,j]<-NA
        xx$nocut$csa_errors[1,1:2,j]<-c(1,"try different starting pars")
        
      }else{
        xx$nocut$csa_errors[1,1:2,j]<-c(0,opt3$message)
        
        xx$nocut$csa_par[1:3,,j]<-csaest[c("qhat","sdsurv","phat1"),]
        
        xx$nocut$csa_par[5:7,,j]<-csaest[row.names(csaest)=="f_calc",][c(1,(no.years/2),no.years),]
        xx$nocut$csa_par[8:(no.years+7),,j]<-csaest[row.names(csaest)=="bhat",]
        
        xx$nocut$csa[4,,j]<-csaest[row.names(csaest)=="rhat", "Estimate"]
        xx$nocut$csa[5,,j]<-csaest[row.names(csaest)=="phat", "Estimate"]
        xx$nocut$csa[6,,j]<-csaest[row.names(csaest)=="bhat", "Estimate"]
        xx$nocut$csa[1,,j]<-xx$nocut$csa[4,,j] * mean_wts[1,]
        xx$nocut$csa[2,,j]<-xx$nocut$csa[5,,j] * mean_wts[2,]
        xx$nocut$csa[3,,j]<-xx$nocut$csa[6,,j] * mean_wts[3,]
        xx$nocut$csa[7,,j]<- csaest[row.names(csaest)=="f_calc", "Estimate"]
      })
    
  }#end of iters loop
  #xx$idx<-qapply(idx, iterMedians)
  #xx$stk<-qapply(stk, iterMedians)
  xx$idx<-NA
  xx$stk<-NA
  
  
  return(xx)
}


#xx<-res[[3]]
#this function needs adapting for new error message storage
assessments_cut<- function(xx){
  stk<-xx$stk
  pr_range <-3:(stk@range["max"]+1)
  f_range <- 2:(stk@range["max"]+1)
  no.years <-max(an(dimnames(stk)$year))
  tp <-an(dimnames(stk)$year)
  iters<-dim(stk)[6]
  mort<-mean(m(stk),na.rm=T)
  waa <- an(iterMeans(catch.wt(stk))[f_range,"1"]) # #according to equation at the start of Schnute
  #fit <- lm(w[2:11] ~ w[1:10])# #according to equation at the start of Schnute
  fit_waa <- lm(waa[2:ceiling(max(f_range)/2)] ~ waa[1:(ceiling(max(f_range)/2)-1)])
  
  xx$cut$schnub0 <- xx$cut$schnualt <- xx$cut$schnub02 <- xx$cut$schnualt2 <- xx$cut$csa <- array(NA,c(7,no.years,iters))
  
  xx$cut$schnub0_par <- xx$cut$schnualt_par <- xx$cut$schnub02_par <- xx$cut$schnualt2_par <-xx$cut$csa_par <- array(NA,c(10,2,iters))
  
  xx$cut$waa_par$W <- an(coef(fit_waa)[1])
  xx$cut$waa_par$rho <- an(coef(fit_waa)[2])
  
  xx$cut$was_par<-array(NA,c(2,1,iters))
  
  xx$cut$rv_par <- array(NA,c(10,2,iters))
  xx$cut$rv <- array(NA,c(7,no.years,iters))
  
  xx$cut$rv_par[1,1,]<-1e-6
  xx$cut$rv_par[2,1,]<-NA
  xx$cut$rv_par[3,1,]<-quantSums(stock.n(stk)[pr_range,1])
  xx$cut$rv_par[4,1,]<-quantSums(stock.n(stk)[f_range,1]*catch.wt(stk)[f_range,1])
  xx$cut$rv_par[5,1,]<-quantMeans(harvest(stk))[,2]
  xx$cut$rv_par[6,1,]<-quantMeans(harvest(stk))[,(no.years/2)]
  xx$cut$rv_par[7,1,]<-quantMeans(harvest(stk))[,no.years]
  xx$cut$rv_par[8,1,]<-quantSums(stock.n(stk)[f_range,1])
  xx$cut$rv_par[9,1,]<-quantSums(stock.n(stk)[f_range,(no.years/2)])
  xx$cut$rv_par[10,1,]<-quantSums(stock.n(stk)[f_range,no.years])
  
  
  xx$cut$rv[1,,]<-stock.n(stk)[2,]*catch.wt(stk)[2,]
  xx$cut$rv[2,,]<-quantSums(stock.n(stk)[pr_range,]*catch.wt(stk)[pr_range,])
  xx$cut$rv[3,,]<-quantSums(stock.n(stk)[f_range,]*catch.wt(stk)[f_range,])
  xx$cut$rv[4,,]<-stock.n(stk)[2,]
  xx$cut$rv[5,,]<-quantSums(stock.n(stk)[pr_range,])
  xx$cut$rv[6,,]<-quantSums(stock.n(stk)[f_range,])
  xx$cut$rv[7,,]<-quantMeans(harvest(stk))
  
  #plot(stk)
  for (j in 1:iters){
    print(j)
    idx<-iter(xx$idx,j)
    mean_wts<-xx$cut$mwts[,,j]
    
    #print("linearmod")
    mod<-stats::lm(mean_wts[2,2:no.years]~mean_wts[3,1:no.years-1])
    W1<-coef(mod)[1]
    rho1<-coef(mod)[2]
    xx$cut$was_par[1:2,,j] <-c(W1,rho1)
    
    if(suppressWarnings(!is.nan(log(rho1)))){
      catch_kg1<-c(colSums(catch.n(idx)[f_range,]*catch.wt(idx)[f_range,]))
      index<-colSums(index(idx)[f_range,]*catch.wt(idx)[f_range,])
      obs<-matrix(NA,nrow=1,ncol=no.years)
      obs[1,]<-index # this index has 0.3 sdlog variability from true biomass
      
      #args(schnute_peb)
      #start_B0 = sb0
      sigma_st <- exp(-mort)
      
      ##======================================
      # Schnute alt runs
      #=====================================
      # print("load firstschnu")
      #print(log(rho1))
      #print(log(W1))
      #print(length(catch_kg1))
      print("bf 1 sch")
      obj1<-schnute_orig(version = 2,catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho1, W = W1 , start_q = 1e-08, start_indexsigma = 0.1, start_sigma = sigma_st, fix_sigma = TRUE, fix_indexsigma = FALSE,ind_l_wt=1)
      print("af 1 sch")
      if(!is.na(obj1$fn())){
        
        schnualt<- tryCatch({
          
          
          opt1 <- nlminb(start=obj1$par,objective=obj1$fn,gradient=obj1$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
          
          if(opt1$convergence==0){
            
            summary(TMB::sdreport(obj1))
            
          }else
            
            1
        },
        error=function(cond) {
          message(paste("Something not working for this iter:", j, "and", id))
          message("Here's the original error message:")
          message(cond)
          # Choose a return value in case of error
          return(NULL)
        }
        )
        
        
        
        
      }else{
        
        schnualt <- "try different starting pars"
      }
      
      suppressWarnings(
        if(is.null(schnualt)){
          xx$cut$schnualt_par[1:2,1,j]<-"obj evals to NaN"
        }else if(schnualt==1){
          xx$cut$schnualt_par[1:2,1,j]<-opt1$message
        }else if(schnualt=="try different starting pars"){
          xx$cut$schnualt_par[1:2,1,j]<-"try different starting pars"
        }else{
          xx$cut$schnualt_par[1:2,,j]<-schnualt[c("qhat","index_sigma"),]
          
          xx$cut$schnualt_par[8:10,,j]<-schnualt[row.names(schnualt)=="N",][c(1,(no.years/2),no.years),]
          
          xx$cut$schnualt[1:3,,j]<-matrix((schnualt[row.names(schnualt)=="rec_bio" | row.names(schnualt)=="post_rec" | row.names(schnualt)=="biomass","Estimate"]),byrow=T,ncol=no.years,nrow=3)
          
          xx$cut$schnualt[5:7,,j] <- xx$cut$schnualt[1:3,,j] /  mean_wts[3:1,,drop=F]
          
        })
      
      #print("after schnu alt")
      ##======================================
      # Schnute B0 estimate runs
      #=====================================
      print("bf 2 sch")
      obj2<-schnute_obserror(version=2,catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho1, W = W1 , start_q = 1e-08, start_indexsigma = 0.1,
                             start_sigma = sigma_st, start_f_calc = 0.5, start_catchsigma = 0.1,
                             fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                             fix_catchsigma = T,ind_l_wt=1)
      print("af 2 sch")
      if(!is.na(obj2$fn())){
        
        schnub0<- tryCatch({
          
          
          opt2 <- nlminb(start=obj2$par,objective=obj2$fn,gradient=obj2$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
          
          if(opt2$convergence==0){
            
            summary(TMB::sdreport(obj2))
            
          }else
            
            1
          
        },
        error=function(cond) {
          message(paste("Something not working for this iter:", j, "and", id))
          message("Here's the original error message:")
          message(cond)
          # Choose a return value in case of error
          return(NULL)
        }
        )
        
        
        
        
      }else{
        
        schnub0 <- "try different starting pars"
      }
      
      suppressWarnings(
        if(is.null(schnub0)){
          xx$cut$schnub0_par[1:3,1,j]<-"obj evals to NaN"
        }else if(schnub0==1){
          xx$cut$schnub0_par[1:3,1,j]<-opt2$message
        }else if(schnub0=="try different starting pars"){
          xx$cut$schnub0_par[1:3,1,j]<-"try different starting pars"
        }else{
          xx$cut$schnub0_par[1:3,,j]<-schnub0[c("qhat","index_sigma","B0"),]
          
          xx$cut$schnub0_par[5:7,,j]<-schnub0[row.names(schnub0)=="f_calc",][c(2,(no.years/2),no.years),]
          xx$cut$schnub0_par[8:10,,j]<-schnub0[row.names(schnub0)=="N",][c(1,(no.years/2),no.years),]
          
          
          xx$cut$schnub0[1:4,,j]<-matrix((schnub0[row.names(schnub0)=="f_calc" | row.names(schnub0)=="rec_bio" | row.names(schnub0)=="post_rec" | row.names(schnub0)=="biomass","Estimate"]),byrow=T,ncol=no.years,nrow=4)
          
          xx$cut$schnub0[5:7,,j] <- xx$cut$schnub0[1:3,,j] /  mean_wts[3:1,,drop=F]
          
        })
      
    }else xx$cut$schnub0_par[1:3,1,j]<- xx$cut$schnualt_par[1:2,1,j] <- "rho error"    #this is end of if to negate against logrho errors
    
    #===================================================================================================
    #
    # With waa W and rho
    #=========================================================================
    W <- xx$cut$waa_par$W
    rho <- xx$cut$waa_par$rho
    mean_wts<-xx$cut$mwts_nosel[,,j]
    
    if(suppressWarnings(!is.nan(log(rho)))){
      
      ##======================================
      # Schnute alt runs
      #=====================================
      
      print("bf 1.2 sch")
      
      obj1.2<- schnute_orig(version = 2,catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho, W = W ,ind_l_wt=1, start_q = 1e-08, start_indexsigma = 0.1, start_sigma = sigma_st, fix_sigma = TRUE, fix_indexsigma = FALSE)
      
      print("af 1.2 sch")
      if(!is.na(obj1.2$fn())){
        
        schnualt2<- tryCatch({
          
          
          opt1.2 <- nlminb(start=obj1.2$par,objective=obj1.2$fn,gradient=obj1.2$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
          
          if(opt1.2$convergence==0){
            
            summary(TMB::sdreport(obj1.2))
            
          }else
            
            1
        },
        error=function(cond) {
          message(paste("Something not working for this iter:", j, "and", id))
          message("Here's the original error message:")
          message(cond)
          # Choose a return value in case of error
          return(NULL)
        }
        )
        
        
        
        
      }else{
        
        schnualt2 <- "try different starting pars"
      }
      
      suppressWarnings(
        if(is.null(schnualt2)){
          xx$cut$schnualt2_par[1:2,1,j]<-"obj evals to NaN"
        }else if(schnualt2==1){
          xx$cut$schnualt2_par[1:2,1,j]<-opt1.2$message
        }else if(schnualt2=="try different starting pars"){
          xx$cut$schnualt2_par[1:2,1,j]<-"try different starting pars"
        }else{
          xx$cut$schnualt2_par[1:2,,j]<-schnualt2[c("qhat","index_sigma"),]
          
          xx$cut$schnualt2_par[8:10,,j]<-schnualt2[row.names(schnualt2)=="N",][c(1,(no.years/2),no.years),]
          
          xx$cut$schnualt2[1:3,,j]<-matrix((schnualt2[row.names(schnualt2)=="rec_bio" | row.names(schnualt2)=="post_rec" | row.names(schnualt2)=="biomass","Estimate"]),byrow=T,ncol=no.years,nrow=3)
          
          xx$cut$schnualt2[5:7,,j] <- xx$cut$schnualt2[1:3,,j] /  mean_wts[3:1,,drop=F]
          
        })
      
      #print("after schnu alt")
      ##======================================
      # Schnute B0 estimate runs
      #=====================================
      print("bf 2.2 sch")
      obj2.2<-schnute_obserror(version=2,catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho, W = W ,ind_l_wt=1, start_q = 1e-08, start_indexsigma = 0.1,
                               start_sigma = sigma_st, start_f_calc = 0.5, start_catchsigma = 0.1,
                               fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                               fix_catchsigma = T)
      print("af 2.2 sch")
      if(!is.na(obj2.2$fn())){
        
        schnub02<- tryCatch({
          
          
          opt2.2 <- nlminb(start=obj2.2$par,objective=obj2.2$fn,gradient=obj2.2$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
          
          if(opt2.2$convergence==0){
            
            summary(TMB::sdreport(obj2.2))
            
          }else
            
            1
          
        },
        error=function(cond) {
          message(paste("Something not working for this iter:", j, "and", id))
          message("Here's the original error message:")
          message(cond)
          # Choose a return value in case of error
          return(NULL)
        }
        )
        
        
        
        
      }else{
        
        schnub02 <- "try different starting pars"
      }
      
      suppressWarnings(
        if(is.null(schnub02)){
          xx$cut$schnub02_par[1:3,1,j]<-"obj evals to NaN"
        }else if(schnub02==1){
          xx$cut$schnub02_par[1:3,1,j]<-opt2.2$message
        }else if(schnub02=="try different starting pars"){
          xx$cut$schnub02_par[1:3,1,j]<-"try different starting pars"
        }else{
          xx$cut$schnub02_par[1:3,,j]<-schnub02[c("qhat","index_sigma","B0"),]
          
          xx$cut$schnub02_par[5:7,,j]<-schnub02[row.names(schnub02)=="f_calc",][c(2,(no.years/2),no.years),]
          xx$cut$schnub02_par[8:10,,j]<-schnub02[row.names(schnub02)=="N",][c(1,(no.years/2),no.years),]
          
          
          xx$cut$schnub02[1:4,,j]<-matrix((schnub02[row.names(schnub02)=="f_calc" | row.names(schnub02)=="rec_bio" | row.names(schnub02)=="post_rec" | row.names(schnub02)=="biomass","Estimate"]),byrow=T,ncol=no.years,nrow=4)
          
          xx$cut$schnub02[5:7,,j] <- xx$cut$schnub02[1:3,,j] /  mean_wts[3:1,,drop=F]
          
        })
      
    }else xx$cut$schnub02_par[1:3,1,j]<- xx$cut$schnualt2_par[1:2,1,j] <- "rho error"    #this is end of if to negate against logrho errors
    
    
    
    #print("after schnu b0")
    ##==============CSA##======================
    #
    #
    #
    catch.no<-colSums(catch.n(idx)[f_range,], na.rm=T)
    obs<-matrix(NA,nrow=2,ncol=no.years)
    att<-data.frame(survey=c(1,1),type=c(1,2))
    selrec<-matrix(1,nrow=1,ncol=no.years)# number of rows is number of rec survey indices
    
    obs[1,]<-c(index(idx)[2,])
    obs[2,]<-colSums(index(idx)[pr_range,])
    
    #args(csa)
    
    #args(csa)
    obj3<- csa(catch_n = catch.no, indices_no = obs, indices_att = att, ts = 0 , selrec = selrec, start_q = 1e-8, start_surveycv = 0.1, start_catchcv = 0.1, start_nmort = mort, start_f_calc = 0.5,fix_nmort = T, fix_prec0 = F, fix_surveycv = F, fix_catchcv = T)
    
    if(!is.na(obj3$fn())){
      
      csaest<- tryCatch({
        
        
        opt3 <- nlminb(start=obj3$par,objective=obj3$fn,gradient=obj3$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10))
        
        if(opt3$convergence == 0){
          
          summary(TMB::sdreport(obj3))
          
        }else
          
          1
        
      },
      error=function(cond) {
        message(paste("Something not working for this iter:", j, "and", id ))
        message("Here's the original error message:")
        message(cond)
        # Choose a return value in case of error
        return(NULL)
      }
      )
      
      
      
      
    }else{
      
      csaest <- "try different starting pars"
    }
    
    suppressWarnings(
      if(is.null(csaest)){
        xx$cut$csa_par[1:3,1,j]<-"obj evals to NaN"
      }else if(csaest==1){
        xx$cut$csa_par[1:3,1,j]<-opt3$message
      }else if(csaest=="try different starting pars"){
        xx$cut$csa_par[1:3,1,j]<-"try different starting pars"
      }else{
        xx$cut$csa_par[1:3,,j]<-csaest[c("qhat","sdsurv","phat1"),]
        
        xx$cut$csa_par[5:7,,j]<-csaest[row.names(csaest)=="f_calc",][c(2,(no.years/2),no.years),]
        xx$cut$csa_par[8:10,,j]<-csaest[row.names(csaest)=="bhat",][c(1,(no.years/2),no.years),]
        
        xx$cut$csa[1:4,,j]<-matrix((csaest[row.names(csaest)=="f_calc" | row.names(csaest)=="rhat" | row.names(csaest)=="phat" | row.names(csaest)=="bhat","Estimate"]),byrow=T,ncol=no.years,nrow=4)
        
        xx$cut$csa[5:6,,j] <- xx$cut$csa[1:2,,j] * mean_wts[2:1,,drop=F]
        xx$cut$csa[7,,j] <- xx$cut$csa[3,,j] * mean_wts[3,]
      })
    
    
  }#end of iters loop
  #xx$idx<-qapply(idx, iterMedians)
  #xx$stk<-qapply(stk, iterMedians)
  #xx$idx<
  #xx$stk<-NA
  
  
  return(xx)
}
