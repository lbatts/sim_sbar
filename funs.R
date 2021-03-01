##=======
#
# Functions for stock simulation
#========
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



caterror <- function(x){
    stk <- x$stk
    idx<- x$idx
  eps_c <- FLQuant(rlnorm(n = prod(dim(stk)), sdlog = 0.1),
                   dim = dim(stk), dimnames = dimnames(landings.n(stk)))
  catch.n(idx) <- catch.n(stk) * eps_c
  catch.wt(idx) <- catch.wt(stk)
  effort(idx) <- quantSums(catch.n(idx)*catch.wt(stk))
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
  
  x$mwts <-mwts_mat
  
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
  
  xx$schnub0 <- xx$schnualt <- xx$csa <- array(NA,c(6,no.years,iters))
  
  xx$schnub0_par <- xx$schnualt_par <- xx$csa_par <- array(NA,c(4,2,iters))
  #plot(stk)
  for (j in 1:iters){
    print(j)
    idx<-iter(xx$idx,j)
    mean_wts<-xx$mwts[,,j]
    #print("linearmod")
    mod<-stats::lm(mean_wts[2,2:no.years]~mean_wts[3,1:no.years-1])
    W1<-coef(mod)[1]
    rho1<-coef(mod)[2]
    if(suppressWarnings(!is.nan(log(rho1)))){
    catch_kg1<-c(effort(idx))
    index<-colSums(index(idx)*catch.wt(idx))
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
    obj1<-schnute_peb("y1_no_f",catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho1, W = W1 , start_q = 1e-08, start_indexsigma = 0.1,
                     start_sigma = sigma_st, start_f_calc = 0.5, start_catchsigma = 0.1,
                     fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                     fix_catchsigma = T)
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
        return(NA)
      },
      warning=function(cond) {
        message("something caused a warning")
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      }
      )
      
      
      
      
    }else{
      
      schnualt <- "try different starting pars"
    }
    
    suppressWarnings(
      if(is.null(schnualt)){
        xx$schnualt_par[1,1,j]<-"obj evals to NaN"  
      }else if(schnualt==1){
        xx$schnualt_par[1,1,j]<-"did not converge"
      }else if(schnualt=="try different starting pars"){
        xx$schnualt_par[1,1,j]<-"try different starting pars"
      }else{
        xx$schnualt_par[1:2,,j]<-schnualt[c("logq","logindex_sigma"),]
        
        xx$schnualt[1:4,,j]<-matrix((schnualt[row.names(schnualt)=="f_calc" | row.names(schnualt)=="rec_bio" | row.names(schnualt)=="post_rec" | row.names(schnualt)=="biomass","Estimate"]),byrow=T,ncol=no.years,nrow=4)
        
      })
    
    #print("after schnu alt")
    ##======================================
    # Schnute B0 estimate runs
    #=====================================
    print("bf 2 sch")
    obj2<-schnute_peb("B0",catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho1, W = W1 , start_q = 1e-08, start_indexsigma = 0.1,
                     start_sigma = sigma_st, start_f_calc = 0.5, start_catchsigma = 0.1,
                     fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,
                     fix_catchsigma = T)
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
        return(NA)
      },
      warning=function(cond) {
        message("something caused a warning")
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      }
      )
      
      
      
      
    }else{
      
      schnub0 <- "try different starting pars"
    }
    
    suppressWarnings(
      if(is.null(schnub0)){
        xx$schnub0_par[1,1,j]<-"obj evals to NaN"  
      }else if(schnub0==1){
        xx$schnub0_par[1,1,j]<-"did not converge"
      }else if(schnub0=="try different starting pars"){
        xx$schnub0_par[1,1,j]<-"try different starting pars"
      }else{
        xx$schnub0_par[1:3,,j]<-schnub0[c("logq","logindex_sigma","logB0"),]
        
        xx$schnub0[1:4,,j]<-matrix((schnub0[row.names(schnub0)=="f_calc" | row.names(schnub0)=="rec_bio" | row.names(schnub0)=="post_rec" | row.names(schnub0)=="biomass","Estimate"]),byrow=T,ncol=no.years,nrow=4)
        
      })
    
    }else xx$schnub0_par[1,1,j]<- xx$schnualt_par[1,1,j] <- "rho error"    #this is end of if to negate against logrho errors
    
    
    
    
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
        return(NA)
      },
      warning=function(cond) {
        message("something caused a warning")
        message("Here's the original warning message:")
        message(cond)
        # Choose a return value in case of warning
        return(NULL)
      }
      )
      
      
      
      
    }else{
      
      csaest <- "try different starting pars"
    }
    
    suppressWarnings(
      if(is.null(csaest)){
        xx$csa_par[1,1,j]<-"obj evals to NaN"  
      }else if(csaest==1){
        xx$csa_par[1,1,j]<-"did not converge"
      }else if(csaest=="try different starting pars"){
        xx$csa_par[1,1,j]<-"try different starting pars"
      }else{
        xx$csa_par[1:3,,j]<-csaest[c("logitqhat","log_surveycv","logphat1"),]
        
        xx$csa[1:4,,j]<-matrix((csaest[row.names(csaest)=="f_calc" | row.names(csaest)=="rhat" | row.names(csaest)=="phat" | row.names(csaest)=="bhat","Estimate"]),byrow=T,ncol=no.years,nrow=4)
        
      })
    
  }#end of iters loop
  
  return(xx)
}