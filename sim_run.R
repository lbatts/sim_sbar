#===================================
# Code for simulation testing of the stage-based assessment models of sbar package
# Author : LB
#Feb 2021
#===================================
rm(list=ls())
# library(FLCore)
# library(FLasher)
# library(FLBRP)
# library(FLife)
# library(plyr)
# library(reshape)
# #library(ggplotFL)
# library(sbar)
# library(doParallel)
# library(TMBhelper)

setwd("C:/Users/LukeB/Documents/sim_sbar")

#load function to create stocks
source("par_setup.R")
#source("funs.R")
par(mfrow = c(2,1))
lapply(sce_ls$sel$her[2:3],function(x){
  
  plot(c(unlist(x))~c(0:8),type="b",col=2)  
  
})

sce_ls$nm$mon$gis
sce_ls$nm$her$gis

sce <- expand.grid(lh = c("mon","her"), ts = c("short","long"), sel = c("kn0","logistic","dome"), nm = c("cons", "gislason"), ar =c ("nocor","0.6rho"), sr = c("recsd0.1","recsd0.4") )
#stk_sel <- sce_ls$sel[[which(sapply(names(sce_ls$sel), function(y) fish %in% y))]]
dim(sce)
 iters<- 100
 timing<-0

 cores=detectCores()
 cl <- makeCluster(cores[1]-1,outfile= "Log.txt") #not to overload your computer
 registerDoParallel(cl)
 seedlist <- seq(from = 100,by = 50,length.out = dim(sce)[1])
 clusterExport(cl, 'seedlist')

st<-NA
st[1]<-Sys.time()

#  #
#dim(sce)[1]
 tmpres<-foreach(i=1:10,.packages = (.packages())) %dopar% {#   ,.combine=cbind
   set.seed(seedlist[i]) 
   lh<-sce$lh[i]
  stk_par<-sce_ls$lh[[lh]]
  stk_range <- sce_ls$range[[lh]]
  ages<-c(stk_range["min"]:stk_range["max"])
  nm<-sce$nm[i]
  stk_m <-unlist(sce_ls$nm[[lh]][nm])
  stk_eq <- lhEql(stk_par,range = stk_range,m = stk_m)
  #plot(stk_eq)
  sel <- sce$sel[i]
  stk_sel <- unlist(sce_ls$sel[[lh]][sel])
  landings.sel(stk_eq) <- stk_sel
  #params(stk_eq) <- FLCore::ab(stk_par[c("s","v")],"bevholt",spr0=spr0(stk_eq))[c("a","b")]
  stk_eq <- brp(stk_eq)
  #plot(stk_eq)
  #plot(stk_sel)
  ts<-sce$ts[i]
  no.years <- sce_ls$ts[[ts]]   #sce_ls$ts[[ts]]
  years_vec <- 1:no.years
        
  ###===============================================
  #This section sets up three stocks with three different harvest dynamics -- no need for loop
  #===============constant (c)
  
        f_c <- c(refpts(stk_eq)['msy', 'harvest'])
        fbar_c<-FLQuant(f_c,dimnames= list(year = ac(years_vec)))#*FLQuant(rlnorm(n=years,sdlog = 0.1))
        #plot(fbar_c)
        
        #==============One way (ow)
        fmax=refpts(stk_eq)['crash', 'harvest']*0.80
        # limits
        f0 <- c(refpts(stk_eq)['msy', 'harvest'])*.5#c(fbar(stk_om)[,1])
        fmax <- c(fmax)
        rate <- exp((log(fmax) - log(f0)) / (length(years_vec)))
        #rate=1.2
        # linear trend
        f_ow <- rate^(0:(no.years+1))*f0
        fbar_ow<-FLQuant(f_ow,dimnames= list(year = ac(years_vec)))#*FLQuant(rlnorm(n=years,sdlog = 0.1))
        #plot(fbar_ow)
        
        #==============rollercoaster (rc)
        f_rc <- rep(NA, length(years_vec))
        
        upy <- ceiling(no.years/3)
        top <- 10
        downy <- no.years-(upy+top)
        
        # linear trend up: 1:upy
        fup <- seq(f0, fmax, length=upy)
        f_rc[1:upy] <- fup
        
        # at the top: upy+1:upy+6
        f_rc[(upy+1):(upy+top-1)] <- fmax
        
        # coming down!
        fmsy <- c(refpts(stk_eq)['msy', 'harvest'])
        fdo <- seq(fmax, fmsy, length=downy+1)
        f_rc[(upy+top):(upy+top+downy)] <- fdo
        #plot(f_rc)
        fbar_rc<-FLQuant(f_rc,dimnames= list(year = ac(years_vec)))#*FLQuant(rlnorm(n=years,sdlog = 0.1))
        
        #set and reset these stks up before applying fbars, sr residuals and catch error
            stk_rc <- stk_c <- stk_ow <- stk_eq
            
            sr <- sce$sr[i]
            sr_sd <- c(sce_ls$sr[[sr]])
            ar<- sce$ar[i]
            rho <- c(sce_ls$ar[[ar]])
            
            fbar(stk_c) <- fbar_c
            fbar(stk_ow) <- fbar_ow
            fbar(stk_rc) <- fbar_rc
            
            stk_c <- propagate(as(stk_c,"FLStock"),iter = iters, fill.iter = TRUE)
            stk_ow <- propagate(as(stk_ow,"FLStock"),iter = iters, fill.iter = TRUE)
            stk_rc <- propagate(as(stk_rc,"FLStock"),iter = iters, fill.iter = TRUE)
            
            stk_c <- fwd(stk_c,fbar = fbar(stk_c)[,-1] , sr=stk_eq, deviances = ar1rlnorm(rho=rho,iters=iters, years=1:no.years, margSD=sr_sd))
            stk_ow <- fwd(stk_ow,fbar = fbar(stk_ow)[,-1] , sr=stk_eq, deviances = ar1rlnorm(rho=rho,iters=iters, years=1:no.years, margSD=sr_sd))
            stk_rc <- fwd(stk_rc,fbar = fbar(stk_rc)[,-1] , sr=stk_eq, deviances = ar1rlnorm(rho=rho,iters=iters, years=1:no.years, margSD=sr_sd))
            print("set up the tree stocks and fwd with fbars")
            # plot(stk_c)
            # plot(stk_ow)
            # plot(stk_rc)
            
            q1 <- 1e-6 * FLQuant(rep(1,times = dims(stk_c)$age ),
                                 dim = dim(stk_c), dimnames = dimnames(landings.n(stk_c)))
            ## measurement error on index
            eps_ind <- list()
            eps_ind <- lapply(1:3,function(x){FLQuant(rlnorm(n = prod(dim(stk_c)), sdlog = 0.3), dim = dim(stk_c), dimnames = dimnames(landings.n(stk_c)))
            })
            
            idx_c<- FLIndex(index = q1 * stock.n(stk_c)*exp(-(harvest(stk_c) * timing + m(stk_c) * timing)) *  eps_ind[[1]])
            idx_ow<- FLIndex(index = q1 * stock.n(stk_ow)*exp(-(harvest(stk_ow) * timing + m(stk_ow) * timing)) *  eps_ind[[2]])
            idx_rc<- FLIndex(index = q1 * stock.n(stk_rc)*exp(-(harvest(stk_rc) * timing + m(stk_rc) * timing)) *  eps_ind[[3]])
            
            range(idx_rc)[c("startf", "endf")] <- range(idx_ow)[c("startf", "endf")]<- range(idx_c)[c("startf", "endf")] <- c(0, 0)
            
            id<-i
            
            res<-list(scenario = as.name(paste(lh,ts,sel,nm,ar,sr, sep="_")), sce_id = id, 
                       stk_c = list(stk = stk_c, idx = idx_c), 
                       stk_ow = list(stk = stk_ow, idx = idx_ow), 
                       stk_rc = list(stk = stk_rc, idx = idx_rc))            
            print("apply catcherror")
            res[3:5] <- lapply(res[3:5], caterror)

  pr_range <-2:(stk_eq@range["max"]+1)
  f_range <- 1:(stk_eq@range["max"]+1)
  no.years <-max(an(dimnames(stk_c)$year))
  tp <-an(dimnames(stk_c)$year)
  mort<-mean(m(stk_eq),na.rm=T)
  
  print(paste("apply assessment over iters",i))
  
  res[3:5] <- lapply(res[3:5], assessments)
  
  #dat <-rbind(as.data.frame(stk_c),as.data.frame(stk_ow),as.data.frame(stk_rc))[,c(1:2,6:7)]
  #dim(dat)
  #dat<-dat[dat$slot ==c("catch","harvest","stock.n"),]
  
  res
   
 }
 
 stopCluster(cl)
 st[2]<-Sys.time()
 
 
 (st[2]-st[1])
 
 (st[2]-st[1])/60

 #save(tmpres,file="res2.RData")
 #save(tmpres,file="res_1to10iters_parallel.RData")
 #save(res,file="res1.RData")
 #save(res,file="res2.RData")
 
 
 
 rm(list=ls())
 load("res1to10_notpar.Rdata")
 load("res_1to10iters_parallel.Rdata")
 ls()
 
 jeff[[10]]$scenario
 tmpres[[10]]$scenario
 
 
 jeff[[1]]$stk_c$mwts[,,10]-tmpres[[1]]$stk_c$mwts[,,10]#  same 
 jeff[[5]]$stk_c$mwts[,,98]-tmpres[[5]]$stk_c$mwts[,,98]#  same 
 
 
 
 jeff[[1]]$stk_c$stk@catch.n[3,7]
 tmpres[[1]]$stk_c$stk@catch.n[3,7]
 
 
 rec(iter(jeff[[1]]$stk_c$stk,7))- rec(iter(tmpres[[1]]$stk_c$stk,7))
 
 tmpres[[1]]$stk_c$schnualt_par[,,1]
 tmpres[[5]]$stk_rc$schnualt_par[,,4]
 tmpres[[5]]$stk_rc$csa_par
 tmpres[[1]]$stk_rc$schnub0_par[1,1,]
 tmpres[[5]]$stk_rc$schnualt_par[1,1,]
 
 