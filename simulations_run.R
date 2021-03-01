#===================================
# Code for simulation testing of the stage-based assessment models of sbar package
# Author : LB
#Feb 2021
#===================================
rm(list=ls())
library(FLCore)
library(FLasher)
library(FLBRP)
library(FLife)
library(plyr)
library(reshape)
library(ggplotFL)
library(sbar)

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



# VARs that don't change
set.seed(1234)


# sce <- expand.grid(LH = c("her","mon"), TS = c(20,40),SEL = c("kn0","logistic","dome"), HD = c("cons", "ow", "rc"), nm = c("cons", "gislason"), AR =c (0,0.6), sigmaR = c(0,0.6) )
#stk_sel <- sce_ls$sel[[which(sapply(names(sce_ls$sel), function(y) fish %in% y))]]
st<-NA
sims<-list()
st[1]<-Sys.time()

  for(lh in names(sce_ls$LH)){
    #lh = names(sce_ls$LH)[1]
  #set up stock in eq
  stk_par<-sce_ls$LH[[lh]]
  stk_range <- sce_ls$range[[lh]]
  ages<-c(stk_range["min"]:stk_range["max"])
  
  for(nm in names(sce_ls$nm[[lh]])){
    #nm = names(sce_ls$nm[[lh]])[1]
  stk_m <-unlist(sce_ls$nm[[lh]][nm])
  stk_eq <- lhEql(stk_par,range = stk_range,m = stk_m)
  #plot(stk_eq)
  print(lh)
  print(nm)
  for(sel in names(sce_ls$sel[[lh]])){
    #sel = names(sce_ls$sel[[lh]])[1]
    stk_sel <- unlist(sce_ls$sel[[lh]][sel])
    landings.sel(stk_eq) <- stk_sel
    #params(stk_eq) <- FLCore::ab(stk_par[c("s","v")],"bevholt",spr0=spr0(stk_eq))[c("a","b")]
    stk_eq <- brp(stk_eq)
    #plot(stk_eq)
    #plot(stk_sel)
    print(sel)
    
    for(ts in 1:length(sce_ls$ts)){
    years <- sce_ls$ts[[ts]]
    years_vec <- 1:years
   print(years)
      ###===============================================
    #This section sets up three stocks with three different harvest dynamics -- no need for loop
    #===============constant (c)
    f_c <- c(refpts(stk_eq)['msy', 'harvest'])
    fbar_c<-FLQuant(f_c,dimnames= list(year = ac(years_vec)))*FLQuant(rlnorm(n=years,sdlog = 0.1))
    #plot(fbar_c)
    #==============One way (ow)
      fmax=refpts(stk_eq)['crash', 'harvest']*0.80
    # limits
    f0 <- c(refpts(stk_eq)['msy', 'harvest'])*.5#c(fbar(stk_om)[,1])
    fmax <- c(fmax)
    rate <- exp((log(fmax) - log(f0)) / (length(years_vec)))
    #rate=1.2
    # linear trend
    f_ow <- rate^(0:(years+1))*f0
    fbar_ow<-FLQuant(f_ow,dimnames= list(year = ac(years_vec)))*FLQuant(rlnorm(n=years,sdlog = 0.1))
    #plot(fbar_ow)
    
    #==============rollercoaster (rc)
    f_rc <- rep(NA, length(years_vec))
    
    # limits
    f0 <- c(refpts(stk_eq)['msy', 'harvest'])*.5
    fmax <- c(refpts(stk_eq)['crash', 'harvest']*0.80)
    upy <- ceiling(years/3)
    top <- 10
    downy <-years-(upy+top)
    
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
    fbar_rc<-FLQuant(f_rc,dimnames= list(year = ac(years_vec)))*FLQuant(rlnorm(n=years,sdlog = 0.1))
    
    
    
    for(sr in 1:length(sce_ls$sr)){
      
      for(ar in 1:length(sce_ls$ar)){
        
        #set and reset these stks up before applying fbars, sr residuals and catch error
        stk_rc <- stk_c <- stk_ow <- stk_eq
        
      sr_sd <- c(sce_ls$sr[[sr]])
      rho <- c(sce_ls$ar[[ar]])
      
      print(sr_sd)
      print(rho)
      
      fbar(stk_c) <- fbar_c
      fbar(stk_ow) <- fbar_ow
      fbar(stk_rc) <- fbar_rc
      
      stk_c <- as(stk_c,"FLStock")
      stk_ow <- as(stk_ow,"FLStock")
      stk_rc <- as(stk_rc,"FLStock")
      
      stk_c <- fwd(stk_c,fbar = fbar(stk_c)[,-1] , sr=stk_eq, deviances = ar1rlnorm(rho=rho,iters=1, years=1:years, margSD=sr))
      stk_ow <- fwd(stk_ow,fbar = fbar(stk_ow)[,-1] , sr=stk_eq, deviances = ar1rlnorm(rho=rho,iters=1, years=1:years, margSD=sr))
      stk_rc <- fwd(stk_rc,fbar = fbar(stk_rc)[,-1] , sr=stk_eq, deviances = ar1rlnorm(rho=rho,iters=1, years=1:years, margSD=sr))
      
       # plot(stk_c)
       # plot(stk_ow)
       # plot(stk_rc)
      
      q1 <- 1e-6 * FLQuant(rep(1,times = dims(stk_c)$age ),
                           dim = dim(stk_c), dimnames = dimnames(landings.n(stk_c)))
      ## measurement error on index
      eps_ind <- list()
      eps_ind <- lapply(1:3,function(x){FLQuant(rlnorm(n = prod(dim(stk_c)), sdlog = 0.3),
                               dim = dim(stk_c), dimnames = dimnames(landings.n(stk_c)))
                   })
      
      timing=0
      idx_c<- FLIndex(index = q1 * stock.n(stk_c)*exp(-(harvest(stk_c) * timing + m(stk_c) * timing)) *  eps_ind[[1]])
      idx_ow<- FLIndex(index = q1 * stock.n(stk_ow)*exp(-(harvest(stk_ow) * timing + m(stk_ow) * timing)) *  eps_ind[[2]])
      idx_rc<- FLIndex(index = q1 * stock.n(stk_rc)*exp(-(harvest(stk_rc) * timing + m(stk_rc) * timing)) *  eps_ind[[3]])
      
      range(idx_rc)[c("startf", "endf")] <- range(idx_ow)[c("startf", "endf")]<- range(idx_c)[c("startf", "endf")] <- c(0, 0)
      
    sims[[as.name(paste(lh,years,sel,nm,"c",paste0("ar",rho),paste0("sr",sr_sd), sep="_"))]] = list(stk = stk_c, idx = idx_c)
    sims[[as.name(paste(lh,years,sel,nm,"rc",paste0("ar",rho),paste0("sr",sr_sd), sep="_"))]] = list(stk = stk_rc, idx = idx_rc)
    sims[[as.name(paste(lh,years,sel,nm,"ow",paste0("ar",rho),paste0("sr",sr_sd), sep="_"))]] = list(stk = stk_ow, idx = idx_ow)
    
    
    
          }
        }
      }
    }
  }
}

st[2]<-Sys.time()
(st[2]-st[1])/60

#save(sims,file = "sims.RData")
tmp<- names(sims) 
length(tmp)
 which(grepl("mon_40_dn_gis_rc",tmp))[1]
 #tmp[which(grepl("her_40_dn_gis_rc",tmp))]  

sims[[278]]$stk@m
sims[[134]]$stk@m

























  
tmp <- lhEql(stk_par,range = stk_range,m = 0.2 ,sel = logistic)
spr0(tmp)
m(tmp) =0.5
spr0(tmp) # spr0 changes automatically with a changing m
plot(tmp)
tmp<-brp(tmp)
plot(tmp) # still weird
refpts(tmp)
params(tmp) <- FLCore::ab(stk_par[c("s","v")],"bevholt",spr0=spr0(tmp))[c("a","b")]
  tmp<-brp(tmp)
  refpts(tmp)
  
tmp2 <- lhEql(stk_par,range = stk_range,m = 0.5 ,sel = logistic)
 refpts(tmp2)
 #tmp2@landings.sel <- tmp@landings.sel
 refpts(tmp)-refpts(tmp2)
 
 
 ###try with sel now
 tmp <- lhEql(stk_par,range = stk_range,m = 0.2 ,sel = logistic)
 plot(tmp)
 refpts(tmp)
 landings.sel(tmp)<- 1
 tmp<-brp(tmp)
 plot(tmp) # still weird
 refpts(tmp)
 #chaneg it back
 landings.sel(tmp)<- logistic(FLQuant(c(an(dimnames(stk_eq)$age)+.5),dimnames=list(age=dimnames(stk_eq)$age)),stk_par)
 
 c(logistic(FLQuant(c(an(dimnames(stk_eq)$age)+.5),dimnames=list(age=dimnames(stk_eq)$age)),stk_par))
 plot(c(dnormal(FLQuant(c(an(dimnames(stk_eq)$age)+.5),dimnames=list(age=dimnames(stk_eq)$age)),stk_par)))
 
 
 selpar<-FLPar("asym" = 1, "a50" = 0, "ato95" = 1)
 selp<- stk_par[16:18]
 selp["sel1"]<-1
 
 selp["sel2"]<-.9
 selp["sel3"]<-20
 
 logistic = logistic(FLQuant(c(0:20+.5),dimnames=list(age = 0:20)),FLPar("asym" = 1, "a50" = 0, "ato95" = 1)) 
 dn = dnormal(FLQuant(c(0:20+.5),dimnames=list(age = 0:20)), FLPar("sel1" = 1, "sel2" = 0.9, "sel3" = 20))
 plot(logistic~ages,ylim=c(0,1))
 points(dn~ages,col=2, pch=8)
 
 
 
 
 
 for(i in 1:10){
 selpar<-FLPar("asym" = 1, "a50" = (5), "ato95" = (1+i))
 
 points(logistic(FLQuant(c(an(dimnames(stk_eq)$age)+.5),dimnames=list(age=dimnames(stk_eq)$age)),selpar)
        ~ages,col=i,pch=8)
 }
 
 
 
 
 
 
 abline(v = stk_par["a50"],lty=2)
 abline(h = 0.5,lty=2)
 
 
 
 tmp2 <- lhEql(stk_par,range = stk_range,m = 0.2 ,sel = logistic)
 
 refpts(tmp2)
 #tmp2@landings.sel <- tmp@landings.sel
 computeRefpts(tmp2)
 refpts(tmp)
 refpts(tmp2)
 
 
 
 fbar(tmp) <- fbar(stk_eq) <- 0.1
 
 om=as(tmp,"FLStock")
 om1=as(stk_eq,"FLStock")
 srres <- rlnorm(1, FLQuant(0, dimnames=list(year=dimnames(om)$year)), 0.1) #waS 0.01
 
 om=fwd(om,fbar=fbar(om)[,-1], sr=tmp,deviances = srres)
 om1=fwd(om1,fbar=fbar(om1)[,-1], sr=tmp,deviances = srres)
 
 plot(om)
 dev.new()
 plot(om1)
 
 
 
 
  plot(stk_eq)
  dev.new()
  plot(tmp)
  
  stk_eq<-brp(stk_eq)
  plot(stk_eq)
  





mon_eq=lhEql(mpar,range = c(min=0,max=20,minfbar=1,maxfbar=20,plusgroup=20) )

plot(mon_eq) # something weird
m(mon_eq) # natural mortality is super wonky
#try with numeric enry rather than default
mon_eq=lhEql(mpar,range = c(min=0,max=20,minfbar=1,maxfbar=20,plusgroup=20), m= 0.25 )
m(mon_eq) # natural mortality 
plot(mon_eq) # looks good

#seems like the default fun for m is not working properly
?lhEql

#default is gislason
mon_eq=lhEql(mpar,range = c(min=0,max=20,minfbar=1,maxfbar=20,plusgroup=20))

#look at gislason fun
?gislason
len=FLQuant(vonB(age = 0:20,mpar),
            dimnames=list(age=0:20))

tmp<-m(mon_eq)
gis<-gislason(len,mpar) # still off but not the same as out from default lheql.....
tmp/gis # default lheql is 419.22 X gislason fun....

#try lukes gislason
luke_gisla <-function(linf, k,len){
  
  logM<-0.55 - (1.61*log(len)) + (1.44*log(linf)) + log(k)
  return(exp(logM))
  }


lb<-luke_gisla(mpar["linf"],mpar["k"],len = vonB(age = 0:20,mpar))
lb-gis
lb-tmp # same as default lheql
#how can you have a mortality of 2875 fo age 1??


(4.118*mpar["k"]^(0.73))*mpar["linf"]^(-0.33)


mFun <- function(age, params){ (0.2+ 1.64* exp(-1 * age)) }
tmp<-mFun(age=0:15,hpar)

args(gislason)
?gislason
  
  
  m(mon_eq)<-rep(0.4,times = 16)
  dev.new()
  tmp<-brp(mon_eq)
  plot(tmp)
  
  
  ages <- 0:15
  len <-mpar["linf"] * (1 - exp(- mpar["k"] * (ages - mpar["t0"]) ))
  len<-FLQuant(len,dimnames = list(age=ages))
  
  m(mon_eq)<-gislason(len,mpar)
  
  
  params=lhPar(FLPar(linf=111))
  len=FLQuant(c( 1.90, 4.23, 7.47,11.48,16.04,20.96,26.07,31.22,
                 36.28,41.17,45.83,50.20,54.27,58.03,61.48,64.62),
              dimnames=list(age=1:16))
  gislason(len,params)
  
  
  
  landings.sel(hke_brp)<-rep(1,21)
  
  
  
  stock.wt(hke_brp)
  plot(hke_brp)
  
  hke_brp<-brp(hke_brp) # recalculate with new natural mortality
  
  plot(hke_brp)
  
  fbar(hke_brp)=refpts(hke_brp)["msy","harvest"]%*%FLQuant(c(rep(.5,19),
                                                             seq(.5,2.5,length.out = 19),
                                                             seq(2.5,2.5,length.out = 25),
                                                             seq(2.5,.5,length.out = 10)[-1],
                                                             seq(.5,1,length.out = 20)[-1],
                                                             rep(1,14)))[,1:105]
  plot(fbar(hke_brp))
  
  plot(fbar(hke_brp)*FLQuant(rlnorm(n=105,sdlog = 0.1))[,1:105])
  
  fbar(hke_brp)=fbar(hke_brp)*FLQuant(rlnorm(n=105,sdlog = 0.1))[,1:105]
  
  om=as(hke_brp,"FLStock")
  
  srres <- rlnorm(1, FLQuant(0, dimnames=list(year=dimnames(om)$year)), 0.1) #waS 0.01
  
  om=fwd(om,fbar=fbar(om)[,-1], sr=hke_brp,deviances = srres)
  
  plot(om)
  
  
  
  plot(om)+
    geom_line(aes(year,data,col=iter),data=plot(iter(window(om,end=100),1:3))$data)+
    theme(legend.position="none")
  
  plot(FLQuants(om,   
                "f" =   function(x) fbar(x)%/%refpts(hke_brp)["msy","harvest"], 
                "rec" = function(x) rec(x)%/%refpts( hke_brp)["msy","rec"], 
                "ssb" = function(x) ssb(x)%/%refpts( hke_brp)["msy","ssb"], 
                "catch"=function(x) landings(x)%/%refpts(hke_brp)["msy","yield"])) + 
    geom_hline(aes(yintercept=1),col="red",linetype=2) 
  
  
  mw<-c(colSums(stock.wt(om)[1:21,]*stock.n(om)[1:21,])/colSums(stock.n(om)[1:21,]))
  #mws<-mw/max(mw)
  ssb<-c(ssb(om))
  #ssbs<-ssb/max(ssb)
  f<-c(harvest(om)[1,])
  #fs<-f/max(f)
  rec<-c(rec(om))
  stock.num<-colSums(stock.n(om))
  stk<-colSums(stock(om))
  cat<-colSums(catch(om))
  
  
  
  #fs<-f/max(f)
  # 
  # setEPS()
  # postscript("C:/Users/LukeB/OneDrive - GMIT/Desktop/Desktop/DD_assesssment_ etc/simulated_stock.eps", horizontal = FALSE, onefile = FALSE, paper = "special",width=12, height=8,pointsize=12)
  
  
  par(mfrow=c(2,3))
  
  plot(mw,main="Mean weight of population",type="b",ylim=c(min(mw),max(mw)),ylab="mean weight (kg)",xlab ="Year")
  abline(v=c(20,60),lty=2,col="red")
  plot(ssb,main="SSB",type="b",ylim=c(min(ssb),max(ssb)),ylab="Spawning stock biomass (tonnes)",xlab ="Year")
  abline(v=c(20,60),lty=2,col="red")
  
  plot(f,main="Fishing mortality",type="b",ylim=c(min(f),max(f)),ylab="F",xlab ="Year")
  abline(v=c(20,60),lty=2,col="red")
  
  plot(rec,main="Recruit numbers",type="b",ylim=c(min(rec),max(rec)),ylab="Number (x 1000)",xlab ="Year")
  abline(v=c(20,60),lty=2,col="red")
  
  plot(stock.num,main="Stock numbers",type="b",ylim=c(min(stock.num),max(stock.num)),ylab="Number (x 1000)",xlab ="Year")
  abline(v=c(20,60),lty=2,col="red")
  
  plot(rec/stock.num,main="Proportion of recruits in stock",type="b",ylim=c(min(rec/stock.num),max(rec/stock.num)),ylab="Proportion",xlab ="Year")
  abline(v=c(20,60),lty=2,col="red")
  
  #dev.off()
  
  
  
  hke_stk<-om
  
  sr1 <- FLSR()
  p4sr <- as.FLSR(hke_stk)#,rec.age=0:2) # ssb units is kg but should be tonnes
  summary(p4sr)
  units(p4sr)["ssb"]<-"t"
  model(p4sr) <- bevholt()
  p4sr<-fmle(p4sr)
  plot(p4sr)
  
  
  
  q1 <- 1e-6 * FLQuant(rep(1,times = dims(hke_stk)$age ),
                       dim = dim(hke_stk), dimnames = dimnames(landings.n(hke_stk)))
  
  #q2 <- 1e-4 * FLQuant(rep(1,times = dims(hke_stk)$age ),
  #                    dim = dim(hke_stk), dimnames = dimnames(landings.n(hke_stk)))
  
  
  
  
  ## measurement error on index
  eps_1 <- FLQuant(rlnorm(n = prod(dim(hke_stk)), sdlog = 0.1),
                   dim = dim(hke_stk), dimnames = dimnames(landings.n(hke_stk)))
  
  eps_2 <- FLQuant(rlnorm(n = prod(dim(hke_stk)), sdlog = .2),
                   dim = dim(hke_stk), dimnames = dimnames(landings.n(hke_stk)))
  
  eps_3 <- FLQuant(rlnorm(n = prod(dim(hke_stk)), sdlog = 0.3),
                   dim = dim(hke_stk), dimnames = dimnames(landings.n(hke_stk)))
  
  
  ## in biomass units
  # idx_1.1 <- FLIndex(index = q1 * stock.n(hke_stk)  * eps_1)#stock.wt(hke_stk)
  # idx_1.2 <- FLIndex(index = q1 * stock.n(hke_stk)  * eps_1)#stock.wt(hke_stk)
  # idx_1.3 <- FLIndex(index = q1 * stock.n(hke_stk)  * eps_1)#stock.wt(hke_stk)
  
  timing=0
  idx_1.1 <- FLIndex(index = q1 * stock.n(hke_stk)*exp(-(harvest(hke_stk) * timing + m(hke_stk) * timing)) *  eps_1)
  idx_1.2 <- FLIndex(index = q1 * stock.n(hke_stk)*exp(-(harvest(hke_stk) * timing + m(hke_stk) * timing)) *  eps_2)
  idx_1.3 <- FLIndex(index = q1 * stock.n(hke_stk)*exp(-(harvest(hke_stk) * timing + m(hke_stk) * timing)) *  eps_3)
  
  
  timing=0.875
  
  idx_2.1 <- FLIndex(index = q1 * stock.n(hke_stk)*exp(-(harvest(hke_stk) * timing + m(hke_stk) * timing)) *  eps_1)#stock.wt(hke_stk) *
  idx_2.2 <- FLIndex(index = q1 * stock.n(hke_stk)*exp(-(harvest(hke_stk) * timing + m(hke_stk) * timing)) *  eps_2)#stock.wt(hke_stk) *
  idx_2.3 <- FLIndex(index = q1 * stock.n(hke_stk)*exp(-(harvest(hke_stk) * timing + m(hke_stk) * timing)) *  eps_3)#stock.wt(hke_stk) *
  
  ### set the survey timing (fix this)
  range(idx_1.1)[c("startf", "endf")] <- c(0, 0)
  range(idx_1.2)[c("startf", "endf")] <- c(0, 0)
  range(idx_1.3)[c("startf", "endf")] <- c(0, 0)
  
  range(idx_2.1)[c("startf", "endf")] <- c(0.875, 0.875)
  range(idx_2.2)[c("startf", "endf")] <- c(0.875, 0.875)
  range(idx_2.3)[c("startf", "endf")] <- c(0.875, 0.875)
  
  
  #range(idx_2)[c("startf", "endf")] <- c(0.875, 0.875)
  #range(idx_3)[c("startf", "endf")] <- c(0, 0)
  
  
  ## rename the true stock
  hke_gsa_xy_true <- hke_stk
  
  ## copy to the observed stock
  hke_gsa_xy <- hke_stk
  eps_c <- FLQuant(rlnorm(n = prod(dim(hke_gsa_xy)), sdlog = 0.3),
                   dim = dim(hke_gsa_xy), dimnames = dimnames(landings.n(hke_gsa_xy)))
  catch.n(hke_gsa_xy) <- catch.n(hke_stk) * eps_c
  catch(hke_gsa_xy) <- computeCatch(hke_gsa_xy)
  
  dev.off()
  mw1<-c(colSums(catch.wt(hke_gsa_xy_true)[1:21,]*catch.n(hke_gsa_xy_true)[1:21,])/colSums(catch.n(hke_gsa_xy_true)[1:21,]))
  mw2<-c(colSums(catch.wt(hke_gsa_xy)[1:21,]*catch.n(hke_gsa_xy)[1:21,])/colSums(catch.n(hke_gsa_xy)[1:21,]))
  plot(mw,main="Mean weight of population",type="b",ylim=c(min(mw),max(mw)),ylab="mean weight (kg)",xlab ="Year")
  lines(mw1,type="b",col=2)
  lines(mw2,type="b",col=3)
  
  ## clear the numbers and fishing mortality
  stock.n(hke_gsa_xy)[] <- NA
  harvest(hke_gsa_xy)[] <- NA
  
  
}






# SIMS & INPUT
sims <- list()
input <- list()


## set the seed
set.seed(102)




