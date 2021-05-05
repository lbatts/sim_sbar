##----------------------------
## A quick look at Schnute 3C. A More Complex Model
## CM: 18/03/2021
## Note everything in mass units - so Nt is biomass for example
##----------------------------
library(FLCore)
library(FLasher)
rm(list=ls())
##load("scenario8_stkstored.RData")
#load("res_10iters_sce4_8_new.RData")

load(("res_10iters_sce3_7_n.RData"))  # Anglefish
#load(("res_10iters_sce4_8_new2.RData")) # Herring

## selectivity scenario
scen <- 1

## iteration
it <- 8
stk <- iter(tmpres[[scen]]$stk_c$stk, it)
idx<- iter(tmpres[[scen]]$stk_c$idx, it)
## true biomass
Nt_true <- an(apply(catch.wt(stk) * stock.n(stk), 2, sum))

Rt_true <- an((catch.wt(stk) * stock.n(stk))["0",])

## get the information required for model 3B

##-----------
## VARIABLES 
##-----------
## average weight of the recruited population
#Xt <- an(apply(catch.wt(stk) * stock.n(stk), 2, sum) / apply(stock.n(stk), 2, sum))

Xt <-tmpres[[scen]]$stk_c$nocut$mwts[3,,it] 
Xt_true <-an(apply(catch.wt(stk) * stock.n(stk), 2, sum) / apply(stock.n(stk), 2, sum))
Xt2 <-tmpres[[scen]]$stk_c$nocut$mwts_nosel[3,,it] 
plot(Xt, type = "o",ylim=c(0,max(Xt,Xt_true)))
lines(Xt_true,type="o",col=2)
lines(Xt2,type="o",col=3)

#points(tmpres[[scen]]$stk_c$nocut$mwts[3,,it],col=2,type="o")
## weight of the new recruits
Yt <-tmpres[[scen]]$stk_c$nocut$mwts[1,,it] 
Yt_true <-an(catch.wt(stk)["0",])
Yt2 <-tmpres[[scen]]$stk_c$nocut$mwts_nosel[1,,it] 



prages<-c(1:range(stk)["max"])
## average weight of fish in the previously exploited population
#Zt <- an(apply(catch.wt(stk)[paste(1:8),] * stock.n(stk)[paste(1:8), ], 2, sum) / apply(stock.n(stk)[paste(1:8), ], 2, sum))
Zt <-tmpres[[scen]]$stk_c$nocut$mwts[2,,it] 
 Zt_true <- an(apply(catch.wt(stk)[paste(prages),] * stock.n(stk)[paste(prages), ], 2, sum) / apply(stock.n(stk)[paste(prages), ], 2, sum))
 Zt2 <-tmpres[[scen]]$stk_c$nocut$mwts_nosel[2,,it] 
 
 plot(Zt, type = "o",ylim=c(0,max(Zt,Zt_true)))
 lines(Zt_true,type="o",col=2)
 lines(Zt2,type="o",col=3)
 
 plot(Zt, type = "o",ylim=c(0,max(Zt)))
 lines(Xt,type="o",col=2)
 
  lines(Xt_true,type="o",col=2,lty=2)
  lines(Zt_true,type="o",col=1,lty=2)
 
  
  #these are the same when knife edged selection
#  Xt<-Xt2
 # Yt<-Yt2
  #Zt<-Zt2
  
 omegat <- Yt/Xt * (Zt - Xt)/(Zt - Yt)

## index
q <- 1e-6
It <- an(apply(catch.wt(stk) * index(idx), 2, sum)) # the index created with error
 #It <-an(q * apply(catch.wt(stk) * stock.n(stk), 2, sum))
dev.off()

# plot(q*Rt_true,type="o")
# lines(c(catch.wt(stk)[1,] * index(idx)[1,]),type="o",col=2)
# 
# plot(c(q*(catch.wt(stk)[2,] * stock.n(stk)[2,])),type="o")
# lines(c(catch.wt(stk)[2,] * index(idx)[2,]),type="o",col=2)
# 
# plot(c(q*(catch.wt(stk)[3,] * stock.n(stk)[3,])),type="o")
# lines(c(catch.wt(stk)[3,] * index(idx)[3,]),type="o",col=2)
# 
# plot(c(q*(catch.wt(stk)[4,] * stock.n(stk)[4,])),type="o")
# lines(c(catch.wt(stk)[4,] * index(idx)[4,]),type="o",col=2)

## catch
Ct <-an(apply(catch.wt(stk) * catch.n(idx), 2, sum)) 
Ct_true <-an(apply(catch.wt(stk) * catch.n(stk), 2, sum)) 

    #an(apply(catch.wt(stk) * catch.n(stk), 2, sum))
##Ct <- an(apply(catch.wt(stk) * catch.n(stk), 2, sum))
plot(It/q, type = "o", ylim = c(0, max(It/q)))
lines(Nt_true,col=2)
plot(Ct, type = "o", pch = 19)
lines(Ct_true,  pch = 19,col=2)

##------------
## PARAMETERS
##------------
## growth
#source("par_setup.R")
# mpar <- FLPar("linf" = 171, "k" = 0.1075, "t0" = -.25, "a" = 0.0000303, "b" = 2.82,"a50" = 4.5)
# # 
#  W_d<- (mpar["a"]*mpar["linf"]^mpar["b"])*(1 - exp(-mpar["k"])) # as derived in Schnute
#  rho_d<- exp(-mpar["k"]) # # as derived in Schnute

# hpar <- FLPar("linf" = 3.09e+01,"k" = 3.51e-01, "t0" = -8.69e-01, "a" = 6.27e-06 ,"b" = 3.09e+00)
# 
# W_d<- (hpar["a"]*hpar["linf"]^hpar["b"])*(1 - exp(-hpar["k"])) # as derived in Schnute
# rho_d<- exp(-hpar["k"])

w <- an(catch.wt(stk)[,"40"]) # #according to equation at the start of Schnute
fit <- lm(w[2:11] ~ w[1:10])# #according to equation at the start of Schnute
#fit <- lm(w[2:5] ~ w[1:4])# #according to equation at the start of Schnute
#fit <- lm(w[2:21] ~ w[1:20])

## check these out!
W <- an(coef(fit)[1])
rho <- an(coef(fit)[2])

#par(mfrow=c(2,1))
plot(Xt[-40], Zt[-1])
abline(lm(Zt[-1] ~ Xt[-40]), lty = 2) ## return to this
abline(c(W, rho))
#abline(c(W_d, rho_d),col=3)


#look at weights over entire age range
plot(w[-length(w)], w[-1])
abline(c(W, rho))
abline(lm(Zt[-1] ~ Xt[-40]), lty = 2) ## return to this
#abline(c(W_d, rho_d),col=3)

plot(Xt[-40], Zt[-1],ylim=c(0,max(w)),xlim=c(0,max(w)))
points(w[-length(w)], w[-1],col=2)
abline(lm(Zt[-1] ~ Xt[-40]), lty = 2) ## return to this
abline(c(W, rho),col=2)
#abline(c(W_d, rho_d),col=3)

fit2<-lm(Zt[-1] ~ Xt[-40])
W2 <- an(coef(fit2)[1])
rho2 <- an(coef(fit2)[2])


W;W2;W_d #big difference between these three values! Yet from same theory!
rho;rho2;rho_d
# W<-W_d
# rho<-rho_d

## natural mortality - all the same so just choosing one age and year
M <- an(m(stk)["1", "40"])
sigma <- exp(-M)

## timing
mu <- 0.5## fraction of catch before natural mortality - more complicated in Luke's second version but trialing here
nu <- an(m.spwn(stk)["1", "40"]) ## fraction of total mortality that occurs before spawning
theta <- 0 ## fraction of year before index derived

## recruitment
## ages at which fish can reach a weight in the interval [V, V')
## k = K = 0 here, I think

## lognormal process error only on index
schnute3c_B5b_nll <- function(pars,growth){
    q <- plogis(pars[1])
    s <- exp(pars[2])
    W <- growth[1]
    rho <- growth[2]
    ##
    Nt <- St <- Pt1_hat <- Rt1_hat <- It1_hat <- rep(NA, length(Xt))
    ## Table 2
    for(t in 1:length(Xt)){
        Nt[t] <- (It[t] + q * theta * (1 - mu * (1 - sigma)) * Ct[t]) / (q * (1 - theta * (1 - sigma)))
        ##
        St[t] <- (1 - nu * (1 - sigma)) * Nt[t] - nu * (1 - mu * (1 - sigma)) * Ct[t]
        ##
        Pt1_hat[t+1] <- (rho + W/Xt[t]) * (sigma * (Nt[t] - mu * Ct[t]) - (1 - mu) * Ct[t])
        ## Eq B5b
        It1_hat[t+1] <-
            max(q * ((1-theta * (1 - sigma))/(1 - omegat[t+1]) * Pt1_hat[t+1] - theta * (1 - mu * (1 - sigma)) * Ct[t+1]), 1e-8)
    }
    ll <- sum(dnorm(log(It[2:length(Xt)]), log(It1_hat[2:length(Xt)]), sd = s, log = TRUE))
    return(-ll)
}

## try a genetic algorithm - slow
## library(DEoptim)
## niter <- 1e3
## fit <- DEoptim(schnute3c_B5b_nll, 
##                lower = c(qlogis(1e-9), log(1e-6)),
##                upper = c(qlogis(1e-3), log(3)),
##                control = list(trace = TRUE, itermax = niter))

## ## visualise convergence
## par(mfrow = c(2, 2))
## for(i in 1:4){
##     plot(1:niter, fit$member$bestmemit[,i])
## }

#pars <- fit$optim$bestmem


#three fits
fit1 <- nlminb(start = c(qlogis(1e-8), log(0.1)),
              schnute3c_B5b_nll, growth = c(W,rho))#, 

schnute3c_B5b_nll(c(qlogis(1e-8), log(0.1)), growth = c(W,rho))

fit2 <- nlminb(start = c(qlogis(1e-8), log(0.1)),
               schnute3c_B5b_nll, growth = c(W2,rho2))#,



# fit3 <- nlminb(start = c(qlogis(1e-8), log(0.1)),
#                schnute3c_B5b_nll, growth = c(W_d,rho_d))#, 

length(Ct)


pars1 <- fit1$par
pars2 <- fit2$par
#pars3 <- fit3$par

q1 <- plogis(pars1[1])
s1 <- exp(pars1[2])
q2 <- plogis(pars2[1])
s2 <- exp(pars2[2])
#q3<- plogis(pars3[1])
#s3 <- exp(pars3[2])

q1;q2;q3 ## for scenario 1 (knife edged)
##

output<-function(pars,growth){
    q <- plogis(pars[1])
    s <- exp(pars[2])
    W <- growth[1]
    rho <- growth[2]
    Nt <- St <- Pt1_hat <- It1_hat <- rep(NA, length(Xt))
## Table 2
for(t in 1:length(Xt)){
    Nt[t] <- (It[t] + q * theta * (1 - mu * (1 - sigma)) * Ct[t]) / (q * (1 - theta * (1 - sigma)))
    ##
    St[t] <- (1 - nu * (1 - sigma)) * Nt[t] - nu * (1 - mu * (1 - sigma)) * Ct[t]
    ##
    Pt1_hat[t+1] <- (rho + W/Xt[t]) * (sigma * (Nt[t] - mu * Ct[t]) - (1 - mu) * Ct[t])
    ## Eq B5b
    It1_hat[t+1] <-
        max(q * ((1-theta * (1 - sigma))/(1 - omegat[t+1]) * Pt1_hat[t+1] - theta * (1 - mu * (1 - sigma)) * Ct[t+1]), 1e-8)
}
return(Nt)
}


Nt1<-output(pars1,growth=c(W,rho))
Nt2<-output(pars2,growth=c(W2,rho2))
#Nt3<-output(pars3,growth=c(W_d,rho_d))
length(Ct)
#
plot(Nt_true, bty = "l", pch = 19, ylim = c(0, max(Nt_true ,Nt2,Nt1 ,na.rm = TRUE)))
lines(Nt1,col=2)
lines(Nt2,col=3)

#cross check and other assessments
rv<-tmpres[[scen]]$stk_c$nocut$rv[3,,it] # real values extracted from storage
salt<-tmpres[[scen]]$stk_c$nocut$schnualt[3,,it]
sb0<-tmpres[[scen]]$stk_c$nocut$schnub0[3,,it]
salt2<-tmpres[[scen]]$stk_c$nocut$schnualt2[3,,it]
sb02<-tmpres[[scen]]$stk_c$nocut$schnub02[3,,it]

# b5b<-tmpres[[scen]]$stk_c$nocut$b5b[1,,it]
# b5a<-tmpres[[scen]]$stk_c$nocut$b5a[1,,it]

#idx<-iter(tmpres[[scen]]$stk_c$idx,it)
#cate<-an(apply(catch.wt(stk) * catch.n(idx), 2, sum))

lines(rv,col=1)
lines(salt,col=3,lty=2,type="o")
lines(salt2,col=2,lty=2,type="o")

lines(sb0,col="purple",lty=3,type="o") #very close
lines(sb02,col="pink",lty=3,type="o") #very close


load("check.Rdata")
csub<-subset(check,check$HD=="stk_c" & check$iter==8)

points(unique(csub$rv[csub$var=="stk_wgt"]),pch=3,col=1)

points(unique(csub$value[csub$var=="stk_wgt" & csub$assessment=="sobnw"]),pch=3,col=2)
points(unique(csub$value[csub$var=="stk_wgt" & csub$assessment=="so"]),pch=3,col=3)

points(unique(csub$value[csub$var=="stk_wgt" & csub$assessment=="sb0nbw"]),pch=3,col="pink")
points(unique(csub$value[csub$var=="stk_wgt" & csub$assessment=="sb0"]),pch=3,col="purple")


#####numbers

plot(Nt_true/Xt, bty = "l", pch = 19, ylim = c(0, max(Nt_true/Xt ,Nt2/Xt,Nt1/Xt ,na.rm = TRUE)))
lines(Nt1/Xt,col=2)
lines(Nt2/Xt,col=3)

#cross check and other assessments
rv<-tmpres[[scen]]$stk_c$nocut$rv[6,,it] # real values extracted from storage
salt<-tmpres[[scen]]$stk_c$nocut$schnualt[6,,it]
sb0<-tmpres[[scen]]$stk_c$nocut$schnub0[6,,it]
salt2<-tmpres[[scen]]$stk_c$nocut$schnualt2[6,,it]
sb02<-tmpres[[scen]]$stk_c$nocut$schnub02[6,,it]

# b5b<-tmpres[[scen]]$stk_c$nocut$b5b[1,,it]
# b5a<-tmpres[[scen]]$stk_c$nocut$b5a[1,,it]

#idx<-iter(tmpres[[scen]]$stk_c$idx,it)
#cate<-an(apply(catch.wt(stk) * catch.n(idx), 2, sum))

lines(rv,col=1)
lines(salt,col=3,lty=2,type="o")
lines(salt2,col=2,lty=2,type="o")

lines(sb0,col="purple",lty=3,type="o") #very close
lines(sb02,col="pink",lty=3,type="o") #very close

points(unique(csub$rv[csub$var=="stk_no"]),pch=3,col=1)

points(unique(csub$value[csub$var=="stk_no" & csub$assessment=="sobnw"]),pch=3,col=2)
points(unique(csub$value[csub$var=="stk_no" & csub$assessment=="so"]),pch=3,col=3)

points(unique(csub$value[csub$var=="stk_no" & csub$assessment=="sb0nbw"]),pch=3,col="pink")
points(unique(csub$value[csub$var=="stk_no" & csub$assessment=="sb0"]),pch=3,col="purple")

lines(unique(csub$value[csub$var=="stk_no" & csub$assessment=="csa"]),pch=3,col="orange")

#==============================================================================
# F

rv<-tmpres[[scen]]$stk_c$nocut$rv[7,,it] # real values extracted from storage
#salt<-tmpres[[scen]]$stk_c$nocut$schnualt[7,,it]
sb0<-tmpres[[scen]]$stk_c$nocut$schnub0[7,,it]
#salt2<-tmpres[[scen]]$stk_c$nocut$schnualt2[6,,it]
sb02<-tmpres[[scen]]$stk_c$nocut$schnub02[7,,it]
csa<-tmpres[[scen]]$stk_c$nocut$csa[7,,it]

# b5b<-tmpres[[scen]]$stk_c$nocut$b5b[1,,it]
# b5a<-tmpres[[scen]]$stk_c$nocut$b5a[1,,it]

#idx<-iter(tmpres[[scen]]$stk_c$idx,it)
#cate<-an(apply(catch.wt(stk) * catch.n(idx), 2, sum))

plot(rv,col=1,type="o",ylim=c(0,max(rv,sb0,sb02,csa)))

lines(sb0,col="purple",lty=1,type="o") #very close
lines(sb02,col="pink",lty=1,type="o") #very close
lines(csa,col="orange",lty=1,type="o") #very close

points(unique(csub$rv[csub$var=="f"]),pch=3,col=1)


points(unique(csub$value[csub$var=="f" & csub$assessment=="sb0nbw"]),pch=3,col="pink")
points(unique(csub$value[csub$var=="f" & csub$assessment=="sb0"]),pch=3,col="purple")

points(unique(csub$value[csub$var=="f" & csub$assessment=="csa"]),pch=3,col="orange")
