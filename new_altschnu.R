

library(sbar)
catch_kg1<-Ct
obs<-matrix(It,nrow=1)
mean_wts <-tmpres[[scen]]$stk_c$nocut$mwts[,,it]
no.years<-length(catch_kg1)

data_tmb <- list(
  obs_catch = catch_kg1,
  obs_ind = obs,
  indices_class = c(2),
  indices_ts = 0,
  mean_wts=mean_wts,
  nu=0,
  mu=0.5,
  SRcode=2,
  spawn_prop=rep(1,length(catch_kg1)),
  l_calc_wt = c(1)
  )

params <- list(
  logitq = stats::qlogis(1e-8),
  logindex_sigma = log(0.1),
  logrec_param = log(c(100000,30000)),
  logitsigma = stats::qlogis(sigma),
  logrho=log(rho),
  logW=log(W)
  )

obj <- TMB::MakeADFun(
  data = c(model = "schnute_orig", data_tmb),
  parameters = params,
  map = list(
    logindex_sigma = factor(1),
    logW = factor(NA),
    logrho = factor(NA),
    logitsigma = factor(NA),
    logrec_param = factor(rep(NA,2))
  ),
  hessian = TRUE,
  silent = TRUE,
  DLL = "sbar_TMBExports")

obj$fn()


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
qest<-plogis(obs.srep[rownames(obs.srep)=="logitq","Estimate"]) #

qest
q1
bio2<-(obs.srep[rownames(obs.srep)=="biomass","Estimate"]) #

plot(Nt_true)
lines(bio,col=2,type="o")
lines(bio2,type="o")
lines(bio3,type="o",col=4)



args(schnute_orig)

obj2<-schnute_orig(version =2,catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho, W = W , start_q = 1e-08, start_indexsigma = 0.1, start_sigma = sigma, fix_sigma = TRUE, fix_indexsigma = FALSE)

obj2$fn()# 1113440

# #for more flexibility in set up
# 
# (opt <- nlminb(start=obj$par,objective=obj$fn,gradient=obj$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10)))


#================================================================================================

data_tmb <- list(
  obs_catch = catch_kg1,
  obs_ind = obs,
  indices_class = c(2),
  indices_ts = 0,
  mean_wts=mean_wts,
  nu=0,
  SRcode=2,
  spawn_prop=rep(1,length(catch_kg1)),
  l_calc_wt = c(1)
)


params <- list(
  logq = log(1e-8),
  logB0 = log(5*max(catch_kg1)),
  logindex_sigma = log(0.1),
  logrec_param = log(c(100000,30000)),
  logitsigma = stats::qlogis(sigma),
  logrho=log(rho),
  logW=log(W),
  logf_calc = log(rep(0.3,length(catch_kg1))),
  logcatch_sigma = log(0.1)
)

obj <- TMB::MakeADFun(
  data = c(model = "schnute_new_V2", data_tmb),
  parameters = params,
  map = list(
    logindex_sigma = factor(1),
    logW = factor(NA),
    logrho = factor(NA),
    logitsigma = factor(NA),
    logrec_param = factor(rep(NA,2)),
    logcatch_sigma = factor(NA)
  ),
  hessian = TRUE,
  silent = TRUE,
  DLL = "sbar_TMBExports")

obj$fn()


args(schnute_obserror)

tmp<-schnute_obserror(version= 3,catchkg = catch_kg1, indiceskg = obs, ts = 0, mwts = mean_wts, tsp = 0, rho = rho, W = W , start_q = 1e-08, start_indexsigma = 0.1,start_rec_a = 3000,start_sigma = sigma,  start_f_calc = 0.3, start_catchsigma = 0.1, start_B0 = 5*max(catch_kg1),fix_sigma = TRUE, fix_B0 = FALSE, fix_indexsigma = FALSE,fix_catchsigma = TRUE)
 
tmp$fn()

(opt <- nlminb(start=tmp$par,objective=tmp$fn,gradient=tmp$gr,control = list(iter.max=100000,eval.max=100000,rel.tol=1e-10)))


srep<-TMB::sdreport(tmp)
res<-summary(srep)

res[row.names(res)=="qhat",]
#res[row.names(res)=="sigma",]
bioB0<-res[row.names(res)=="biomass","Estimate"]
lines(bioB0,col=2,type="o")
