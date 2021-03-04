#===================================
# Functions for plotting simulation runs
# Author : LB
#Distributed under GPL-3 or later
#Feb 2021
#===================================
#x<-tmpres[[1]]$stk_c
#x<-i$stk_c
res_extract<-function(x){
  
  
  rv<-melt(x$rv)
  head(rv)
  # levels(factor(rv$Var1))# rec_wgt, postrec_wgt, stk_wgt, rec_no, postrec_no, stk_no, f
  # levels(factor(rv$Var2))#year
  # levels(factor(rv$Var3))#iter
  # 
  rvvars<-c("rec_wgt", "postrec_wgt", "stk_wgt", "rec_no", "postrec_no", "stk_no", "f")
  rvlevels<-levels(factor(rv$Var1))# rec_wgt, postrec_wgt, stk_wgt, rec_no, postrec_no, stk_no, f
  lookup<-data.frame(rvvars,rvlevels)
  
  rv$var<-lookup$rvvars[match(rv$Var1,lookup$rvlevels)]
  rv$type <- "real values"
  
  #if()
  
  
  cs<-melt(x$csa)
  lookup$csalevels<-c(6,5,7,2,1,3,4)
  cs$var<-lookup$rvvars[match(cs$Var1,lookup$csalevels)]
  cs$type<-"CSA"
  
  schnualt<-melt(x$schnualt)
  lookup$schnulevels<-c(3,2,1,7,6,5,4)
  schnualt$var<-lookup$rvvars[match(schnualt$Var1,lookup$schnulevels)]
  schnualt$type<-"Schnute orig"
  
  schnub0<-melt(x$schnub0)
  schnub0$var<-lookup$rvvars[match(schnub0$Var1,lookup$schnulevels)]
  schnub0$type<-"Schnute B0 est"
  
dat<-rbind(rv,cs,schnualt,schnub0)
colnames(dat)[2:3]<-c("year","iter")
# dim(dat)
# dim(dat[!complete.cases(dat[ , 4]),])
#dat<-dat[complete.cases(dat[ , 4]),]
  

return(dat[,2:6])  
  
  
  
}