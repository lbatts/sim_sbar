#===================================
# Functions for plotting simulation runs
# Author : LB
#Distributed under GPL-3 or later
#Feb 2021
#===================================
#x<-tmpres[[1]]$stk_c
#x<-i$stk_c

res_extract<-function(x){
  #x<-i$stk_c 
  if(length(names(x))==4){
    
    yy<-lapply(x[3:4],res_extract_inner)
    cutdat<- dplyr::bind_rows(yy, .id = "aorif")
    
  }else{
    yy<-lapply(x[3],res_extract_inner)
    cutdat<- dplyr::bind_rows(yy, .id = "aorif")
    
  } 
  
  return(cutdat)
}

res_extract_inner<-function(y){

#y<-x$nocut
  rv<-melt(y$rv)
  head(rv)
  # levels(factor(rv$Var1))# rec_wgt, postrec_wgt, stk_wgt, rec_no, postrec_no, stk_no, f
  # levels(factor(rv$Var2))#year
  # levels(factor(rv$Var3))#iter
  #
  rvvars<-c("rec_wgt", "postrec_wgt", "stk_wgt", "rec_no", "postrec_no", "stk_no", "f")
  rvlevels<-levels(factor(rv$Var1))# rec_wgt, postrec_wgt, stk_wgt, rec_no, postrec_no, stk_no, f
  lookup<-data.frame(rvvars,rvlevels)

  rv$var<-lookup$rvvars[match(rv$Var1,lookup$rvlevels)]
  rv$type <- "rv"
  
  cs<-melt(y$csa)
  lookup$csalevels<-c(1:7)
  cs$var<-lookup$rvvars[match(cs$Var1,lookup$csalevels)]
  cs$type<-"csa"

  schnualt<-melt(y$schnualt)
  lookup$schnulevels<-c(1:7)
  schnualt$var<-lookup$rvvars[match(schnualt$Var1,lookup$schnulevels)]
  schnualt$type<-"so"

  schnub0<-melt(y$schnub0)
  schnub0$var<-lookup$rvvars[match(schnub0$Var1,lookup$schnulevels)]
  schnub0$type<-"sb0"

  schnualt2<-melt(y$schnualt2)
  lookup$schnulevels<-c(1:7)
  schnualt2$var<-lookup$rvvars[match(schnualt2$Var1,lookup$schnulevels)]
  schnualt2$type<-"soubw"
  
  schnub02<-melt(y$schnub02)
  schnub02$var<-lookup$rvvars[match(schnub02$Var1,lookup$schnulevels)]
  schnub02$type<-"sb0ubw"
  
dat<-rbind(rv,cs,schnualt,schnub0,schnualt2,schnub02)
colnames(dat)[2:3]<-c("year","iter")
# dim(dat)
# dim(dat[!complete.cases(dat[ , 4]),])
#dat<-dat[complete.cases(dat[ , 4]),]


return(dat[,2:6])



}


par_extract<-function(x){

  #x<-i$stk_c 
  if(length(names(x))==4){
  
  yy<-lapply(x[3:4],par_extract_inner)
  cutdat<- dplyr::bind_rows(yy, .id = "aorif")
  
  }else{
    yy<-lapply(x[3],par_extract_inner)
    cutdat<- dplyr::bind_rows(yy, .id = "aorif")
   
   } 
  
  return(cutdat)
     
}



par_extract_inner<-function(y){
  
#y<-x[[3]]
#y$rv_par[,,1]
  dm<-length(y$rv[6,,1])
  
  tmprv<-array(NA,c((dm+7),dim(y$rv_par)[2],dim(y$rv_par)[3]))
  tmprv[1:10,1:2,]<-y$rv_par
  tmprv[8:(dm+7),1,]<-y$rv[6,,]
  
  #tmprv[c(8,27),1,6]==y$rv_par[c(8,10),1,6]
  #tmprv[c(8:27),1,6]==y$rv[6,,6]
  
  
rv_par<-melt(tmprv)
rv_par<-dcast(rv_par, ...~Var2, value.var='value')


rvvars<-c("qhat", NA, "phat1", "B0", "F1", "Fmid", "Fend", paste0(rep("N",dm),1:dm ))
rvlevels<-levels(factor(rv_par$Var1))# rec_wgt, postrec_wgt, stk_wgt, rec_no, postrec_no, stk_no, f
lookup<-data.frame(rvvars,rvlevels)
# rv_par$errors <- "rv" 
# rv_par$t1<-NA
# rv_par$t2<-NA
# rv_par$t1[!is.na(rv_par$`1`)] <- rv_par$`1`[!is.na(rv_par$`1`)] 
# rv_par$t2[!is.na(rv_par$`2`)] <- rv_par$`2`[!is.na(rv_par$`2`)]

rv_par$var<-lookup$rvvars[match(rv_par$Var1,lookup$rvlevels)]
rv_par$type <- "rv"
rv_par$var<-as.character(rv_par$var)
#dim(rv_par)

# 
# errors<-c("obj evals to NaN", "singular convergence (7)" ,"false convergence (8)", "try different starting pars","rho error")


cs<-melt(y$csa_par)
cs<-dcast(cs, ...~Var2, value.var='value')
#cs$errors <- as.character(ifelse(cs$`1` %in% errors, cs$`1`, NA))
#cs$`1`[which(cs$`1`%in% errors)] <- NA
cs[,3]<-as.numeric(cs[,3])
cs[,4]<-as.numeric(cs[,4])
#cs$t1<-NA
#cs$t2<-NA
#cs$t1[!is.na(cs$`1`)] <- cs$`1`[!is.na(cs$`1`)] #with(cs[!is.na(cs$`1`),],ifelse(Var1 == 1, plogis(`1`),exp(`1`)))
#cs$t2[!is.na(cs$`2`)] <- cs$`2`[!is.na(cs$`2`)] #with(cs[!is.na(cs$`2`),],ifelse(Var1 == 1, plogis(`2`),exp(`2`)))
lookup<-data.frame(vars = 1:(dm+7), csalevels =  c("qhat", "surveysigma","phat1",NA,"F1", "Fmid", "Fend", paste0(rep("N",dm),1:dm )))
cs$var<-lookup$csalevels[match(cs$Var1,lookup$vars)]
cs$var<-as.character(cs$var)
#suppressWarnings(cs$var[which(cs$errors%in% errors)] <-  "all")
cs$type<-"csa"
cs$var<-as.character(cs$var)

#cs
#cs[complete.cases(cs[ , "var"]),]


salt<-melt(y$schnualt_par)
salt<-dcast(salt, ...~Var2, value.var='value')
salt[,3]<-as.numeric(salt[,3])
salt[,4]<-as.numeric(salt[,4])
lookup$saltlevels <- c("qhat", "surveysigma",NA,NA,NA, NA, NA, paste0(rep("N",dm),1:dm ))
salt$var<-lookup$saltlevels[match(salt$Var1,lookup$vars)]
salt$var<-as.character(salt$var)
salt$type<-"so"


sb0<-melt(y$schnub0_par)
sb0<-dcast(sb0, ...~Var2, value.var='value')
sb0[,3]<-as.numeric(sb0[,3])
sb0[,4]<-as.numeric(sb0[,4])
lookup$sb0levels <- c("qhat", "surveysigma","B0",NA,"F1", "Fmid", "Fend", paste0(rep("N",dm),1:dm ))
sb0$var<-lookup$sb0levels[match(sb0$Var1,lookup$vars)]
sb0$var<-as.character(sb0$var)
sb0$type<-"sb0"


salt2<-melt(y$schnualt2_par)
salt2<-dcast(salt2, ...~Var2, value.var='value')
salt2[,3]<-as.numeric(salt2[,3])
salt2[,4]<-as.numeric(salt2[,4])
lookup$salt2levels <- c("qhat", "surveysigma",NA,NA,NA, NA, NA,paste0(rep("N",dm),1:dm ))
salt2$var<-lookup$salt2levels[match(salt2$Var1,lookup$vars)]
salt2$var<-as.character(salt2$var)
salt2$type<-"soubw"
# 
# matchString <- paste0("convergence", collapse = "\\b|")
# matchString <- paste0("\\b", matchString, "\\b")
# unique(sb02$`1`[grepl(pattern = matchString, x = sb02$`1`)])

sb02<-melt(y$schnub02_par)
sb02<-dcast(sb02, ...~Var2, value.var='value')
sb02[,3]<-as.numeric(sb02[,3])
sb02[,4]<-as.numeric(sb02[,4])
lookup$sb02levels <- c("qhat", "surveysigma","B0",NA,"F1", "Fmid", "Fend", paste0(rep("N",dm),1:dm ))
sb02$var<-lookup$sb02levels[match(sb02$Var1,lookup$vars)]
sb02$var<-as.character(sb02$var)
sb02$type<-"sb0ubw"


# str(cs);
# str(salt)
dat<-rbind(rv_par,cs,salt,sb0,salt2,sb02)[2:6]
colnames(dat)[1]<-c("iter")
#colnames(dat)[2:3]<-c("rawest","rawse")
colnames(dat)[2:3]<-c("est","se")

#dat[dat$iter==1 & dat$type=="csa",]

#head(dat)
# dim(dat)
#dim(dat[complete.cases(dat[ , "var"]),])
dat<-dat[complete.cases(dat[ , "var"]),]

# (90*3)+80
#dim(dat)
return(dat)

}



error_extract<-function(x){
  
  #x<-i$stk_c 
  if(length(names(x))==4){
    
    yy<-lapply(x[3:4],error_extract_inner)
    cutdat<- dplyr::bind_rows(yy, .id = "aorif")
    
  }else{
    yy<-lapply(x[3],error_extract_inner)
    cutdat<- dplyr::bind_rows(yy, .id = "aorif")
    
  } 
  
  return(cutdat)
  
}



error_extract_inner<-function(y){
  
  #y<-x[[3]]
  cs<-melt(y$csa_errors)
  cs<-dcast(cs, ...~Var2, value.var='value')
  cs[,3]<-as.numeric(cs[,3])
  cs$type<-"csa"
  
  schnualt<-melt(y$schnualt_errors)
  schnualt<-dcast(schnualt, ...~Var2, value.var='value')
  schnualt[,3]<-as.numeric(schnualt[,3])
  schnualt$type<-"so"
  
  schnub0<-melt(y$schnub0_errors)
  schnub0<-dcast(schnub0, ...~Var2, value.var='value')
  schnub0[,3]<-as.numeric(schnub0[,3])
  schnub0$type<-"sb0"
  
  
  schnualt2<-melt(y$schnualt2_errors)
  schnualt2<-dcast(schnualt2, ...~Var2, value.var='value')
  schnualt2[,3]<-as.numeric(schnualt2[,3])
  schnualt2$type<-"soubw"
  
  schnub02<-melt(y$schnub02_errors)
  schnub02<-dcast(schnub02, ...~Var2, value.var='value')
  schnub02[,3]<-as.numeric(schnub02[,3])
  schnub02$type<-"sb0ubw"
  
  
  
  
  
  dat<-rbind(cs,schnualt,schnub0,schnualt2,schnub02)[2:5]
  colnames(dat)[1]<-c("iter")
  #colnames(dat)[2:3]<-c("rawest","rawse")
  colnames(dat)[2:3]<-c("convergence","Error")
  
  return(dat)
  
}
