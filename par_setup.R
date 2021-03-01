source("funs.R")

#load database
load(url("https://github.com//fishnets//fishnets//blob//master//data//fishbase-web//fishbase-web.RData?raw=True"))

#Select Lophius piscatorius and Clupea harengus harengus as a case study
#MON <- subset(fb,species=="Lophius piscatorius") #scrape database MON
HER <- subset(fb,species=="Clupea harengus harengus")
rm(fb)
#MON <- MON[,c("sex","linf","k","t0","a","b")] # 
HER <- HER[,c("linf","k","t0","a","b")]  

#Get the means and create an FLPar object
#MON <- apply(MON[,2:6],2,mean,na.rm=T)
HER <- apply(HER[,1:5],2,mean,na.rm=T)


#MON["a"]<-MON["a"]*1e-3  #a should be smaller!
#HER["a"]<-HER["a"]*1e-3  #a should be smaller!

## Use as many parameters as possible that can be found in a4a assessment for anglerfish
mpar <- FLPar("linf" = 171, "k" = 0.1075, "t0" = -.5, "a" = 0.0000303, "b" = 2.82,"a50" = 4.5)
hpar <- FLPar(HER)


#fill in missing par as guestitmates from other life history parameters

mpar=lhPar(mpar)
hpar=lhPar(hpar)

hpar["a"]<-hpar["a"]*1e-3  #a should be smaller to have kilos
mpar["s"]<-0.8  # steepness of SR, this useed to derive params for bevholt later
hpar["s"]<-0.7  
hpar["v"] <-30000 
mpar["v"] <- 120000 # the highest observed stock biomass from a4a assessment is around this so lets say this for now, calculated with geomean Sr the V biomass is > 500000
  
sce_ls <- list(
  
  lh = list(mon = mpar, 
            her = hpar),
  range = list(mon = c(min=0,max=20,minfbar=1,maxfbar=20,plusgroup=20), 
               her = c(min=0,max=8,minfbar=2,maxfbar=8,plusgroup=8)),
  
  sel = list(mon = list(kn0 = 1, 
                        logistic = logistic(FLQuant(c(0:20+.5),dimnames=list(age = 0:20)),FLPar("asym" = 1, "a50" = 0, "ato95" = 1)), 
                        dn = dnormal(FLQuant(c(0:20+.5),dimnames=list(age = 0:20)), FLPar("sel1" = 1, "sel2" = 0.9, "sel3" = 20))  ), 
             her = list(kn0 = 1, 
                        logistic = logistic(FLQuant(c(0:8+.5),dimnames=list(age = 0:8)), FLPar("asym" = 1, "a50" = 0, "ato95" = 2)), 
                        dn = dnormal(FLQuant(c(0:8+.5),dimnames=list(age = 0:8)),FLPar("sel1" = 2, "sel2" = 1.9, "sel3" = 10))  )  ),
  
  nm = list(mon = list(cons = 0.25,#same as a4a assessment 
                       gis = luke_gisla(mpar["linf"],mpar["k"],len = vonB(age = c(0:20+.5),mpar))),#should ages be +0.5????????
           her = list(cons = griffiths(hpar), 
                      gis = luke_gisla(hpar["linf"],hpar["k"],len = vonB(age = c(0:8)+.5,hpar)))  ),
  
  fc = c("c","ow","rc"),
  
  ts = list(short = 20,long = 40),
  
  sr = list(recsd0.1 = 0.1, recsd0.4 = 0.4),
  
  ar = list(nocor = 0, rho = 0.6)
  
)

#set up FLBRP object
# mon_eq=lhEql(mpar,range = c(min=0,max=20,minfbar=1,maxfbar=20,plusgroup=20))
# plot(mon_eq)
# m(mon_eq) #dodgy
# harvest(mon_eq) #dodgy
# 
# 
# 
# her_eq=lhEql(mpar,range = c(min=0,max=8,minfbar=2,maxfbar=8,plusgroup=8))
# plot(her_eq)
# m(her_eq) #dodgy
# harvest(mon_eq) #dodgy



