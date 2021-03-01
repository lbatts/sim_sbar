
#load database
load(url("https://github.com//fishnets//fishnets//blob//master//data//fishbase-web//fishbase-web.RData?raw=True"))

#Select Lophius piscatorius and Clupea harengus harengus as a case study
MON <- subset(fb,species=="Lophius piscatorius") #scrape database MON
HER <- subset(fb,species=="Clupea harengus harengus")
MON <- MON[,c("sex","linf","k","t0","a","b")] # 
HER <- HER[,c("sex","linf","k","t0","a","b")]  

#Get the means and create an FLPar object
MON <- apply(MON[,2:6],2,mean,na.rm=T)
HER <- apply(HER[,2:6],2,mean,na.rm=T)
MON["v"] <- HER["v"] <-5000

MON["a"]<-MON["a"]*1e-3  #a should be smaller!
HER["a"]<-HER["a"]*1e-3  #a should be smaller!

mpar <- FLPar(MON)
hpar <- FLPar(HER)

#fill in missing par as guestitmates from other life histroy parameters

mpar=lhPar(mpar)
hpar=lhPar(hpar)


#set up FLBRP object
eq=lhEql(par,range = c(min=0,max=15,minfbar=1,maxfbar=7,plusgroup=8))
