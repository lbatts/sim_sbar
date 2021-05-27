#===================================
# Investigating parameters estimation results of simulation testing
# Author : LB
#Distributed under GPL-3 or later
#Feb 2021
#===================================

#Best to restart r session if coming straight from sim runs as some package conflicts...
rm(list=ls())
#
detach(package:reshape) # might need this if not restarted R so reshape2 can be used properly
library(reshape2)
library(tidyr)
library(dplyr)

#setwd("C:/Users/LukeB/Documents/sim_sbar")
setwd("/home/luke/Documents/sim_sbar")
# quick check that runs that are not logistic or dome are the same

 # load("res_10iters_allsce.Rdata")
#  i<-tmpres[[1]]
#  load("res_10iters_allsce_ldcut.RData")
#  j<-tmpres[[1]]
# i$stk_c$mwts - j$stk_c$mwts
# i$stk_c$csa - j$stk_c$csa
rm(list=ls())
source("plot_funs.R")
 load("res_100iters_allsce_new_2.RData")
 #load("res_10iters_allsce.Rdata")
 #i<-tmpres[[1]]
ls()
head(sce)

bigdat_ls<-lapply(tmpres[1:dim(sce)[1]],function(i){

  print(i$sce_id)
  tt<-lapply(i[3:5],par_extract)
  dat<- dplyr::bind_rows(tt, .id = "HD")
  dat$sce_id <- i$sce_id
  #dat$scenario <- as.character(i$scenario)

  return(dat)
})

bigdat<-dplyr::bind_rows(bigdat_ls)


convergence_ls<-lapply(tmpres[1:dim(sce)[1]],function(i){
  
  print(i$sce_id)
  tt<-lapply(i[3:5],error_extract)
  dat<- dplyr::bind_rows(tt, .id = "HD")
  dat$sce_id <- i$sce_id
  #dat$scenario <- as.character(i$scenario)
  
  return(dat)
})

convergence<-dplyr::bind_rows(convergence_ls)

head(bigdat)
head(convergence)
dim(bigdat)
dim(convergence)


#====================================================
#currently we have the 48 scenarios from sce dataframe and the three Harvest Dynamics (HD) for each scenario. 48*3 = 144 scenarios. 144* 10 iters = 1440
head(sce)
data_table_1 = data.table::data.table(bigdat, key="sce_id")
sce$sce_id<-as.integer(row.names(sce))
data_table_2 = data.table::data.table(sce, key="sce_id")
data_table_3 = data.table::data.table(convergence, key="sce_id")

f_dat<-merge(data_table_1, data_table_2,all.x=T)
#f_dat<-merge(bigdat, sce, by.x  = "sce_id" , by.y =0,all.x=T)
c_dat<-merge(data_table_3, data_table_2,all.x=T)


head(f_dat)
unique(f_dat$aorif)
unique(f_dat$type)
unique(c_dat$type)
unique(f_dat$var)
 #subset(f_dat,f_dat$sce_id==5 & f_dat$type=="CSA")
# subset(bigdat,bigdat$sce_id==24 & bigdat$type=="CSA")

rm(list=c("bigdat","bigdat_ls","convergence","convergence_ls","tmpres","par_extract","par_extract_inner","res_extract","res_extract_inner","error_extract","error_extract_inner","data_table_1","data_table_2","data_table_3"))# remove so free up space
ls()
head(f_dat)
str(f_dat)
f_dat$HD<-factor(f_dat$HD)
f_dat$aorif<-factor(f_dat$aorif)
c_dat$HD<-factor(c_dat$HD)
c_dat$aorif<-factor(c_dat$aorif)


dim(f_dat)
#checks
length(levels(factor(f_dat$sce_id)))
length(unique(with(f_dat, interaction(sce_id, HD, drop=TRUE))))   # gives correct number of 144
length(unique(with(f_dat, interaction(sce_id, HD,iter, drop=TRUE))))   # gives correct number 144 * iters
length(unique(with(f_dat, interaction(sce_id, HD,iter,type, drop=TRUE))))   # gives correct number 144 * iters * 6 (5 assessments and 1 real values)
table(f_dat$type) #so counts are lower as no f values
unique(f_dat$type)

table(f_dat$HD)
dim(subset(f_dat,f_dat$sce_id==5 & f_dat$HD=="stk_c" & f_dat$iter==10 & f_dat$type == "rv"))

#with cuts
length(levels(factor(f_dat$aorif)))
length(unique(with(f_dat, interaction(sce_id, HD,aorif, drop=TRUE))))   # gives correct number of 240
length(unique(with(f_dat, interaction(sce_id, HD,iter,aorif, drop=TRUE))))   # gives correct number 240 * iters
length(unique(with(f_dat, interaction(sce_id, HD,iter,type,aorif, drop=TRUE))))   # gives correct number 2400 * 6 

#### get real values in their own columns
head(f_dat)
#tmp<-subset(f_dat,f_dat$sce_id==5 & f_dat$HD=="stk_c" & f_dat$iter==10)
# dim(tmp)
# table(tmp$type)
tmp1<-data.table::dcast.data.table(f_dat, sce_id+HD+aorif+iter+lh+ts+sel+ar+sr+var~ type, value.var="est")
tmp2<-data.table::dcast.data.table(f_dat, sce_id+HD+aorif+iter+lh+ts+sel+ar+sr+var~ type, value.var="se")
colnames(tmp1[,c(1:10,16)])

tmp1.2<-melt(tmp1,id.vars = c(1:10,16),variable.name = "assessment") # c(1:10,12)
tmp2.2<-melt(tmp2,id.vars = c(1:10,16),variable.name = "assessment")
 head(tmp2.2)

 all.equal(tmp1.2[,1:10],(tmp2.2)[,1:10])
f_dat<-cbind(tmp1.2,tmp2.2[,13])
colnames(f_dat)[13:14]<-c("est","se")
f_dat[1:20,]

tmp<-subset(f_dat,f_dat$sce_id==5 & f_dat$HD=="stk_c" & f_dat$iter==10)

###make n40 into nend
f_dat$n_id<-ifelse(f_dat$ts=="short" & f_dat$var=="N20","Nend",ifelse( f_dat$ts=="long" & f_dat$var=="N40" ,"Nend",     f_dat$var))

unique(f_dat$n_id[f_dat$ts=="short" & f_dat$var=="N20"])
unique(f_dat$n_id[f_dat$ts=="long" & f_dat$var=="N40"])

f_dat$var<-f_dat$n_id
dim(f_dat)

f_dat<-f_dat[,-15]
head(f_dat)


dim(tmp)
length(unique(tmp$rv))
table(tmp$assessment)
table(tmp$rv)

rm("tmp" ,   "tmp1",   "tmp1.2" ,"tmp2" , "tmp2.2")
####=====lets deal with non convergence etc. first==============
colnames(c_dat)[7]<-"assessment"
head(f_dat)
f_dat$uniqrun <- with(f_dat[,], interaction(sce_id, HD,iter,assessment,aorif, drop=TRUE))
c_dat$uniqrun <- with(c_dat[,], interaction(sce_id, HD,iter,assessment,aorif, drop=TRUE))
head(c_dat[,c(5,6,13)])
data_table_1 = data.table::data.table(f_dat, key="uniqrun")
data_table_2 = data.table::data.table(c_dat, key="uniqrun")

f_dat<-merge(data_table_1, data_table_2[,c(5,6,13)], all.x=T)
dim(f_dat)
head(f_dat)
#f_dat$Error[f_dat$type=="rv"]<-"rv"
rm("data_table_1" ,   "data_table_2")  
head(c_dat)
table(f_dat$Error)
sum(table(c_dat$Error))/5

table(f_dat$assessment)


table(c_dat$assessment[c_dat$Error=="try different starting pars"],c_dat$sel[c_dat$Error=="try different starting pars"],c_dat$sce_id[c_dat$Error=="try different starting pars"])


range(f_dat$est[f_dat$var == "qhat"],na.rm=T)

f_dat$filter<-ifelse(f_dat$convergence==1,"noncon",ifelse(f_dat$var == "qhat"& f_dat$est > 1e-7,"realistic_estimates","unrealistic_estimates"))

f_dat$filter[which(f_dat$uniqrun %in% f_dat$uniqrun[f_dat$filter == "realistic_estimates"])] <- "realistic_estimates"


#f_dat[700:715,]

#tmp<-droplevels(f_dat[f_dat$var=="qhat" & f_dat$type != "real values",c(15,16,1)])
rm("c_dat")

conver<-f_dat[f_dat$var=="qhat",]

subset(conver,conver$sce_id==24 & conver$assessment=="CSA")



length(unique(with(conver, interaction(sce_id, HD, iter,assessment,aorif, drop=TRUE))))
length(unique(with(conver, interaction(sce_id, HD,iter ,aorif,drop=TRUE))))
length(unique(with(conver, interaction(sce_id, HD,aorif, drop=TRUE))))

conver %>%
  group_by(assessment) %>%
  count()

tmp<-conver %>%
  group_by(Error,assessment) %>%
  count
sum(tmp$n)

table(conver$assessment)
head(conver)

save(conver, file="conver.RData")
table(conver$Error) # mostly "did not converge"
table(conver$filter) # mostly "did not converge"

dim(conver)
head(conver)
table(conver$var)
table(conver$assessment)

# tmp<-table(conver[,c(3,9,12,17)])
# tmp


tmp<-conver %>% 
  count(assessment, sel,filter) %>% 
  group_by(sel,assessment) %>% 
  transmute(filter, Percentage=n/sum(n)*100) %>%
  pivot_wider(names_from = sel,values_from = Percentage)

tmp2<-conver %>% 
  count(assessment, HD,filter) %>% 
  group_by(HD,assessment) %>% 
  transmute(filter, Percentage=n/sum(n)*100) %>%
  pivot_wider(names_from = HD,values_from = Percentage)#%>%
#filter(filter=="realistic estimates")
table(conver$filter)
table(conver$filter,conver$assessment,conver$sel,conver$ts)

range(conver$est[conver$filter=="unrealistic_estimates"& conver$assessment=="SOPEM-cw"  ])

1e-6/4.1e-08



library(xtable)
print(xtable(tmp[,1:5]),include.rownames=FALSE)
 

tmp<-conver %>% 
  count(assessment, sel,filter) %>% 
  group_by(sel,assessment) %>% 
  transmute(filter, Percentage=n/sum(n)*100) %>%
  pivot_wider(names_from = sel,values_from = Percentage)%>%
  arrange(assessment,filter)
tmp

tmp2<-conver %>% 
  count(assessment, HD,filter) %>% 
  group_by(HD,assessment) %>% 
  transmute(filter, Percentage=n/sum(n)*100) %>%
  pivot_wider(names_from = HD,values_from = Percentage)%>%
  arrange(assessment,filter)
tmp[,1:2]==tmp2[,1:2]
tmp<-bind_cols(tmp,tmp2[,3:5])


tmp2<-conver %>% 
  count(assessment, lh,filter) %>% 
  group_by(lh,assessment) %>% 
  transmute(filter, Percentage=n/sum(n)*100) %>%
  pivot_wider(names_from = lh,values_from = Percentage)%>%
  arrange(assessment,filter)
tmp[,1:2]==tmp2[,1:2]
tmp<-bind_cols(tmp,tmp2[,3:4])


tmp2<-conver %>% 
  count(assessment, ts,filter) %>% 
  group_by(ts,assessment) %>% 
  transmute(filter, Percentage=n/sum(n)*100) %>%
  pivot_wider(names_from = ts,values_from = Percentage)%>%
  arrange(assessment,filter)
tmp[,1:2]==tmp2[,1:2]
tmp<-bind_cols(tmp,tmp2[,3:4])


tmp2<-conver %>% 
  count(assessment, sr,filter) %>% 
  group_by(sr,assessment) %>% 
  transmute(filter, Percentage=n/sum(n)*100) %>%
  pivot_wider(names_from = sr,values_from = Percentage)%>%
  arrange(assessment,filter)
tmp[,1:2]==tmp2[,1:2]
tmp<-bind_cols(tmp,tmp2[,3:4])


tmp2<-conver %>% 
  count(assessment, ar,filter) %>% 
  group_by(ar,assessment) %>% 
  transmute(filter, Percentage=n/sum(n)*100) %>%
  pivot_wider(names_from = ar,values_from = Percentage)%>%
  arrange(assessment,filter)
tmp[,1:2]==tmp2[,1:2]
tmp<-bind_cols(tmp,tmp2[,3:4])

tmp
print(xtable(tmp[,c(-3:-5)]),include.rownames=FALSE)
#massive issues with selelctivity that is logistic and dome

#====================================================================================
#Relative error tables
dim(f_dat)
head(f_dat)

###use this as a a  nice summary at start of results section
table(f_dat$filter)/sum(table(f_dat$filter))


#filter f dat!!!!!!!!!

#f_dat[f_dat$type=="real values",]
head(f_dat)

f_dat$rel_err <- (f_dat$est - f_dat$rv)/f_dat$rv
f_dat$abrel_err <- abs((f_dat$est - f_dat$rv)/f_dat$rv)
f_dat$rel_se <- f_dat$se/f_dat$est

range(f_dat$abrel_err[f_dat$filter=="realistic_estimates"& f_dat$assessment=="SOPEM-cw" & f_dat$var=="Nend" & f_dat$lh=="her" & f_dat$sel=="kn0"& f_dat$HD=="stk_c" ])

tmp_un<-f_dat[f_dat$filter=="realistic_estimates"& f_dat$assessment=="SOPEM-cw" & f_dat$var=="Nend" & f_dat$lh=="her" & f_dat$sel=="kn0"& f_dat$HD=="stk_c", ]

abs((224470700-14822940)/14822940)   

abs((0.0001-0.1)/0.1)   

fpars<-c(unique(f_dat$var)[c(2:4)])
npars<-unique(f_dat$var)[grep("N",unique(f_dat$var))]

table(f_dat$filter)
# 
# tmp<-f_dat %>%
#   filter(filter == "realistic_estimates" & sel == "kn0") %>%
#   filter(var %in% fpars) %>%
#   group_by(sel,assessment,var) %>%
#   summarise(mean_re = mean(rel_err,na.rm=T),median_re = median(rel_err,na.rm=T),mean_rse = mean(rel_se,na.rm=T),median_rse = median(rel_se,na.rm=T))
#   
# tmp
# library(xtable)
# print(xtable(tmp),include.rownames=FALSE)


tmp_un<-subset(f_dat,f_dat$filter== "realistic_estimates"  & f_dat$assessment == "CSA"&  f_dat$uniqrun == "38.stk_c.1.CSA.nocut")
table(tmp_un$lh[tmp_un$est>1e-09])
table(tmp_un$sr[tmp_un$est>1e-09])
table(tmp_un$ar[tmp_un$est>1e-09])
table(tmp_un$ts[tmp_un$est>1e-09])
table(tmp_un$sel[tmp_un$est>1e-09])

table(tmp_un$sce_id[tmp_un$est>1e-09])


range(tmp_un$abrel_err)
range(tmp_un$est)
d <- density(log(tmp_un$est)) # returns the density data
plot(d)
abline(v=log(1e-16),lty="dotted",col=2)

(1e-15-1e-6)/1e-6

###qhat table
tmp<-f_dat %>%
  filter(filter %in% c("realistic_estimates","unrealistic_estimates") & var == "qhat") %>% #filter == "realistic_estimates" &
   group_by(sel,lh,assessment, HD) %>%
  summarise(MRE = mean(rel_err,na.rm=T),MARE = median(abrel_err,na.rm=T))

  field <- f_dat %>% distinct(sel) %>% pull()
  field2 <- f_dat %>% distinct(HD) %>% pull()
  sub_field <- colnames(tmp)[5:6]

pivot_names1 <- purrr::map(field[c(1,2,3)],~paste(., field2, sep = "_")) %>% unlist()
pivot_names2 <- purrr::map(pivot_names1, ~paste(., sub_field, sep = "_")) %>% unlist()
pivot_vals <- rep(sub_field, length(pivot_names2)/2)
pivot_vars1 <- purrr::map(field[c(1,2,3)], rep, 6) %>% unlist()
pivot_vars2 <- rep(purrr::map(field2, rep, 2) %>% unlist(),3)

spec <- tibble(.name = pivot_names2, .value = pivot_vals, sel=pivot_vars1, HD=pivot_vars2 )

tmp<-tmp %>% pivot_wider_spec(spec)

tmp2<-f_dat %>%
  filter(filter == "realistic_estimates" & var == "qhat") %>%
  group_by(sel,lh,assessment, HD) %>%
  summarise(MRE = mean(rel_err,na.rm=T),MARE = median(abrel_err,na.rm=T)) %>%
  pivot_wider(
    names_from = c(sel,HD),
    names_sep = "_",
    values_from = c(MRE,MARE)
  )

colnames(tmp)
colnames(tmp2)

tmp$kn0_stk_rc_MRE==tmp2$MRE_kn0_stk_rc
tmp$logistic_stk_c_MARE==tmp2$MARE_logistic_stk_c

tmp
print(xtable(tmp),digits=c(1,1,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3),include.rownames=FALSE)

######################
#Fend and N end MARE table


tmp<-f_dat %>%
  filter(filter %in% c("realistic_estimates") & var %in% c("Fend","Nend")) %>% #filter == "realistic_estimates" & 
  group_by(sel,lh,assessment, HD,var) %>%
  summarise(MARE = median(abrel_err,na.rm=T)) 

field <- f_dat %>% distinct(sel) %>% pull()
field2 <- f_dat %>% distinct(HD) %>% pull()
field3 <- c("Fend","Nend")
sub_field <- colnames(tmp)[6]

pivot_names1 <- purrr::map(field[c(1,2,3)],~paste(., field2, sep = "_")) %>% unlist()
pivot_names2 <- purrr::map(pivot_names1, ~paste(., field3, sep = "_")) %>% unlist()
pivot_names3 <- purrr::map(pivot_names2, ~paste(., sub_field, sep = "_")) %>% unlist()

pivot_vals <- rep(sub_field, length(pivot_names3))
pivot_vars1 <- purrr::map(field[c(1,2,3)], rep, 6) %>% unlist()
pivot_vars2 <- rep(purrr::map(field2, rep, 2) %>% unlist(),3)
pivot_vars3 <- rep(purrr::map(field3, rep, by = 1) %>% unlist(),9)

spec <- tibble(.name = pivot_names3, .value = pivot_vals, sel=pivot_vars1, HD=pivot_vars2 , var=pivot_vars3 )

tmp<-tmp %>% pivot_wider_spec(spec)

tmp2<-f_dat %>%
  filter(filter %in% c("realistic_estimates","unrealistic_estimates") & var %in% c("Fend","Nend")) %>%
  group_by(sel,lh,assessment, HD,var) %>%
  summarise(MARE = median(abrel_err,na.rm=T)) %>%
  pivot_wider(
    names_from = c(sel,HD,var),
    names_sep = "_",
    values_from = c(MARE)
  )

colnames(tmp)  
colnames(tmp2)  

tmp$kn0_stk_rc_Fend_MARE==tmp2$kn0_stk_rc_Fend
tmp$logistic_stk_c_Nend_MARE==tmp2$logistic_stk_c_Nend
dim(tmp)
tmp[,10:20]
print(xtable(tmp,digits=c(1,1,1,rep(2,18)),include.rownames=FALSE))



# 
# 
# 
# f_dat[f_dat$filter=="realistic_estimates"&f_dat$lh=="mon"&f_dat$sel=="kn0"&f_dat$assessment=="sb0nbw"& f_dat$var =="qhat",]
# 
# tmp<-f_dat[f_dat$filter=="realistic_estimates" & f_dat$var =="qhat",]
# 
# tt<-with(tmp, tapply(rel_err, list(assessment, var,sel,lh,HD), median,na.rm=T))
# dim(tt)
# print(xtable(tt[,,1],digits=c(1,3,3,3),include.rownames=FALSE))


plot_dat<-f_dat %>%
  filter(filter %in% c("realistic_estimates") )# sce_id %in% c(c(3,4,7,8,11,12))
#save(plot_dat,file="limsce_runs.RData")

library(ggplot2)
library(ggh4x)
library(RColorBrewer)
library(scales)

# rects <- data.frame(xstart = c("csa",    "sb0"  ,  "sb0ubw", "so","soubw"), xend = c("csa","sb0" ,   "sb0ubw", "so"  ,   "soub"), col = myColors)
fa<-unique(plot_dat$assessment)[1:3]


plot_dat <- plot_dat %>%
  mutate(assessment = forcats::fct_relevel(assessment, 
                                    "CSA","ASOEM-cw","ASOEM-ubw","SOPEM-cw","SOPEM-ubw"))

levels(plot_dat$assessment) <- c("CSA",expression("S1"["c"]),expression("S1"["u"]),expression("S0"["c"]),expression("S0"["u"]))


myColors <- brewer.pal(5,"Set1")
names(myColors) <- levels(plot_dat$assessment)
colScale <- scale_colour_manual(name = "Assessment",values = myColors,labels = parse_format())
colScale_fill <- scale_fill_manual(name = "Assessment",values = myColors,labels = parse_format())

myColors2 <- rev(brewer.pal(5,"Greys"))
names(myColors2) <- levels(plot_dat$assessment)
myColors2[3]<-"#000000"
colScale2 <- scale_colour_manual(name = "Assessment",values = myColors2, labels = parse_format())

fa<-levels(plot_dat$assessment)[1:3]

colnames(plot_dat)[6:10]<-c("LH","TS","SEL","AR","SR")
colnames(plot_dat)[13]<-"Assessment"
levels(plot_dat$LH)<- c("large demersal","small pelagic") 
levels(plot_dat$HD)<- c("c","ow","rc") 

####=====================================
#stock numbers
dev.off()
#install.packages("ggrepel")
# library(ggrepel)
# ####plots
# for(selh in levels(plot_dat$SEL)){
#   #for(tsh in levels(plot_dat$ts)){
#   for(varh in  c("re","se")){
#     # selh<-"kn0"
#     # tsh<-"long"
#     # varh<-"re"
#     #
#     tsh<-"both"
#     tmpdat <- plot_dat %>%
#       filter(var %in% npars,SEL == selh)
#     
#     
#     
#     dataMedian <- tmpdat%>%
#       filter(LH=="her")%>%
#       group_by(SR,AR,HD,Assessment)%>%
#       summarise(MD = round(median(rel_err),2))
#     
#     if(varh=="re"){
#       
#       
#       
#       dataMedian <- tmpdat%>%
#         filter(LH=="her")%>%
#         group_by(SR,AR,HD,Assessment)%>%
#         summarise(MD = round(median(rel_err),2))
#       
#       
#       pdf(paste0("/home/luke/Documents/latex_p2/4sims/her",tsh,selh,varh,"N.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
#       
#       p<-ggplot(tmpdat[tmpdat$LH=="her",],aes(x=Assessment,y=rel_err,fill=Assessment))
#       print(
#         p+geom_violin(trim=T,position=position_dodge(1),scale="width",width=.8)+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(aes(col=Assessment),fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+geom_text_repel(data = dataMedian, aes(x=Assessment, y=MD, label = MD), size = 3,nudge_x=0.5,nudge_y=2)+ facet_nested(SR+AR~HD, labeller=label_both) +ylab("Relative error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.title = element_blank(), legend.text = element_text(color = "white"),legend.key = element_rect(colour = "transparent", fill ="transparent"))+guides(fill = guide_legend(override.aes = list(color ="transparent", fill ="transparent") ) )
#       )
#       #position = position_dodge(width = 0.8)
#       
#       
#       
#       
#       # )
#       dev.off()
#       
#       dataMedian <- tmpdat%>%
#         filter(LH=="mon")%>%
#         group_by(SR,AR,HD,Assessment)%>%
#         summarise(MD = round(median(rel_err),2))
#       
#       pdf(paste0("/home/luke/Documents/latex_p2/4sims/mon",tsh,selh,varh,"N.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
#       
#       #
#       p<-ggplot(tmpdat[tmpdat$LH=="mon",],aes(x=Assessment,y=rel_err,fill=Assessment)) + facet_nested(SR+AR~HD, labeller=label_both) #
#       print(
#         p+geom_violin(trim=T,position=position_dodge(1),scale="width",width=.8)+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(aes(col=Assessment),fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+geom_text_repel(data = dataMedian, aes(x=Assessment, y=MD, label = MD), size = 3,nudge_x=0.5,nudge_y=2)+ylab("Relative error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1))
#       )
#       
#       
#       dev.off()
#       
#       
#     }else{
#       
#       #dev.off()
#       
#       pdf(paste0("/home/luke/Documents/latex_p2/4sims/her",tsh,selh,varh,"N.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
#       
#       p<-ggplot(tmpdat[tmpdat$LH=="her",],aes(x=Assessment,y=rel_se,fill=Assessment)) + facet_nested(SR+AR~HD, labeller=label_both) #
#       print(
#         p+geom_violin(position=position_dodge(1),scale="width",width=.8)+ stat_summary(aes(col=Assessment),fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative standard error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.title = element_blank(), legend.text = element_text(color = "white"),legend.key = element_rect(colour = "transparent", fill ="transparent"))+guides(fill = guide_legend(override.aes = list(color ="transparent", fill ="transparent") ) )
#         
#       )
#       
#       dev.off()
#       pdf(paste0("/home/luke/Documents/latex_p2/4sims/mon",tsh,selh,varh,"N.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
#       
#       p<-ggplot(tmpdat[tmpdat$LH=="mon",],aes(x=Assessment,y=rel_se,fill=Assessment)) + facet_nested(SR+AR~HD, labeller=label_both) #
#       print(
#         p+geom_violin(position=position_dodge(1),scale="width",width=.8)+ stat_summary(aes(col=Assessment),fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative standard error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.title = element_blank(), legend.text = element_text(color = "white"),legend.key = element_rect(colour = "transparent", fill ="transparent"))+guides(fill = guide_legend(override.aes = list(color ="transparent", fill ="transparent") ) )
#         
#       )
#       
#       
#       dev.off()
#     }
#     
#   }
# }

# ####=====================================
# #F
# dev.off()
# fp<-fpars[1:2]
# #####f plots
# for(selh in levels(plot_dat$SEL)){
#   #for(tsh in levels(plot_dat$ts)){
#   for(varh in  c("re","se")){
#     # selh<-"kn0"
#     # tsh<-"long"
#     # varh<-"re"
#     #
#     tsh<-"both"
#     tmpdat <- plot_dat %>%
#       filter(var %in% fp,SEL == selh,Assessment %in% fa)%>%
#       mutate(var = forcats::fct_relevel(var,
#                                         "F1", "Fend"))
#
#     if(varh=="re"){
#
#
#
#       #
#
#       dataMedian <- tmpdat%>%
#         filter(LH=="her")%>%
#         group_by(SR,AR,HD,var,Assessment)%>%
#         summarise(MD = round(median(rel_err),2))
#
#       pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/her",tsh,selh,varh,"F.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
#
#       p<-ggplot(tmpdat[tmpdat$LH=="her",],aes(x=Assessment,y=rel_err,fill=Assessment)) + facet_nested(SR+AR~HD+var, labeller=label_both) #
#       print(
#         p+geom_violin(trim=T,position=position_dodge(1),scale="width",width=.8)+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(aes(col=Assessment),fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+geom_text_repel(data = dataMedian, aes(x=Assessment, y=MD, label = MD), size = 3,nudge_x=0.5,nudge_y=2,force=2.5)+ylab("Relative error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.title = element_blank(), legend.text = element_text(color = "white"),legend.key = element_rect(colour = "transparent", fill ="transparent"))+guides(fill = guide_legend(override.aes = list(color ="transparent", fill ="transparent") ) )
#       )
#
#
#
#
#
#       # )
#       dev.off()
#
#       dataMedian <- tmpdat%>%
#         filter(LH=="mon")%>%
#         group_by(SR,AR,HD,var,Assessment)%>%
#         summarise(MD = round(median(rel_err),2))
#
#       pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/mon",tsh,selh,varh,"F.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
#
#       #
#       p<-ggplot(tmpdat[tmpdat$LH=="mon",],aes(x=Assessment,y=rel_err,fill=Assessment)) + facet_nested(SR+AR~HD+var, labeller=label_both) #
#       print(
#         p+geom_violin(trim=T,position=position_dodge(1),scale="width",width=.8)+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(aes(col=Assessment),fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+geom_text_repel(data = dataMedian, aes(x=Assessment, y=MD, label = MD), size = 3,nudge_x=0.5,nudge_y=2,force=2.5)+ylab("Relative error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1))
#       )
#
#
#       dev.off()
#
#
#     }else{
#
#       #dev.off()
#
#       pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/her",tsh,selh,varh,"F.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
#
#       p<-ggplot(tmpdat[tmpdat$LH=="her",],aes(x=Assessment,y=rel_se,fill=Assessment)) + facet_nested(SR+AR~HD+var, labeller=label_both) #
#       print(
#         p+geom_violin(position=position_dodge(1),scale="width",width=.8)+ stat_summary(aes(col=Assessment),fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative standard error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.title = element_blank(), legend.text = element_text(color = "white"),legend.key = element_rect(colour = "transparent", fill ="transparent"))+guides(fill = guide_legend(override.aes = list(color ="transparent", fill ="transparent") ) )
#
#       )
#
#       dev.off()
#       pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/mon",tsh,selh,varh,"F.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
#
#       p<-ggplot(tmpdat[tmpdat$LH=="mon",],aes(x=Assessment,y=rel_se,fill=Assessment)) + facet_nested(SR+AR~HD+var, labeller=label_both) #
#       print(
#         p+geom_violin(position=position_dodge(1),scale="width",width=.8)+ stat_summary(aes(col=Assessment),fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative standard error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1))+theme(legend.title = element_blank(), legend.text = element_text(color = "white"),legend.key = element_rect(colour = "transparent", fill ="transparent"))+guides(fill = guide_legend(override.aes = list(color ="transparent", fill ="transparent") ) )
#
#       )
#
#
#       dev.off()
#     }
#
#   }
# }


##===============================
#here starts code for ms plots


width_measured <- 8.5
height_measured <- 8.5

####=====================================
#stock numbers
dev.off()
##### plots
for(selh in levels(plot_dat$SEL)){
  #for(tsh in levels(plot_dat$ts)){
  # selh<-"kn0"
  #selh<-"logistic"
  # tsh<-"long"
  # varh<-"re"
  # 
  tsh<-"both"
  if(selh=="dome"){
    tmpdat <- plot_dat %>%
      filter(var %in% npars,SEL == selh,Assessment!="\"S1\"[\"c\"]")
    
  }else{
  tmpdat <- plot_dat %>%
    filter(var %in% npars,SEL == selh)
  }
  
  p1<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[2],],aes(x=Assessment,y=rel_err,fill=Assessment)) 
  
  p1<-p1+geom_violin(trim=T,position=position_dodge(1),scale="width",width=.8)+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(col=1,fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ facet_nested(SR+AR~LH+HD, labeller=label_both) +ylab("Relative error") + theme_classic()+ colScale_fill+ colScale+scale_x_discrete(labels = parse_format())+coord_cartesian(ylim=c(-1,1))+theme(axis.text.x = element_text(angle = 45, hjust = 1,colour = "black"))
  
  #+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  #
  p2<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[1],],aes(x=Assessment,y=rel_err,fill=Assessment)) 
  
  p2<-p2+geom_violin(trim=T,position=position_dodge(1),scale="width",width=.8)+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(col=1,fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ facet_nested(SR+AR~LH+HD, labeller=label_both) +ylab("Relative error") + theme_classic()+ colScale_fill+ colScale+scale_x_discrete(labels = parse_format())+coord_cartesian(ylim=c(-1,1))+theme(axis.text.x = element_text(angle = 45, hjust = 1,colour = "black"))
  
  
  #dev.off()
  
  p3<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[2],],aes(x=Assessment,y=rel_se,fill=Assessment)) + facet_nested(SR+AR~LH+HD, labeller=label_both) #
  
  p3<- p3+geom_violin(position=position_dodge(1),scale="width",width=.8)+ stat_summary(col=1,fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative standard error") + theme_classic()+ colScale_fill+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1,colour = "black"))+scale_x_discrete(labels = parse_format())+coord_cartesian(ylim=c(0,1))
  
  
  
  p4<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[1],],aes(x=Assessment,y=rel_se,fill=Assessment)) + facet_nested(SR+AR~LH+HD, labeller=label_both) #
  
  p4<- p4+geom_violin(position=position_dodge(1),scale="width",width=.8)+ stat_summary(col=1,fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative standard error") + theme_classic()+ colScale_fill+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1,colour = "black"))+scale_x_discrete(labels = parse_format())+coord_cartesian(ylim=c(0,1))
  
  
  pp<-ggpubr::ggarrange(p2,p1,p4,p3, ncol=2, nrow=2, common.legend = TRUE, legend="bottom",labels = c("A", "B","C","D"))
  pp
  flpath<-paste0("/home/luke/Documents/latex_p2/4sims/",tsh,selh,"N.pdf")
  dev.off()
  pdf(file= flpath,width=width_measured, height=height_measured,pointsize=12)
  print(pp)
  dev.off()
  
  
  
}




####=====================================
#F
dev.off()
fp<-fpars[1:2]
#####f plots
for(selh in levels(plot_dat$SEL)){
  #for(tsh in levels(plot_dat$ts)){
  # selh<-"kn0"
  # tsh<-"long"
  # varh<-"re"
  # 
  tsh<-"both" 
  if(selh=="dome"){
    tmpdat <- plot_dat %>%
      filter(var %in% fp,SEL == selh,Assessment %in% fa[-2])%>%
      mutate(var = forcats::fct_relevel(var, 
                                        "F1", "Fend"))
    
  }else{
  tmpdat <- plot_dat %>%
    filter(var %in% fp,SEL == selh,Assessment %in% fa)%>%
    mutate(var = forcats::fct_relevel(var, 
                                      "F1", "Fend"))
  }
  
  
  p1<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[2],],aes(x=Assessment,y=rel_err,fill=Assessment)) 
  
  p1<-p1+geom_violin(trim=T,position=position_dodge(1),scale="width",width=.8)+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(col=1,fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ facet_nested(SR+AR~LH+HD+var, labeller=label_both) +ylab("Relative error") + theme_classic()+ colScale_fill+ colScale+scale_x_discrete(labels = parse_format())+coord_cartesian(ylim=c(-1,1.5))+theme(axis.text.x = element_text(angle = 45, hjust = 1,colour = "black"))
  
  #+theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  
  #
  p2<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[1],],aes(x=Assessment,y=rel_err,fill=Assessment)) 
  
  p2<-p2+geom_violin(trim=T,position=position_dodge(1),scale="width",width=.8)+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(col=1,fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ facet_nested(SR+AR~LH+HD+var, labeller=label_both) +ylab("Relative error") + theme_classic()+ colScale_fill+ colScale+scale_x_discrete(labels = parse_format())+coord_cartesian(ylim=c(-1,1.5))+theme(axis.text.x = element_text(angle = 45, hjust = 1,colour = "black"))
  
  
  #dev.off()
  
  p3<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[2],],aes(x=Assessment,y=rel_se,fill=Assessment)) + facet_nested(SR+AR~LH+HD+var, labeller=label_both) #
  
  p3<- p3+geom_violin(position=position_dodge(1),scale="width",width=.8)+ stat_summary(col=1,fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative standard error") + theme_classic()+ colScale_fill+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1,colour = "black"))+scale_x_discrete(labels = parse_format())+coord_cartesian(ylim=c(0,.7))
  
  
  
  p4<-ggplot(tmpdat[tmpdat$LH==levels(plot_dat$LH)[1],],aes(x=Assessment,y=rel_se,fill=Assessment)) + facet_nested(SR+AR~LH+HD+var, labeller=label_both) #
  
  p4<- p4+geom_violin(position=position_dodge(1),scale="width",width=.8)+ stat_summary(col=1,fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative standard error") + theme_classic()+ colScale_fill+ colScale+theme(axis.text.x = element_text(angle = 45, hjust = 1,colour = "black"))+scale_x_discrete(labels = parse_format())+coord_cartesian(ylim=c(0,.7))
  
  
  pp<-ggpubr::ggarrange(p2,p1,p4,p3, ncol=2, nrow=2, common.legend = TRUE, legend="bottom",labels = c("A", "B","C","D"))
  pp
  flpath<-paste0("/home/luke/Documents/latex_p2/4sims/",tsh,selh,"F.pdf")
  dev.off()
  pdf(file= flpath, width=(width_measured+1.5), height=(height_measured+1.5),pointsize=12)
  print(pp)
  dev.off()

}

#=============================================
#Here endeth code for MS plots
#==================================================================





















































######======not used

dev.off()
#####f plots
for(selh in levels(plot_dat$sel)){
  
    for(varh in  c("re","se")){
      # selh<-"kn0"
      # tsh<-"long"
      # varh<-"fpars"
      # 
      tmpdat <- plot_dat %>%
        filter(var %in% fpars,sel == selh,ts ==tsh, Assessment %in% fa)%>%
        mutate(var = forcats::fct_relevel(var, 
                                          "F1", "Fmid", "Fend"))
      
      if(varh=="re"){
        
        
        
        # 
        
        pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/her",tsh,selh,varh,"F.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
        
        p<-ggplot(tmpdat[tmpdat$lh=="her",],aes(x=assessment,y=rel_err,fill=var)) + facet_nested(sr+ar~HD, labeller=label_both) #
        print(
          p+geom_violin(trim=T,position=position_dodge(1))+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative error")+ scale_fill_brewer(palette="Greys") + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        )
        
        
        
        
        
        # )
        dev.off()
        
        pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/mon",tsh,selh,varh,"F.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
        
        #
        p<-ggplot(tmpdat[tmpdat$lh=="mon",],aes(x=assessment,y=rel_err,fill=var)) + facet_nested(sr+ar~HD, labeller=label_both) #
        print(
          p+geom_violin(trim=T,position=position_dodge(1))+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative error")+ scale_fill_brewer(palette="Greys") + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        )
        dev.off()
        
        
      }else{
        
        #dev.off()
        
        pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/her",tsh,selh,varh,"F.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
        
        p<-ggplot(tmpdat[tmpdat$lh=="her",],aes(x=assessment,y=rel_se,fill=var)) + facet_nested(sr+ar~HD, labeller=label_both) #
        print(
          p+geom_boxplot(position=position_dodge(1)) +ylab("Relative standard error")+ scale_fill_brewer(palette="Greys") + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        )
        
        dev.off()
        pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/mon",tsh,selh,varh,"F.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
        
        p<-ggplot(tmpdat[tmpdat$lh=="mon",],aes(x=assessment,y=rel_se,fill=var)) + facet_nested(sr+ar~HD, labeller=label_both) #
        print(
          p+geom_boxplot(position=position_dodge(1)) +ylab("Relative standard error")+ scale_fill_brewer(palette="Greys") + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        )
        
        
        dev.off()
      }
    }
  }









####=====================================
#stock numbers with grid arrange
dev.off()
#####f plots
for(selh in levels(plot_dat$sel)){
    for(varh in  c("re","se")){
      # selh<-"kn0"
      # 
      # varh<-"re"
      # 
      tmpdat <- plot_dat %>%
        filter(var %in% npars,sel == selh)
      
      if(varh=="re"){
        
        
        
  
        
        p<-ggplot(tmpdat[tmpdat$lh=="her",],aes(x=assessment,y=rel_err,fill=assessment)) + facet_nested(sr+ar~HD, labeller=label_both) #
        
        p1 <- p+geom_violin(trim=T,position=position_dodge(1),scale="width")+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(fun=median, geom="point", shape=23,position=position_dodge(1), size=1)+ylab("Relative error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        
     
        p<-ggplot(tmpdat[tmpdat$lh=="mon",],aes(x=assessment,y=rel_err,fill=assessment)) + facet_nested(sr+ar~HD, labeller=label_both) #
      
        p2 <- p+geom_violin(trim=T,position=position_dodge(1),scale="width")+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(fun=median, geom="point", shape=23,position=position_dodge(1), size=1)+ylab("Relative error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
     
        
     
        
      }else{
     
        p<-ggplot(tmpdat[tmpdat$lh=="her",],aes(x=assessment,y=rel_se,fill=assessment)) + facet_nested(sr+ar~HD, labeller=label_both) #
       
        p3 <-  p+geom_violin(position=position_dodge(1),scale="width") +ylab("Relative standard error")+ scale_fill_brewer(palette="Greys") + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
       
     
        
        p<-ggplot(tmpdat[tmpdat$lh=="mon",],aes(x=assessment,y=rel_se,fill=assessment)) + facet_nested(sr+ar~HD, labeller=label_both) #
       
         p4 <- p+geom_violin(position=position_dodge(1),scale="width") +ylab("Relative standard error")+ scale_fill_brewer(palette="Greys") + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        
        
      
      }
    }
  
  
  pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/blh",selh,"F.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
  
  
  pp<-ggarrange(p2, p1,p4,p3, ncol=2, nrow=2, common.legend = TRUE, legend="right", align="hv",labels=c("a","b","c","d"))
  print(pp)
  
  dev.off()
}










































































#####################
#condensed plots for ms

dev.off()
#####N plots
      # selh<-"kn0"
      # tsh<-"long"
      # varh<-"re"
      # 
      tmpdat <- plot_dat %>%
        filter(var %in% npars)
     
       # tmp_un<-subset(tmpdat,tmpdat$assessment == "CSA")
       # table(tmpdat$lh[tmpdat$rel_err<=2])
       # 
       # table(tmp_un$lh[tmp_un$rel_err>2])
       # 
      
      
      varh="re"
       
        # 
        # pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/her",varh,"Ncondense.pdf"), onefile = FALSE, paper = "special",width=12, height=12,pointsize=12)
        # 
        p<-ggplot(tmpdat[tmpdat$lh=="her",],aes(x=assessment,y=rel_err,fill=assessment)) + facet_nested(sel~HD, labeller=label_both)#+ylim(-1,1)#,fill=assessment
        #print(
          p1<-p+geom_violin(outlier.alpha=0.1,scale="width")+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(fun=median, geom="point", shape=23,position=position_dodge(1), size=1)+ylab("Relative error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        #)
        
        
        
        
        
        # )
        dev.off()
        # 
        # pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/mon",varh,"Ncondense.pdf"), onefile = FALSE, paper = "special",width=12 ,height=12,pointsize=12)
        # 
        #
        p<-ggplot(tmpdat[tmpdat$lh=="mon",],aes(x=assessment,y=rel_err,fill=assessment)) + facet_nested(sel~HD, labeller=label_both)#+ylim(-1,1) #,fill=assessment
        #print(
          p2<-p+geom_violin(outlier.alpha=0.1,scale="width")+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(fun=median, geom="point", shape=23,position=position_dodge(1), size=1)+ylab("Relative error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        #)
        
        
        dev.off()
        
        
      #}else{
        
        #dev.off()
        varh="se"
        # pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/her",varh,"Ncondense.pdf"), onefile = FALSE, paper = "special",width=12 ,height=12,pointsize=12)
        
        p<-ggplot(tmpdat[tmpdat$lh=="her",],aes(x=assessment,y=rel_se,fill=assessment)) + facet_nested(sel~HD, labeller=label_both) #,fill=assessment
        #print(
         p3<- p+geom_violin(position=position_dodge(1),scale="width") +ylab("Relative standard error")+ scale_fill_brewer(palette="Greys") + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        #)
        
        # dev.off()
        # pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/mon",varh,"Ncondense.pdf"), onefile = FALSE, paper = "special",width=12, height=12,pointsize=12)
        # 
        p<-ggplot(tmpdat[tmpdat$lh=="mon",],aes(x=assessment,y=rel_se,fill=assessment)) + facet_nested(sel~HD, labeller=label_both) #,fill=assessment
        #print(
         p4<- p+geom_violin(position=position_dodge(1),scale="width") +ylab("Relative standard error")+ scale_fill_brewer(palette="Greys") + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        #)
        
        
        dev.off()
        
        pp<-ggarrange(p2, p1,p4,p3, ncol=2, nrow=2, common.legend = TRUE, legend="right", align="hv",labels=c("a","b","c","d"))
        pp
      #   
      # gridExtra::grid.arrange( gridExtra::arrangeGrob(p1, p2, top="A"), gridExtra::arrangeGrob(p3, p4, top="B"), ncol=2)
      # 
 # f condensed plots#==========================================================================
        fpars2<-fpars[c(2)]
        dev.off()
        #####N plots
        # selh<-"kn0"
        # tsh<-"long"
        # varh<-"re"
        # 
        tmpdat <- plot_dat %>%
          filter(var %in% fpars2)
        
        varh="re"
        
        
        pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/her",varh,"Fcondense.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
        
        p<-ggplot(tmpdat[tmpdat$lh=="her",],aes(x=assessment,y=rel_err,fill=assessment)) + facet_nested(sel~HD+var, labeller=label_both) #
        #print(
        p+geom_violin(trim=TRUE,position=position_dodge(1))+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        #)
        
        
        
        
        
        # )
        dev.off()
        
        pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/mon",varh,"Fcondense.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
        
        #
        p<-ggplot(tmpdat[tmpdat$lh=="mon",],aes(x=assessment,y=rel_err,fill=assessment)) + facet_nested(sel~HD+var, labeller=label_both) #
        print(
          p+geom_violin(trim=TRUE,position=position_dodge(1))+geom_abline(slope=0,intercept=0,linetype="dashed")+ stat_summary(fun=median, geom="point", shape=23,position=position_dodge(1), size=2)+ylab("Relative error") + theme_classic()+ scale_fill_brewer(palette="Greys")+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        )
        
        
        dev.off()
        
        
        #}else{
        
        #dev.off()
        varh="se"
        pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/her",varh,"Fcondense.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
        
        p<-ggplot(tmpdat[tmpdat$lh=="her",],aes(x=assessment,y=rel_se,fill=assessment)) + facet_nested(sel~HD+var, labeller=label_both) #
        print(
          p+geom_boxplot(position=position_dodge(1)) +ylab("Relative standard error")+ scale_fill_brewer(palette="Greys") + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        )
        
        dev.off()
        pdf(paste0("C:/Users/LukeB/Documents/latex_p2/4sims/mon",varh,"Fcondense.pdf"), onefile = FALSE, paper = "special",width=8, height=6,pointsize=12)
        
        p<-ggplot(tmpdat[tmpdat$lh=="mon",],aes(x=assessment,y=rel_se,fill=assessment)) + facet_nested(sel~HD+var, labeller=label_both) #
        print(
          p+geom_boxplot(position=position_dodge(1)) +ylab("Relative standard error")+ scale_fill_brewer(palette="Greys") + theme_classic()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
        )
        
        
        dev.off()
        
        