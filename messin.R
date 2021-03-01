

# list(scenario = as.name(paste(lh,ts,sel,nm,ar,sr, sep="_")), sce_id = id, 
#      stk_c = stk_c, idx_c = idx_c, 
#      stk_ow = stk_ow, idx_ow = idx_ow, 
#      stk_rc = stk_rc, idx_rc = idx_rc)

# 
#         
# jeff<-list(scenario = as.name(paste(lh,ts,sel,nm,ar,sr, sep="_")), sce_id = id,
#   stk_c = list(stock_n = stock.n(stk_c), catch_n = catch.n(stk_c), catch_wgt = catch.wt(stk_c), idx = index(idx_c)),
#   stk_ow = list(stock_n = stock.n(stk_ow), catch_n = catch.n(stk_ow), catch_wgt = catch.wt(stk_ow), idx = index(idx_ow)),
#   stk_rc = list(stock_n = stock.n(stk_rc), catch_n = catch.n(stk_rc), catch_wgt = catch.wt(stk_rc), idx = index(idx_rc))
#      

# 
# jeff<-list(scenario = as.name(paste(lh,ts,sel,nm,ar,sr, sep="_")), sce_id = id,
#            stk_c = list(idx = index(idx_c)),
#            stk_ow = list(idx = index(idx_ow)),
#            stk_rc = list(idx = index(idx_rc))
# )






# graphics::plot(catch(iter(stk_rc,1)),ylim=c(0,20000))
# lines(effort(iter(idx_rc,1)),col=2)  
# lines(colSums(catch.n(iter(idx_rc,1))*catch.wt(iter(idx_c,1))),col=3)      
# 
# 
# graphics::plot(colSums(catch.n(iter(stk_rc,1))),ylim=c(0,4000))
# lines(colSums(catch.n(iter(idx_rc,1))),col=2)      



# idx_c <- caterror(stk_c,idx_c)          
# idx_ow <- caterror(stk_ow,idx_ow)          
# idx_rc <- caterror(stk_rc,idx_rc)       
# 



# 
# 
# # # 
# #


plot(iter(xx$stk,9))
dev.off()
graphics::plot(catch(iter(xx$stk,9)),ylim=c(0,20000))
 lines(effort(idx),col=2)  
 lines(colSums(catch.n(idx)*catch.wt(idx)),col=3)  

 Y2<-c(quantSums(catch.wt(iter(xx$stk,9))[1,]*catch.n(idx)[1,])/quantSums(catch.n(iter(xx$stk,9))[1,]))
 Z2<-c(quantSums(catch.wt(iter(xx$stk,9))[pr_range,]*catch.n(iter(xx$stk,9))[pr_range,])/quantSums(catch.n(iter(xx$stk,9))[pr_range,]))
 X2<-c(quantSums(catch.wt(iter(xx$stk,9))[f_range,]*catch.n(iter(xx$stk,9))[f_range,])/quantSums(catch.n(iter(xx$stk,9))[f_range,]))
 
 
Y<-mean_wts[1,]
Z<-mean_wts[2,]
X<-mean_wts[3,]

par(mfrow=c(1,2))
#model X -> Z progression with linear model
plot(y=Z[2:no.years],x=X[1:no.years-1],xlim=c(0,max(Z[2:no.years])),ylim=c(0,max(Z[2:no.years])),pch=19,xaxs="i",yaxs="i",bty="l",xlab=expression(paste("X"[t]," (kg)")),ylab=expression(paste("Z"[t+1]," (kg)")))


points(y=Z2[2:no.years],x=X2[1:no.years-1],col=2)
mod<-lm(Z[2:no.years]~X[1:no.years-1])
x=seq(0,100,by=0.01)
lines(x=x,y=(coef(mod)[1] + coef(mod)[2]*x),col=2)



legend("topleft", legend =expression(paste("Linear model prediction",(Z[t+1]==italic(W) + rho*X[t]))), col = c(2), lty = c(1),cex=.8,box.lty=0)


W1<-coef(mod)[1]
rho1<-coef(mod)[2]
#visualise - red lines are growth from X -> Z


plot(y=Y,x=tp,type="l",col=4,ylim=c(0,max(Z[2:no.years])),pch=19,xaxs="i",yaxs="i",bty="l",xlab="Year",ylab="Mean weight (kg)")
lines(y=Z,x=tp,col=2)
lines(y=X,x=tp,col=3)

xtmp1<-W1 + rho1*X
#
for(i in 1:no.years){
  lines(y=c(X[i],xtmp1[i]),x=c(tp[i],tp[i+1]),col=1,lty=2)

}

legend("topright", legend = c("Growth","Y (Recruits)", "Z (Previously exploited biomass)", "X (Total biomass)"), col = c(1,4,2,3), lty = c(2,1,1,1),cex=0.8,box.lty=0)

# #dev.off()

# 
# 
# xx$schnualt_par[,,1]
# xx$schnub0_par[,,1]
# xx$schnualt[,,1]
# xx$schnub0[,,1]
# xx$csa[,,1]
# xx$csa_par[,,1]
# 


