rm(list=ls(all=T))
source("sfc3.0.R")
lp=sfc.model("ch5.txt",modelName="lp")
lp<-sfc.check(lp,fill=T)
0
0
0
0

lp
#simulates the model
datalp<-simulate(lp, maxIter=100)

#create 2 shocks in the model,a; an increase in short-term interest rate, b; a decrease in longterm interes rate.
init = datalp$baseline[66,]
lp.shock1<-sfc.addScenario(model=lp,vars=list(c("r_b","p_bl_bar")),values=list(c(0.04,15)),inits=1960,ends=2010,starts=init)
shock1<-simulate(lp.shock1, maxIter=100)

# plot wealth to disposable income ratio; to show how it looks after the above shocks ##  fig 5.1 , p-152 :)
plot(lp$time,shock1$scenario[,"WYD"],type="l",xlab="",ylab="",lty=1,lwd=2, col="green")
legend(x=1980,y=96,legend="Wealth to Disposable Income Ratio",lwd=2,col="green",bty="n")
grid()

# plot consumotion and disposable income after shock1 (fig 5.2, p-152) 
plot(lp$time,shock1$scenario[,"cons"],type="l",xlab="",ylab="",lty=1,lwd=2, col="blue")
lines(lp$time,shock1$scenario[,"yd_r"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
legend(x=1980,y=96,legend=c("Consuption","Disposable Income"),lwd=c(2,2),lty=c(1,1),col=c("blue","red"),bty="n")
grid()

# plot bill to wealth ratio and bond to wealth ratio after shock1 hits the system (fig 5.4 p-153)
plot(lp$time,shock1$scenario[,"bills"],type="l",xlab="",ylab="",ylim=range(0.37,0.42),lty=1,lwd=2, col="blue")
lines(lp$time,shock1$scenario[,"bonds"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
legend(x=1970,y=0.397,legend=c("Bills to Wealth Ratio","Bonds to Wealth Ratio"),lwd=c(2,2),lty=c(1,1),col=c("blue","red"),bty="n")
grid()

#Lets create a shock no.2; a drop in the propensity to consume out of income i.e alpha1 from 0.8 to 0.7
init = datalp$baseline[66,]
lp.shock2<-sfc.addScenario(model=lp,vars=list(c("alpha1")),values=list(c(0.7)),inits=1960,ends=2010,starts=init)
shock2<-simulate(lp.shock2)

###Plot National Income (GDP) after shock2; fall in propensity to consume out of income
plot(lp$time,shock1$scenario[,"y"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
legend(x=1975,y=114,legend="Nation Income (GDP)",lwd=2,col="red",bty="n")
grid()



