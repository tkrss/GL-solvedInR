rm(list=ls(all=T))
source("sfc3.0.R")
dis=sfc.model("ch9.txt",modelName="dis")
##for this mode you can't set the initial values zero or the model would not be solved. Initial values for the model has to be specified
### I obtainined the initial values of the stocks from the book. Based on the steady state solution
dis=sfc.check(dis,fill=T) 
 0.15
 1
 1
 0.129
 0.129
 0.129
 1
 2.61
 1
 1
dis
#simulates the model. my computer is slow so I run 100 Iterations only
datadis<-simulate(dis, maxIter=100)

# create shock1; one-shot increase in the costing margin(called mark-up); phi from 0.25 to 0.3
init = datadis$baseline[66,]
dis.shock1<-sfc.addScenario(model=dis,vars=list(c("phi")),values=list(c(0.3)),inits=1960,ends=2010,starts=init)
shock1<-simulate(dis.shock1, maxIter=100)


# plot the GDP of region South and North after shock1
time2=c(1950:2000)
plot(time2,shock1$scenario[as.character(time2),"yd_k_hs"],type="l",xlab="",ylab="",lty=1, col="black")
lines(time2,shock1$scenario[as.character(time2),"c_k"],type="l",xlab="",ylab="",lty=1, col="red")
legend(x=1970,y=80.1,legend=c("Real Disposable Income", "Real Consumption"),lwd=c(2,2),col=c("black","red"),bty="n")
grid()

# create shock2; increase in the target inventories to sales ratio sigmat from 0.15 to 0.25
init = datadis$baseline[66,]
dis.shock2<-sfc.addScenario(model=dis,vars=list(c("sigmat")),values=list(c(0.25)),inits=1960,ends=2010,starts=init)
shock2<-simulate(dis.shock2, maxIter=100)

# plot (Haig¨CSimons) real disposable income and of real consumption after shock2 9.2 p 299..
plot(dis$time,shock2$scenario[,"yd_k_hs"],type="l",xlab="",ylab="",lty=1, col="black")
lines(dis$time,shock2$scenario[,"c_k"],type="l",xlab="",ylab="",lty=1, col="red")  
legend(x=1970,y=82,legend=c("Real Disposable Income", "Real Consumption"),col=c("black","red"),bty="n")
grid()  

# plot Desired increase in physical inventories and Change in realized inventories after shock2 ...fig 9.3 p-299
plot(time2,shock2$scenario[as.character(time2),"d.inv_k"],type="l",xlab="",ylab="",lty=1,lwd=2, col="black")
lines(time2,shock2$scenario[as.character(time2),"d.inv_k_e"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
legend(x=1970,y=1.25,legend=c("Change in realized Inventories", "Desired Increase Inventories"), lwd=c(2,2),col=c("black","red"))
grid() 
