rm(list=ls(all=T))
source("sfc3.0.R")
pc=sfc.model("ch4.txt",modelName="pc")
pc<-sfc.check(pc,fill=T) 
86.485
86.485
0.025
21.62
pc
#simulates the model
datapc<-simulate(pc, maxIter=100)


# a 100 basis points interest rate shock in the model
init = datapc$baseline[66,]
pc.shock1<-sfc.addScenario(model=pc,vars="r_bar",values=0.035,inits=1960,ends=2010,starts=init)
shock1<-simulate(pc.shock1, maxIter=100)

##plot figure 4.3.. p-112
plot(pc$time,shock1$scenario[,"SOM"],type="l",xlab="",ylab="",lty=1,lwd=2,)
par(new=T)
plot(pc$time,(shock1$scenario[,"SOB"]),lty=2, lwd=2,type="l",axes=F,ylab="",xlab="")
axis(4,pretty(c(0.750, 1.1*max(shock1$scenario[,"SOB"],na.rm=T))))
legend(x=1970,y=0.78,legend=c("Share of Bills","Share of Money"),lty=c(2,1),lwd=2,bty="n")
grid()

#COnsunption and disposable income after interest rate shcoks  ...fig 4.4 . p 113
time2=c(1950:2000)
plot(time2,shock1$scenario[as.character(time2),"yd"],type="l",xlab="",ylab="",lty=1,lwd=2, col="green")
lines(time2,shock1$scenario[as.character(time2),"cons"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
legend(x=1970,y=88,legend=c("Disposable Income","Consumption"),lwd=c(2,2),lty=c(1,1),col=c(3,2),bty="n")
grid()