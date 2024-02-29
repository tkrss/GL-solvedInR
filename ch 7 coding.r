rm(list=ls(all=T))
source("sfc3.0.R")
bmw=sfc.model("ch7.txt",modelName="sim")
bmw<-sfc.check(bmw,fill=T) # set initial values to 0
0
0
#simulates the model
data.bmw<-simulate(bmw)

# create a shock in autonomous consumption, set alpha0 = 28
init = data.bmw$baseline[66,]
bmw.shock1<-sfc.addScenario(model=bmw,vars="alpha0",values=28,inits=1960,ends=2010,starts=init)
shock1<-simulate(bmw.shock1)

# plots of disposible income and consumption after shock1 (alpha = 28)
# Replicates figure 7.1 page 231
plot(bmw$time,shock1$scenario[,"yd"],type="l",xlab="",ylab="",lty=1,lwd=2, col="green")
lines(bmw$time,shock1$scenario_1[,"c_d"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
legend(x=1980,y=190,legend=c("Disposable Income","Consumption"),lwd=c(2,2),lty=c(1,1),col=c(3,2),bty="n")
grid()

# Now create a shock in the model BMW by alpha1 equals 0.74
init = data.bmw$baseline[66,]
bmw.shock2<-sfc.addScenario(model=bmw,vars="alpha1",values="0.74",inits=1960,ends=2010,starts=init)
shock2<-simulate(bmw.shock2)

# plots of disposible income and consumption after shock2 (alpha1 = 0.74)
# Note. shock 2 is in the original model, which means shock 1 is not present in the system when I created shock 2
# Replicate figure 7.3 page no. 232
plot(bmw$time,shock2$scenario[,"yd"],type="l",xlab="",ylab="",lty=1,lwd=2, col="green")
lines(bmw$time,shock2$scenario[,"c_d"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
legend(x=1980,y=180,legend=c("Disposable Income","Consumption"),lwd=c(2,2),lty=c(1,1),col=c(3,2),bty="n")
grid()
# How is Ouput to Capital Ratio (OCR) affected by shock2  ... figure 7.4 (page 240)
plot(bmw$time,shock2$scenario[,"OCR"],type="l",xlab="",ylab="",lty=1,lwd=2, col="blue")
legend(x=1975,y=0.96,legend="Output to Capital Ratio",lwd= 2 ,bty="n")
grid()

# How is wage (w) affected by shock 2..figure 7.5 p-242    .. This shock does not replicate the book exactly due to time period or something. not an important issue at the moment.
init = data.bmw$baseline[66,]
bmw.shock2<-sfc.addScenario(model=bmw,vars="alpha1",values="0.74",inits=1960,ends=2010,starts=init)
shock2<-simulate(bmw.shock2)
plot(bmw$time,shock2$scenario[,"w"],type="l",xlab="",ylab="",lty=1,lwd=2, col="blue")
legend(x=1975,y=0.96,legend="Real wage",lwd= 2 ,bty="n")
grid()

