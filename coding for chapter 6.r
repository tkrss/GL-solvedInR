rm(list=ls(all=T))
source("sfc3.0.R")
reg=sfc.model("ch6.txt",modelName="reg")
reg<-sfc.check(reg,fill=T)  # for this model I've already specified the initial values in the text file
reg

#simulates the model
datareg<-simulate(reg, maxIter=100)

### if a model is not simulating and taking too long just reduce the number of iterations as I experienced in this chapter
# datareg1=simulate(reg1,maxIter=100)


# Create a shock; increase in the propensity to import of Region S  from mu_s = 0.18781  to mu_s= 0.20781
init = datareg$baseline[66,]
reg.shock1<-sfc.addScenario(model=reg,vars=list(c("mu_s")),values=list(c(0.20781)),inits=1960,ends=2010,starts=init)
shock1<-simulate(reg.shock1, maxIter=100)

###Plots to Household disposable Income, Govt Balance and & trade Balance after shock1...Fig 6.1, p 181
plot(reg$time,shock1$scenario[,"HHW"],type="l",xlab="",ylab="",lty=1,lwd=2,ylim=range(-1.30,0), col="blue")
lines(reg$time,shock1$scenario[,"GOV"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
lines(reg$time,shock1$scenario[,"TBAL"],type="l",xlab="",ylab="",lty=1,lwd=2, col="green")
legend(x=1970,y=-1,legend=c("Household Change in Wealth","Government Balance", "Trade Balance"),lwd=c(2,2,2),col=c("blue","red","green"),bty="n")
grid()

# plot the GDP of region South and North after shock1...Fig 6.2. p 182
plot(reg$time,shock1$scenario[,"y_n"],type="l",xlab="",ylab="",lty=1,lwd=2, col="black", ylim=range(102,111))
lines(reg$time,shock1$scenario[,"y_s"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
legend(x=1970,y=107,legend=c("North Region GDP", "SOUTH REGION GDP"),lwd=c(2,2,2),col=c("black","red"),bty="n")
grid()

# Create a shock2; increase in the govt expenditure of Region South from g_s = 20  to g_s= 25 
init = datareg$baseline[66,]
reg.shock2<-sfc.addScenario(model=reg,vars=list(c("g_s")),values=list(c(25)),inits=1960,ends=2010,starts=init)
shock2<-simulate(reg.shock2, maxIter=100)

# plot the GDP of region South and North after shock2  ,,,,....Fig 6.3 , p 184
plot(reg$time,shock2$scenario[,"y_n"],type="l",xlab="",ylab="",lty=1,lwd=2, col="black", ylim=range(105,126))
lines(reg$time,shock2$scenario[,"y_s"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
legend(x=1970,y=120,legend=c("SOUTH REGION GDP", "NORTH REGION GDP"),lwd=c(2,2),col=c("red","black"),bty="n")
grid()

###Plots of Household disposable Income, Govt Balance and & trade Balance after shock2...Fig 6.4, p 184
plot(reg$time,shock2$scenario[,"HHW"],type="l",xlab="",ylab="",lty=1,lwd=2,ylim=range(-3.5,2.5), col="blue")
lines(reg$time,shock2$scenario[,"GOV"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
lines(reg$time,shock2$scenario[,"TBAL"],type="l",xlab="",ylab="",lty=1,lwd=2, col="green")
legend(x=1968,y=2,legend=c("Household Change in Wealth","Government Balance", "Trade Balance"),lwd=c(2,2,2),col=c("blue","red","green"),bty="n")
grid()

# create a shock3; Third experiment: increase in household saving propensity Region S... from alpha1= 0.7 to alpha1= 0.6
init = datareg$baseline[66,]
reg.shock3<-sfc.addScenario(model=reg,vars=list(c("alpha1_s")),values=list(c(0.6)),inits=1960,ends=2010,starts=init)
shock3<-simulate(reg.shock3, maxIter=100)

# plot the GDP of region South and North after shock3  ,,,,....Fig 6.5 , p 185
plot(reg$time,shock3$scenario[,"y_n"],type="l",xlab="",ylab="",lty=1,lwd=2, col="black", ylim=range(90,110))
lines(reg$time,shock3$scenario[,"y_s"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
legend(x=1970,y=103.5,legend=c("SOUTH REGION GDP", "NORTH REGION GDP"),lwd=c(2,2),col=c("red","black"),bty="n")
grid()

###Plots of Household disposable Income, Govt Balance and & trade Balance after shock3...Fig 6.6, p 186
plot(reg$time,shock3$scenario[,"HHW"],type="l",xlab="",ylab="",lty=1,lwd=2,ylim=range(-3,5), col="blue")
lines(reg$time,shock3$scenario[,"GOV"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
lines(reg$time,shock3$scenario[,"TBAL"],type="l",xlab="",ylab="",lty=1,lwd=2, col="green")
legend(x=1968,y=4,legend=c("Household Change in Wealth","Government Balance", "Trade Balance"),lwd=c(2,2,2),col=c("blue","red","green"),bty="n")
grid()

# create a shock4; Fourth experiment: increase in the liquidity preference in Region S.. from alpha1= 0.67 to alpha1= 0.75
init = datareg$baseline[66,]
reg.shock4<-sfc.addScenario(model=reg,vars=list(c("lambda0_s")),values=list(c(0.75)),inits=1960,ends=2010,starts=init)
shock4<-simulate(reg.shock4, maxIter=100)

###Plots of Household disposable Income, Govt Balance and & trade Balance after shock4...Fig 6.6, p 186
plot(reg$time,shock4$scenario[,"HHW"],type="l",xlab="",ylab="",lty=1,lwd=2,ylim=range(-0.13,0.13), col="blue")
lines(reg$time,shock4$scenario[,"GOV"],type="l",xlab="",ylab="",lty=1,lwd=2, col="red")
lines(reg$time,shock4$scenario[,"TBAL"],type="l",xlab="",ylab="",lty=1,lwd=2, col="green")
legend(x=1968,y=0.13,legend=c("Household Change in Wealth","Government Balance", "Trade Balance"),lwd=c(2,2,2),col=c("blue","red","green"),bty="n")
grid()




