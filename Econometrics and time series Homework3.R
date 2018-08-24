#Problem 2.7 in Time Series Analysis and its applications(Shumway and Stoffer)

#1.Create ACF of ARMA(1,1),(1,0),(0,1), and plot them
plot(as.ts(ARMAacf(ar=c(0.6),ma=c(0.9),lag.max=15)),col="black",lwd=2,
     main="ACF graph of ARMA models with different parameters",ylab="ACF",xlab="Lag")
lines(as.ts(ARMAacf(ar=c(0.6),lag.max=15)),col="red",lwd=2)
lines(as.ts(ARMAacf(ma=c(0.9),lag.max=15)),col="blue",lwd=2)
legend(x="topright", legend=c("ARMA(1,1)","ARMA(1,0)","ARMA(0,1)"), 
       inset=0.05, cex=1.25, lwd=2, lty=c(1, 1, 1), col=c("black", "red", "blue"))

#2.Generate 100 observations from above, compute sample ACF and PACF
#ARMA(1,1)
rand_om <- rnorm(100,0,1)
arma_11<- c(1:100)
arma_11[1] <- (1+0.6*0.9)*(0.6+0.9)/(1+2*0.6*0.9+0.9^2)
for(i in c(2:100)){
  arma_11[i] <- 0.6*arma_11[i-1]+rand_om[i]+0.9*rand_om[i-1]
}

arma11_acf <- acf(arma_11,type="correlation",plot=T,lag.max=100)
arma11_acf
arma11_pacf <- acf(arma_11,type="partial",lag.max=100)

#ARMA(1,0)
arma_10<- c(1:100)
arma_10[1] <- 0.6
for(i in c(2:100)){
  arma_10[i] <- 0.6*arma_10[i-1]+rand_om[i]
}

arma10_acf <- acf(arma_10,type="correlation",plot=T,lag.max=100)
arma10_acf
arma10_pacf <- acf(arma_10,type="partial",lag.max=100)

#ARMA(0,1)
arma_01<- c(1:100)
arma_01[1] <- 0.9/(1+0.9^2)
for(i in c(2:100)){
  arma_01[i] <- rand_om[i]+0.9*rand_om[i-1]
}

arma01_acf <- acf(arma_01,type="correlation",plot=T,lag.max=100)
arma01_acf
arma01_pacf <- acf(arma_01,type="partial",lag.max=100)

##Making sure data is as wanted
#plot.ts(arma_11)
#lines(as.ts(arma_10),col="red")
#lines(as.ts(arma_01),col="blue")






