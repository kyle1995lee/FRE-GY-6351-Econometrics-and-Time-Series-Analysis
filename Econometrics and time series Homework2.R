#open up new tab for plot
x11()

#theoretical ACF values
ARMAacf(ar=c(-0.2,0.48),lag.max=8)
ARMAacf(ar=c(-0.2,0.48),lag.max=8,pacf=TRUE)
plot(as.ts(ARMAacf(ar=c(-0.2,0.48),lag.max=100)))
plot(as.ts(ARMAacf(ar=c(-0.2,0.48),lag.max=100,pacf=TRUE)))
#generated ACF values
rand_om <- rnorm(100,0,1)
ar_model <- rand_om
ar_model[1] <- 1
ar_model[2] <- 1
for(i in c(3:100)){
  ar_model[i] <- -0.2*ar_model[i-1]+0.48*ar_model[i-2]+rand_om[i]
}
#print(ar_model)
ar_acf <- acf(ar_model,type="correlation",plot=T)
ar_acf
ar_pacf <- acf(ar_model,type="partial")


#C
#theoretical ACF values
ARMAacf(ar=c(-0.6),ma=c(1.2),lag.max=8)
ARMAacf(ar=c(-0.6),ma=c(1.2),lag.max=8,pacf=TRUE)
plot(as.ts(ARMAacf(ar=c(-0.6),ma=c(1.2),lag.max=100)))
plot(as.ts(ARMAacf(ar=c(-0.6),ma=c(1.2),lag.max=100,pacf=TRUE)))
#generated ACF values
rand_om2 <- rnorm(100,0,1)
ar_model2 <- rand_om2
ar_model2[1] <- 1
for(i in c(2:100)){
  ar_model2[i] <- -0.6*ar_model2[i-1]+rand_om2[i]+1.2*rand_om2[i-1]
}
#print(ar_model)
ar_acf <- acf(ar_model2,type="correlation",plot=T)
ar_acf


#D
#theoretical ACF values
ARMAacf(ar=c(-1.8,-0.81),ma=c(-0.4,0.04),lag.max=8)
ARMAacf(ar=c(-1.8,-0.81),ma=c(-0.4,0.04),lag.max=8,pacf=TRUE)
plot(as.ts(ARMAacf(ar=c(-1.8,-0.81),ma=c(-0.4,0.04),lag.max=100)))
plot(as.ts(ARMAacf(ar=c(-1.8,-0.81),ma=c(-0.4,0.04),lag.max=100,pacf=TRUE)))
#generated ACF values
rand_om3 <- rnorm(100,0,1)
ar_model3 <- rand_om3
ar_model3[1] <- 1
ar_model3[2] <- 1
for(i in c(3:100)){
  ar_model3[i] <- -1.8*ar_model3[i-1]-0.81*ar_model3[i-2]+rand_om3[i]-0.4*rand_om3[i-1]+0.04*rand_om3[i-2]
}
#print(ar_model)
ar_acf <- acf(ar_model3,type="correlation",plot=T)
ar_acf
