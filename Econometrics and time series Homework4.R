#1
#load data
card_io <- read.table("Cardio.txt")
car_dio <- as.ts(card_io[,1])
#1.fit yule walker and ols
ar_yw <- ar(car_dio,method="yule-walker")
ar_ols <- ar(car_dio,method="ols")
#summary(ar_yw)
#summary(ar_ols)

#print results
ar_yw$ar
as.vector(ar_ols$ar)
#2.estimated std error obtained by liner regression
ar_ols$asy.se.coef$ar

#using calculations to get the estimated standard errors
temp <- acf(car_dio,lag.max=4,plot=FALSE,"covariance")
gammatrix <- toeplitz(as.vector(c(temp$acf[1],temp$acf[2])))
r_sq <- matrix(c(temp$acf[2],temp$acf[3]),2,1)
car_phi <- solve(gammatrix)%*%r_sq
car_sigma2 <- temp$acf[1]-car_phi[1]*temp$acf[2]-car_phi[2]*temp$acf[3]
est_std_err <- sqrt(car_sigma2/length(car_dio)*solve(gammatrix))[1,1]
#compare results
est_std_err
ar_ols$asy.se.coef$ar[1]

#2
#import data
sun_spot <- read.table("SUNSPOT.txt")
#creating the gamma2 matrix and the r matrix
gamma <- c(1382.2,1114.4,591.73,96.216)
gam_ma <- matrix(c(gamma[1],gamma[2],gamma[2],gamma[1]),2,2)
r_2 <- matrix(c(gamma[2],gamma[3]),2,1)
#solving for phi and sigma squared
phi <- solve(gam_ma)%*%r_2
sigma_2 <- gamma[1]-phi[1]*gamma[2]-phi[2]*gamma[3]


#3
#import and fit burg model
la_ke <- read.table("lake.txt")
ar(la_ke,method="burg")


#4
#using library itsmr
library(itsmr)
#estimate MA coefficients using innovations algorithm, using m=17
ia(as.ts(la_ke),2,17)
#transforming MA to ARMA(1,1)
arma_phi <- temp$theta[2]/temp$theta[1]
arma_theta <- temp$theta[1]-temp$theta[2]/temp$theta[1]



