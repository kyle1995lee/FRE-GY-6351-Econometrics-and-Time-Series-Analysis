# Fit a regression line of lake level vs time
da_ta <- read.table("lake.txt")
la_ke <- unname(unlist(da_ta))
ti_me <- 1875:1972
x11()
plot(ti_me,la_ke, xlab="year",ylab="lake level", pch=4,main ="regression of lake level vs time")
reg_formula <- la_ke ~ ti_me
reg_model <- lm(reg_formula)
abline(reg_model,lwd=2,col="red")

# plot the residuals
x11()
plot(ti_me,reg_model$residuals,xlab="year",ylab="residuals", main ="residuals of lake level vs time")
abline(h=0,lwd=2,col="red")


# Fit a regression line of lake level vs last years level
x11()
last_lake <- c(NA,la_ke[1:97])
plot(last_lake,la_ke,xlab="last year lake level",ylab="lake level", main="regression of lake level vs last years",pch=4)
reg_formula2 <- la_ke ~ last_lake
reg_model2 <- lm(reg_formula2)
abline(reg_model2,lwd=2,col="red")

# plot the residuals
last_lake_plot <- last_lake[2:98]
plot(last_lake_plot,reg_model2$residuals,xlab="last year lake level",ylab="residuals", main ="residuals of lake level vs last")
abline(h=0,lwd=2,col="red")


#better fit comparison(compare adjusted R^2(larger) and residual standard error(smaller) better)
summary(reg_model)
summary(reg_model2)

#moving average
movavg <- function(x,n=3){stats::filter(x,rep(1/n,n), sides=2)}
lake_ma <- movavg(la_ke)
lake_ma2 <- la_ke-lake_ma
plot(lake_ma2)
plot(ti_me,lake_ma2, xlab="year",ylab="lake level detrend",main ="regression of lake level vs time")
reg_formula3 <- lake_ma2 ~ ti_me
reg_model3 <- lm(reg_formula3)
abline(reg_model3,lwd=2,col="red")

time_plot <- ti_me[3:98]
plot(time_plot,reg_model3$residuals,xlab="year",ylab="residuals", main ="residuals of lake level vs time")
abline(h=0,lwd=2,col="red")



#2 import wine data
wi_ne <- read.table("wine.txt")
colnames(wi_ne) <- c("year", "month","sales")
win_e <- log(wi_ne$sales)
library(zoo)
tim_e <- as.yearmon(paste(wi_ne$year, wi_ne$month),"%Y %m")
plot(tim_e, win_e,main="Wine data")
plot(as.ts(win_e),main="wine data original")
library("forecast")
trend_wine <- ma(win_e,order=4,centre=T)
detrend_wine <- win_e-trend_wine
plot(as.ts(detrend_wine),main="detrended wine data", xlab="Time",ylab="Quantity")

ts_wine <- ts(win_e,frequency=12)
decompose_wine <- decompose(ts_wine,"additive")
plot(as.ts(decompose_wine$seasonal),main="seasonality of wine data")
plot(as.ts(decompose_wine$trend))
plot(as.ts(decompose_wine$random),main="detrended and deseasoned wine data")
plot(decompose_wine)

