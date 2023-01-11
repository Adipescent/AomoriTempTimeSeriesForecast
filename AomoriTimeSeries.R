library(MASS)

temp = read.csv("monthly_temperature_aomori_city.csv",header = TRUE)
train = temp[c(1:1644),]
test = temp[c(1645:1668),]
test.ts = ts(test[,3],start = c(2019,1),frequency = 12)
test.ts = (test.ts*1.8)+32
temp.ts = ts(train[,3],start = c(1882,1),frequency = 12)
temp.ts = (temp.ts*1.8)+32 #convert to Farenheit so that it won't have negative values

#===================Box-Cox Transform=======================
t=1:length(temp.ts)
fit = lm(temp.ts~t)
bcTransform= boxcox(temp.ts~t, plotit =TRUE)

lambda = bcTransform$x[which(bcTransform$y == max(bcTransform$y))]
temp.bc =(1/lambda)*(temp.ts^lambda-1)
#train = (1/lambda)*(temp.ts^lambda-1)

op <- par(mfrow = c(1,2))
ts.plot(temp.ts,main = "Original data",ylab = expression(X[t]))
ts.plot(temp.bc,main = "Box-Cox tranformed data", ylab = expression(Y[t]))

var(temp.ts)
var(temp.bc)
hist(temp.ts)
hist(temp.bc)

#boxcox transformation seems to reduce variance a lot as well as made the data look
#slightly more normally distributed and spread out a little more evenly. Therefore
#I shall use the box-cox transfomation. 
#==========================Using Regular data===================
op <- par(mfrow = c(1,1))
ts.plot(temp.ts,main = "Original data",ylab = expression(X[t]))        

acf(temp.ts,lag.max = 100)
#shows slight seasonality
pacf(temp.ts,lag.max= 100)
ts.plot(temp.ts)
abline(lm(temp.ts~as.numeric(1:length(temp.ts))),col = "blue")
abline(h=mean(temp.ts),col="red")
var(temp.ts)
diff12 = diff(temp.ts,lag = 12, difference = 1)
ts.plot(diff12)
acf(ts(diff12,freq=1),lag.max  = 100,main = "ACF 100 lags after differencing at lag 12")
pacf(ts(diff12,freq=1),lag.max = 100,main = "PACF 100 lags diff at 12")
var(diff12)
#acf no longer slowly decaying

diff12_1 = diff(diff12,lag = 1,difference = 1)
ts.plot(diff12_1)#plot after differencing at lag 12 and 1
acf(ts(diff12_1,frequency=1),lag.max = 100,main = "ACF 100 lag, diff 12 and 1")#P = 12
pacf(ts(diff12_1,frequency=1),lag.max = 100,main = "PACF 100 lags, diff at 12 and 1",)
var(diff12_1)
#=================using Box-cox=====================
acf(temp.bc,lag.max = 100)
#shows slight seasonality
pacf(temp.bc,lag.max= 100)
ts.plot(temp.bc)
abline(lm(temp.bc~as.numeric(1:length(temp.ts))))
abline(h=mean(temp.bc),col="red")
var(temp.bc)
diff12 = diff(temp.bc,lag = 12, difference = 1)
ts.plot(diff12)
acf(ts(diff12,freq=1),lag.max  = 100,main = "ACF 100 lags after differencing at lag 12")
pacf(ts(diff12,freq=1),lag.max = 100,main = "PACF 100 lags diff at 12")
var(diff12)
#acf no longer slowly decaying

diff12_1 = diff(diff12,lag = 1,difference = 1)
ts.plot(diff12_1)#plot after differencing at lag 12 and 1
acf(ts(diff12_1,frequency=1),lag.max = 100,main = "ACF 100 lag, diff 12 and 1")#P = 12
pacf(ts(diff12_1,frequency=1),lag.max = 100,main = "PACF 100 lags, diff at 12 and 1",)
var(diff12_1)
#variance is now very small but it did get bigger then if I were to just difference at 
#lag 12. 

#=================Histogram=========================
hist(temp.bc)
hist(diff12)
hist(diff12_1)
#The histogram of the data differenced only at lag 12 looks symmetric and Gaussian.
#When it is also differenced at lag 1, there seems to be some sort of outlier that is 
#increasing the variance. The original data does not seem to have much of a trend so
#it may be best to not use this differencing. 
#If diff12_1 was used it may just be an MA model.
#===================ACF&PACF==========================
acf(ts(diff12,freq=1),lag.max  = 100,main = "ACF 100 lags after differencing at lag 12")
#Q= 1, q = 1 or 2 or 3 (probably 2)

pacf(ts(diff12,freq=1),lag.max = 200,main = "PACF 100 lags diff at 12")
#P = 4 or 7..., p = 1 or 2 or 11
#D = 1, d =0
#==================Models=============================
#library(qpcR)

#model (p,d,q)x(P,D,Q)
#model 1: s =12,D = 1, d= 0, Q = 1, P = 4, q=2, p = 2
# 2,0,2)x(4,1,1)
x= arima(temp.bc, order=c(2,0,2), seasonal = list(order = c(4,1,1), period = 12), method="ML",optim.method="Nelder-Mead")
x
x2= arima(temp.bc, order=c(2,0,3), seasonal = list(order = c(3,1,1), period = 12), method="ML",optim.method="Nelder-Mead")
x2
# y= arima(diff12, order=c(2,0,2), seasonal = list(order = c(4,1,1), period = 12), method="ML")
# y
#x2 smaller aic:1958.85
x2.1 <- arima(temp.ts, order=c(2,0,2), seasonal = list(order = c(4,1,1), period = 12), method="ML")
x2.1
x2.1.na <- arima(temp.ts, order=c(2,0,2), seasonal = list(order = c(0,1,4), period = 12), method="ML")
x2.1.na
#x2.1 smaller aci 1952.04,temp.ts w/o Nelder Meald - 7101.56
x3 <- arima(temp.ts, order=c(1,1,1), seasonal = list(order = c(0,1,1), period = 12), method="ML")
x3# 
x3.na <- arima(temp.ts, order=c(2,0,2), seasonal = list(order = c(3,1,1), period = 12), method="ML",fixed = c(NA,NA,NA,NA,0,0,0,NA))
x3.na
#x3 has the smallest aic 1945.55
x3.1<- arima(temp.ts, order=c(2,1,2), seasonal = list(order = c(1,1,1), period = 12), method="ML")
x3.1
#with differencing at lag 1 aic is smaller

#AICc(arima(temp.bc, order=c(2,0,2), seasonal = list(order = c(1,1,1), period = 12),fixed = c(NA,NA,NA,NA,0,NA), method="ML",optim.method="Nelder-Mead"))
#

#Check if your coefficients have changed


#
#========================Model equation============================
library(plotrix) 
# x3 -> (1-0.3378B)Yt = (1-0.9829B)(1-0.9764B^12)
plot(polyroot(c(1,-0.3378)),xlim = c(-2,5),ylim = c (-5,5))
plot(polyroot(c(1,-0.9829)),xlim = c(-2,5),ylim = c (-5,5))#looking closely it is outside
#(1-0.5974B+0.0346B^2)nabla(12),nabla(1)Bc(U_t)= (1-1.3769B+0.3688B^2)(1-0.9671B^12)
plot(polyroot(c(1,-1.3769,0.3688)),xlim = c(-2,4),ylim=c(-2,2))#x3.1 not invertible
draw.circle(0,0,1)
plot(polyroot(c(1,-0.5974,0.0346)),xlim=c(-20,20),ylim=c(-20,20))#x3.1 stationary
draw.circle(0,0,1)
#(1+0.2309B-0.6059B^2)(1-0.0345B^12)U_t = (1 +0.5733B -0.3462B^2)(1-0.9380B^12)
plot(polyroot(c(1,0.2309,-0.6059)),xlim = c(-2,4),ylim=c(-2,2))#x3 is stationary
draw.circle(0,0,1)
plot(polyroot(c(1,0.5733,-0.3462)),xlim = c(-2,4),ylim=c(-2,2))#x3 is invertible
draw.circle(0,0,1)
#second model seems to be stationary and invertible. 
#=======================diagnostic checking(x3)=============================
resx3<-residuals(x3)
hist(resx3,density=20,breaks=20, col="blue", xlab="", prob=TRUE)         
m <- mean(resx3)
std <- sqrt(var(resx3))
curve( dnorm(x,m,std), add=TRUE )
plot.ts(resx3)
fitt <- lm(resx3 ~ as.numeric(1:length(resx3))); abline(fitt, col="red") 
abline(h=mean(resx3), col="blue")
qqnorm(resx3,main= "Normal Q-Q Plot for Model B")
qqline(resx3,col="blue")
acf(resx3,lag.max = 100)
pacf(resx3,lag.max = 100)
shapiro.test(resx3)#should not use transformations because then it does not fit shapiro wilk test
# 6 lags seem to be outside of the confidence intervals for the pacf of the residuals when using the bc transformation
Box.test(resx3,lag = 12,type = c("Box-Pierce"),fitdf = 2)
Box.test(resx3, lag = 12, type = c("Ljung-Box"), fitdf = 2)
Box.test((resx3)^2, lag = 12, type = c("Ljung-Box"), fitdf = 0)

#=======================Diagnostic checking(x3.1)=====================
resx3.1<-residuals(x3.1)
hist(resx3.1,density=20,breaks=20, col="blue", xlab="", prob=TRUE)         
m <- mean(resx3.1)
std <- sqrt(var(resx3.1))
curve( dnorm(x,m,std), add=TRUE )
plot.ts(resx3.1)
fitt <- lm(resx3.1 ~ as.numeric(1:length(resx3.1))); abline(fitt, col="red") 
abline(h=mean(resx3.1), col="blue")
qqnorm(resx3.1,main= "Normal Q-Q Plot for Model B")
qqline(resx3.1,col="blue")
acf(resx3.1,lag.max = 100)
pacf(resx3.1,lag.max = 100)
shapiro.test(resx3.1)
#7 lags for the pacf seem to be outside of the pacf
#=====================Diagnostic checking(x2.1)=========================
resx2.1<-residuals(x2.1)
hist(resx2.1,density=20,breaks=20, col="blue", xlab="", prob=TRUE)         
m <- mean(resx2.1)
std <- sqrt(var(resx2.1))
curve( dnorm(x,m,std), add=TRUE )
plot.ts(resx2.1)
fitt <- lm(resx2.1 ~ as.numeric(1:length(resx2.1))); abline(fitt, col="red") 
abline(h=mean(resx2.1), col="blue")
qqnorm(resx2.1,main= "Normal Q-Q Plot for Model B")
qqline(resx2.1,col="blue")
acf(resx2.1,lag.max = 100)
pacf(resx2.1,lag.max = 100)
shapiro.test(resx2.1)
#======================Forecasting===========================================
library(forecast)
forecast(x3)
pred.tr <- predict(x3, n.ahead = 24)
U.tr= pred.tr$pred + 2*pred.tr$se 
L.tr= pred.tr$pred - 2*pred.tr$se 
ts.plot(temp.ts, xlim=c(1880,+2020+24), ylim = c(min(temp.ts),max(U.tr)))
lines(U.tr, col="blue", lty="dashed")
lines(L.tr, col="blue", lty="dashed")
points( pred.tr$pred, col="red")

#with true values
U= exp(U.tr)
L= exp(L.tr)

ts.plot(test.ts,xlim = c(2019,2021),ylim = c(min(temp.ts),max(U.tr)),col="red")
lines(U.tr, col="blue", lty="dashed")
lines(L.tr, col="blue", lty="dashed")
points( pred.tr$pred, col="black")
# points((length(test)+1):(length(test)+24), pred.orig, col="green")
# points((length(apt)+1):(length(apt)+12), pred.orig, col="black")
