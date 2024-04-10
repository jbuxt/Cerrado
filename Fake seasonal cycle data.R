#Make synthetic dataset

library(asdetect)
library(chngpt)

#Crude way
x <- seq(0,239)
y <- 0.1*(sin(2*x*pi/12) + rnorm(240,mean=0,sd=0.1))+ 0.5

plot(x,y,type="l")

y_ts <- ts(y,frequency = 12)
y_stl <- stl(y_ts, s.window="periodic",t.window = 43)
 
plot(y_stl)


#Choose random point for abrupt change

y <- 0.1*(sin(2*x*pi/12) + rnorm(240,mean=0,sd=0.1))+ 0.5

random_index <- sample(index,1)
y_abrupt <- y

for (i in 1:length(x)){
  if (i >= random_index){
    y_abrupt[i] <- y[i] + 0.1
  }
}

shift_detect <- as_detect(y_abrupt)

par(mfrow=c(2,1))
plot(y_abrupt,type='l')
plot(shift_detect,type='l')

y_abrupt_stl <- stl(ts(y_abrupt,frequency=12), s.window=13)
y_abrupt_stl_periodic <- stl(ts(y_abrupt,frequency=12), s.window="periodic")
plot(y_abrupt_stl)


y_abrupt_trend <- y_abrupt_stl$time.series[,2]
y_abrupt_trend_asdetect <- as_detect(y_abrupt_trend)
plot(y_abrupt_trend,type='l')
plot(y_abrupt_trend_asdetect,type='l')

fit=chngptm(formula.1=(1:length(y_abrupt_trend)), formula.2=y_abrupt_trend, type="segmented", family="gaussian")
test=chngptm.test(formula.1=(1:length(y_abrupt_trend)), formula.2=y_abrupt_trend, type="segmented", family="gaussian")


fit=chngptm(formula.1=x~1, formula.2=~index, data= x, type="step",
            family="gaussian", var.type="bootstrap", ci.bootstrap.size=10)

test=chngpt.test(formula.null=x~1, formula.chngpt=~index, data= x, type="step", family="gaussian")


plot(fit$)
summary(fit)

x <- as.data.frame(y_abrupt_trend)
x['index'] <- 1:length(y_abrupt_trend)

#Use asdetect







#Ignore below for now...
#Choose random point for gradual linear change


y <- 0.1*(sin(2*x*pi/12) + rnorm(240,mean=0,sd=0.1))+ 0.5

plot(y,type='l')

random_index <- sample(index,1)
y_linear <- y

for (i in 1:length(x)){
  if (i >= random_index){
    y_linear[i] <- y[i] +0.0005*x[i] - 0.001*x[random_index]
  }
}


plot(y_linear,type='l')

shift_detect <- as_detect(y_linear)

par(mfrow=c(2,1))
plot(y_linear,type='l')
plot(shift_detect,type='l')


# Remove 1 data point every 12 months

y_abrupt_1_na <- y_abrupt

for (i in 1:20){
  random_point <- sample(1:12,1)
  annual_point <- ((i-1)*12+random_point)
  y_abrupt_1_na[annual_point] <- NA
}

plot(y_abrupt_1_na,type='l')
stl(ts(y_abrupt_1_na,frequency=12),s.window='periodic')


#Linearly interpolate

y_interpolate <- na.approx(y_abrupt_1_na)

plot(y_interpolate, type='l')
lines(y_abrupt_1_na, col='red')


#Alternative method - Savitzky and Golay filter




#Use chngpt



fit=chngptm (formula.1=V3_BioV3B1, formula.2=NAb_score, dat.mtct.2, type="segmented", family="gaussian")
summary(fit)


x <- 1:240
y <- sin(2*pi*x/12)


