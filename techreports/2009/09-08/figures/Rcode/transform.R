library('Cairo')

## A noisy sine wave as query
idx<-seq(0,6.28,len=30);
query<-(sin(idx)+runif(38)/2)/5;

## A cosine is for template; sin and cos are offset by 25 samples
template<-cos(idx)

## init graphics and plot both: query and template
par(mfrow=c(1,3))

plot(NULL, ylim=c(-1,5), xlim=c(0, 60), ylab="Values", main="Time series before transformation")
lines(cbind(c(30:60),template+4), col="brown2", lwd=3)
lines(query,col="blue3", lwd=3)

plot(NULL, ylim=c(-1,5), xlim=c(0, 60), ylab="Values", main="Scaling (x5) and 5-day moving average applied")
lines(cbind(c(30:60),template+4), col="brown2", lwd=3)
lines(query,col="blue4", lwd=1)
lines(filter(query*5, rep(1/5,5), sides=1),col="blue3", lwd=3)


plot(NULL, ylim=c(-1,5), xlim=c(0, 60), ylab="Values", main="Scaling (x5), 5-day moving average and shifting applied")
lines(cbind(c(30:60),template+4), col="gray", lwd=15)
lines(cbind(c(30:60),template+4), col="brown2", lwd=3)
lines(query,col="blue4", lwd=1)
lines(cbind(c(25:63),(filter(query*5, rep(1/5,5), sides=1)+3.7)[5:34]),col="blue3", lwd=3)

