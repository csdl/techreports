##
##
##
library(dtw)
par(mfrow=c(1,3), cex.main=1.5)
##
idx       <- seq(-2,4,len=250);
query     <- sin(idx*1.5)*atan(idx*1.3);
reference <- cos(-0.2+idx*1.6)*atan(idx*1.3);
plot(reference, type="l", lwd=2, col="red", main="Time-series before alignment",
ylab="values", xlab="time"); 
lines(query,col="blue", lwd=2);

## Find the best match
alignment<-dtw(query,reference,keep=TRUE);
ticks <- seq(-2, 4, 0.5)            
ticks <- round((ticks+2)*41)
hw <- (alignment$index1 %in% ticks)   # where are they on the w. curve?
hi <- (1:length(alignment$index1))[hw];   # get the indices of TRUE elems
#dtwPlotThreeWay(alignment,match.indices=hi,
# xlab="Query index",ylab="Reference index",main="Timeseries alignment",);

dtwPlotDensity(alignment,
  main="DTW-based time-series alignment cost matrix")

#plot(reference)
#lines(query[alignment$index1]~alignment$index2,col="blue")

## Plot the (unwarped) query and the inverse-warped reference
plot(query,type="l",col="blue", main="Aligned time-series")
lines(reference[alignment$index2]~alignment$index1, col="red")


