rm(list=ls())
###Given: p=0.6,n=1000

###Part a
sampling<-seq(1,1000,by=1)
y.geom<-rgeom(1000,prob=0.6)
mlle=1000/(sum(y.geom)+1000)
loglikelihood.geom<-(1000*log(mlle)) + (sum(y.geom)*log(1-mlle))
print(mlle)
print(loglikelihood.geom)

###Part b
loglikvalue.geom<-function(theta.geom,x)
{
  loglikelihood.geom<-(1000*log(theta.geom)) + (sum(x)*log(1-theta.geom))
  return(loglikelihood.geom)
}

theta.values<-seq(0,1,by=0.05) ### generating a sequence of lambda
ll.geom<-loglikvalue.geom(theta.values,y.geom)
data.geom<-data.frame(cbind(theta.values,ll.geom))
plot(data.geom[,1],data.geom[,2],xlab="",ylab="")

