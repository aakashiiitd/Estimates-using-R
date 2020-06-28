rm(list=ls())

###part a
n<-1000
y.norm<-rnorm(n, mean = 0, sd = 1)
norm.m<-mean(y.norm) ###  method of moment estimator for mean
norm.v<-mean(y.norm*y.norm)
norm.var<-(norm.v-(norm.m*norm.m)) #method of moment estimator for variance
print(norm.m)
print(norm.var)

#### Part b
m<-c(0.2,0.3)
ml = nlminb(start=m,objective=function(mle)
{
  loglikelihood.norm<-(((n/2)*log(2*(22/7))) + ((n/2)*log(mle[2]*mle[2])) + ((1/(2*mle[2]*mle[2]))*sum((y.norm-mle[1])*(y.norm-mle[1]))))
  return(loglikelihood.norm)
})
print(ml[1])

###part c
norm.std<-1
li<-c()
norm.values<-seq(-3,3,by=0.15) ### generating a sequence
for(i in norm.values){
  loglikelihood.norm<-(((n/2)*log(2*(22/7))) + ((n/2)*log(norm.std*norm.std)) + ((1/(2*norm.std*norm.std))*sum((y.norm-i)*(y.norm-i))))
  li<-c(li,-loglikelihood.norm)
}
plot(norm.values,li)