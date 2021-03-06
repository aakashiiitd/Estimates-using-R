rm(list=ls())
###Given
values<-c(10.4, 19.6, 18.8, 13.9, 17.8, 16.8, 21.6, 17.9,12.5, 11.1, 4.9, 12.8, 14.8, 22.8, 20.0,
         15.9,16.3, 13.4,17.1,14.5, 19.0, 22.8, 1.3, 0.7, 8.9, 11.9, 10.9, 7.3, 5.9, 3.7, 17.9, 19.2,
         9.8, 5.8, 6.9, 2.6, 5.8, 21.7, 11.8, 3.4, 2.1, 4.5, 6.3, 10.7, 8.9, 9.4, 9.4, 7.6, 10.0, 3.3,
         6.7, 7.8, 11.6, 13.8, 18.6)
values<-sort(values)
###part a
#answer is highest ordered X
print(values[length(values)])

###part b

loglikvalue.unif<-function(std.unif)
{
  loglikelihood.unif<-(-dunif(values,0,std.unif,log=T))
  return(loglikelihood.unif)
}

#lambda.values<-seq(20,30,by=0.1) ### generating a sequence of lambda
ll.unif<-loglikvalue.unif(values)

data.unif<-data.frame(cbind(values,ll.unif))
plot(data.unif[,1],data.unif[,2],xlab="Values",ylab="Computed_Values")