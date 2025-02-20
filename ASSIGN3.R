#pbinom Q1
n<-12
p<-1/6
cat(pbinom(9,n,p)-pbinom(6,n,p),"\n")

#Q2 normal distribution
z<-((84-72)/15.2)
less_84<-pnorm(z)
cat((1-less_84)*100,"\n")

#Q3 Poisson 
P_X_0<-exp(-5)
cat(P_X_0,"\n")

P_X<-(((exp(-50)*(50^48))/factorial(48))+((exp(-50)*(50^49))/factorial(49))+((exp(-50)*(50^50))/factorial(50)))
cat(P_X,"\n")

#Q4 hypergeometric
n<-250
sample<-17
def<-5  
s<-3
cat(((choose(17,3))*(choose(233,2)))/choose(250,5))

#Q5 dbinom prob mass  pbinom cum mass
p<-0.447
n<-31
cat("x distributed as",n,"  ",p,"\n")

x<-c(0:n)
plot(0:n,dbinom(x,n,p),type="h",col="blue",xlab="x",ylab="y",main="histogram")
plot(0:n,pbinom(x,n,p),type="s",col="blue",xlab="x",ylab="y",main="s")
mean<-n*p
var<-n*p*(1-p)
sd<-sqrt(var)
cat(mean," ",var," ",sd,"\n")
