rm(list = ls())

install.packages("bayesm")

library(bayesm)
data(margarine)

# Problem 1
# Record the data
install.packages("csv")
library(csv)
data1<- margarine$choicePrice
write.csv(data1, "/Users/celiahober/Desktop/ECON613/data1.csv")
colnames(data1)
unique(data1$hhid)
length(unique(data1$hhid))
data2<- margarine$demos
write.csv(data2, "/Users/celiahober/Desktop/ECON613/data2.csv")
data<-merge(data1,data2,by="hhid")
## Average and Dispersion of product characteristics
average<-apply(data1[,3:12],2,mean)
dispersion<-apply(data1[,3:12],2,sd)
print(average)
print(dispersion)
# Market share by product characteristics#
freq<-table(data1$choice)
View(freq)
marketshare<- as.vector(freq/4470)
names(marketshare)<-c("p1","p2","p3","p4","p5","p6","p7","p8","p9","p10")
print(marketshare)
# Market share by brands
marketshareppk<-sum(freq[1],freq[7])/4470
marketsharepbb<-freq[2]/4470
marketsharepfi<-sum(freq[3],freq[8])/4470
marketsharephse<-sum(freq[4],freq[10])/4470
marketsharepgen<-freq[5]/4470
marketshareplmp<-freq[6]/4470
marketsharepss<-freq[9]/4470
brandshare<-cbind(marketshareppk,marketsharepbb,marketsharepfi,marketsharephse,marketsharepgen,marketshareplmp,marketsharepss)
print(brandshare)
# Market share by forms
stk<-sum(freq[1:6])/4470
tub<-sum(freq[7:10])/4470
formshare<-cbind(stk,tub)
print(formshare)
# Mapping with the observed attributes
Freqs <-table(data$Income, data$choice)
Freqs[1,]
class(Freqs)
Sum<-table(data$Income)
Sum[1]
prob<- matrix(0,nrow=14,ncol=10) 
for ( i in 1:14){ 
  prob[i,]<-Freqs[i,]/Sum[i]
  
}
View(prob)
rownames(prob)<-c("Income2.5","Income7.5","Income12.5","Income17.5","Income22.5","Income27.5","Income32.5","Income37.5","Income42.5","Income47.5","Income55","Income67.5","Income87.5","Income130")
colnames(prob) <- colnames(data1[,3:12])
View(prob)

# Problem 2
# Apply the conditional logit model#
# manipulate x by substracting the first column#
data1<- margarine$choicePrice
x<-data1[,3:12]- data1[,3]
x<-as.matrix(x)
decision<-matrix(0,nrow = 4470,ncol = 10)
for (i in 1:10) {
  decision[,i]<-cbind(as.numeric(data1$choice==i))
}
View(decision)
# Loglikelihood
logit.loglk <- function (beta, x) {
  A<-matrix(rep(beta[2:10],each=4470),nrow=4470,ncol=9)
  A<-cbind(0,A)
  v<- x * beta[1] + A 
  
  sum<-rowSums(exp(v))
  lgprob<-log(exp(v)/sum)
  lgprobnew<- lgprob * decision
  loglk<- sum(lgprobnew)
  return(-loglk)
} 

beta <- rep(-0.5,10) 
optimLogit <- optim(beta, logit.loglk, x=x)
optimLogit
logitparameter<-optimLogit$par
print(logitparameter)

# Explanation: if relative price goes up by 1, then utility of choosing alternative compared with product 1 will go down by 6.33.

# Problem 3
# Apply the multinomial logit model#
I<-data$Income
I<- replicate(10, I)
# Loglikelihood
multilogit<- function(beta,I){
  A<-matrix(rep(beta[10:18],each=4470),nrow=4470,ncol=9)
  A<-cbind(0,A)
  B<- matrix(rep(beta[1:9],each=4470),nrow = 4470,ncol=9)
  B<-cbind(0,B)
  V<-I * B + A
  sum<-rowSums(exp(V))
  prob<- exp(V)/sum
  logprob<-log(prob)
  lgprobnew<- logprob * decision
  loglk<- sum(lgprobnew)
  return(-loglk)
} 

beta <- rep(0,18) 
optimLogit <- optim(beta, multilogit, I=I)
multilogitparameter<-optimLogit$par
print(multilogitparameter)

# Explanation: The betajs are all negative, so if income goes up, relative utilities of alternative will go down, and individual is more willing to choose product 1.

# Problem 4
#marginal effect of the conditional logit model#
A1<-matrix(rep(logitparameter[2:10],each=10),nrow=10,ncol=9)
A1<-cbind(0,A1)
data1<- margarine$choicePrice
x<-data1[,3:12]- data1[,3]
x<-as.matrix(x)
xmean<-colMeans(x, na.rm = FALSE, dims = 1)
View(xmean)
xmean<-matrix(rep(xmean,each=10),nrow=10, ncol=10) 
View(xmean)
logitparameter[1]
vmean<- xmean * logitparameter[1]+ A1
summean<-rowSums(exp(vmean))
probmean<-exp(vmean)/summean
View(probmean)
E<- diag(10)
mf<- t(probmean) * (E-probmean) * logitparameter[1]
View(mf)

# marginal effect of the average for the multinomial logit model#
#calculate the probability#
multilogitparameter
A<-matrix(rep(multilogitparameter[10:18],each=4470),nrow=4470,ncol=9)
A<-cbind(0,A)
B<- matrix(rep(multilogitparameter[1:9],each=4470),nrow = 4470,ncol=9)
B<-cbind(0,B)
dim(B)
V<-I * B + A
View(V)
P<-exp(V)/rowSums(exp(V))
dim(P)
multibeta<-as.vector(c(0,multilogitparameter[1:9]))
View(multibeta)
betamean<- as.numeric(P %*% multibeta)
betamean<-replicate(10,betamean)
View(betamean)
dim(betamean)
mf<- P * (B-betamean)
dim(mf)
View(mf)
averagemf<-colMeans(mf,na.rm = FALSE, dims = 1)
View(averagemf)

# Problem 5
# the income level#
I<-data$Income
View(I)
I<- replicate(10, I)
# the relative price specific#
data1<- margarine$choicePrice
x<-data1[,3:12] - data1[,3]
x<-as.matrix(x)
#consider the decison matrix
decision<-matrix(0,nrow = 4470,ncol = 10)
for (i in 1:10) {
  decision[,i]<-cbind(as.numeric(data1$choice==i))
}
View(decision)

# the mix logit Likelihood #
mix.logit<- function(beta,x,I){
  A<-matrix(rep(beta[11:19],each=4470),nrow=4470,ncol=9)
  A<-cbind(0,A)
  B<- matrix(rep(beta[2:10],each=4470),nrow = 4470,ncol=9)
  B<-cbind(0,B)
  V<- x * beta[1]+ I * B + A
  P<-exp(V)/rowSums(exp(V))
  lgprob<-log(P)
  lgprobnew<- lgprob * decision
  loglk1<- sum(lgprobnew)
  return(-loglk1)
} 

beta <- rep(-0.5,19) 
optimLogit1 <- optim(beta, mix.logit, I=I,x=x)
#report the mixparameterf#
mixparameterf<-optimLogit1$par

# the alternative specification
I<-data$Income
Ir<-replicate(9,I)
data1<- margarine$choicePrice
xr<-data1[,3:11] - data1[,3]
# the decision matrix#
decision<-matrix(0,nrow = 4470,ncol = 9)
for (i in 1:9) {
  decision[,i]<-cbind(as.numeric(data1$choice==i))
}
View(decision)
# the mix logit likelihood#
mix.logit<- function(beta,xr,Ir){
  A1<-matrix(rep(beta[10:17],each=4470),nrow=4470,ncol=8)
  A1<-cbind(0,A1)
  B1<- matrix(rep(beta[2:9],each=4470),nrow = 4470,ncol=8)
  B1<-cbind(0,B1)
  dim(B1)
  V<- xr * beta[1]+Ir * B1 + A1
  P<-exp(V)/rowSums(exp(V))
  lgprob<-log(P)
  lgprobnew<- lgprob * decision
  loglk2<- sum(lgprobnew)
  return(-loglk2)
}  
par2 <- rep(0,17) 
optimLogit2 <- optim(par2, mix.logit, Ir=Ir,xr=xr)
#report the parameter of restricted model
mixparameterr<-optimLogit2$par
#report the likelihood of the restricted model
lr<-optimLogit2$value
#report the likelihood of the unresitrcted model using the value of beta from 1:9
parameter<-c(mixparameterf[1:9],mixparameterf[11:18])
lf<-mix.logit(parameter,xr,Ir)
#calculate the MTT
MTT<- 2* (lf-lr)
print(MTT)
qchisq(.95, df=17)
# Comparing the results, we reject the null hypothesis. It indicates that all chocies need to be specified.