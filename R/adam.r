## self-sample power calc
## 24th March 2025

# Models ------------------------------------------------------------------
## Pre-menopausal mean and sd for the distribution of FSH given age FSH is 
## measured:

## Model2: r1^2 ~ alpha2 + beta2*Age
## mean1 = mean(FSH) (by age)
## Model3: log(mean1) ~ alpha3 + beta3*Age
## sd(log(fsh)) = sqrt(alpha2 +beta2*Age)
## Post-menopausal mean and sd for the distribution of FSH given age FSH is 
## measured:
## Model4: log(FSH) ~ alpha4 + beta4*Age
## Model4 residuals: r2
## Model5: r2^2 ~ alpha5 + beta5*Age
## mean2 = mean(FSH) (by age)
## Model6: log(mean2) ~ alpha6 + beta6*Age
## sd(log(fsh)) = sqrt(alpha5 +beta5*Age)


## fsh model parameters - see above for description

## measured:
## Model1: log(FSH) ~ alpha1 + beta1*Age
## Model1 residuals: r1
## Model2: r1^2 ~ alpha2 + beta2*Age
## mean1 = mean(FSH) (by age)
## Model3: log(mean1) ~ alpha3 + beta3*Age
## sd(log(fsh)) = sqrt(alpha2 +beta2*Age)


##pre-menop
alpha2 = -1.15123373;
beta2 = 0.03330157;
alpha3 = 0.60678151;
beta3 = 0.05137627
##post-menop
alpha5 = 1.8028357; 
beta5 = -0.02719853;
alpha6 = 2.69220927;
beta6 = 0.03432768

age<-47
mymnsd1<-c(alpha3 +beta3*age, sqrt(alpha2 +beta2*age))
mymnsd1b<-mymnsd1/mymnsd1[2]

age<-53
mymnsd2<-c(alpha6 +beta6*age, sqrt(alpha5 +beta5*age))
mymnsd2b<-mymnsd2/mymnsd2[2]

mymnsd2b[1]-mymnsd1b[1]

pnorm(2.8*0.75/sqrt(2))

power.t.test(n=25,sd=1, power=0.9)

##################

mymn<-sapply(40:60,function(age) c(alpha3 +beta3*age, sqrt(alpha2 +beta2*age), alpha6 +beta6*age, sqrt(alpha5 +beta5*age)))


myz<-(mymn[3,]-mymn[1,])/ sqrt(mymn[2,]^2 + mymn[4,]^2)
myz<-(mymn[3,]-mymn[1,])/ sqrt(mymn[2,]^2 + mymn[4,]^2)

cbind(myz,pnorm(myz))

##check sims
library("pROC")
myn<-100000
mycase<-rnorm(myn,mymn[3,10], mymn[4,10])
myctl<-rnorm(myn,mymn[1,10], mymn[2,10])
mypredictor<-c(mycase, myctl)
myy<-rep(c(1,0), each=myn)
auc(myy, mypredictor)
