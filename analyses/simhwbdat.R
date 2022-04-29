################################
### GRIT HWB data simulation ###
#### Started April 14, 2022 ####
###### By Ailene Ettinger ######
### ailene.ettinger@tnc.org ####
################################
#helpful website for simulating: 
#https://aosmith.rbind.io/2018/04/23/simulate-simulate-part-2/#simulate-simulate-dance-to-the-music
#helpful for ordinal logistic regression: https://stats.oarc.ucla.edu/r/dae/ordinal-logistic-regression/
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(dplyr)
library(lme4)
library(scales)
library(MASS)
#set working directory
setwd("~/GitHub/grit")

###Below code from https://stats.stackexchange.com/questions/374413/how-to-simulate-likert-scale-data-in-r
set.seed(8649)     # this makes the example exactly reproducible
N      = 10      # this is how much data I'll generate
latent = rnorm(N)  # this is the actual latent variable I want to be measureing
strongeff<-.9
weakeff<-.1
##### generate latent responses to items
item1 = latent + rnorm(N, mean=0, sd=0.2)  # the least noisy correlate
item2 = latent + rnorm(N, mean=0, sd=0.3)
item3 = latent + rnorm(N, mean=0, sd=0.5)
item4 = latent + rnorm(N, mean=0, sd=1.0)
item5 = latent + rnorm(N, mean=0, sd=1.2)  # the noisiest

##### convert latent responses to ordered categories
item1 = findInterval(item1, vec=c(-Inf,-2.5,-1, 1,2.5,Inf))  # fairly unbiased
item2 = findInterval(item2, vec=c(-Inf,-2.5,-1, 1,2.5,Inf))
item3 = findInterval(item3, vec=c(-Inf,-3,  -2, 2,3,  Inf))  # middle values typical
item4 = findInterval(item4, vec=c(-Inf,-3,  -2, 2,3,  Inf))
item5 = findInterval(item5, vec=c(-Inf,-3.5,-3,-1,0.5,Inf))  # high ratings typical

##### combined into final scale
manifest = round(rowMeans(cbind(item1, item2, item3, item4, item5)), 1)
manifest
# [1]  3.4  3.6  3.4  3.8  2.6  3.4  3.2  2.0  3.8  3.2
round(latent, 1)
# [1]  1.3  0.6  0.2  1.0 -1.5  0.1  0.4 -2.5  2.3 -0.3
cor(manifest, latent)
# [1] 0.9280074

#An alternative approach from the same exchnage:One way to generate Likert data is according to a proportional odds model. 
#Here, the underlying distribution of the (latent) response is a logistic random variable with a 
#enter ?? that can vary as a function of one or more predictors. 
#The latent variable is then thresholded into as many categories with an arbitrary number of cutpoints. 
#Achieving a target number of response categories is quite difficult, either requiring advanced math or ( more likely) play it by ear.
#true pop:


beta<-.5#effect size
var <- c(.1,.5,2.5)#variation
#From greg's study (https://www.pnas.org/doi/10.1073/pnas.1510459112), his mean to std dev are
#mean change =???2.33, SE = 0.55, SD=2.4 (so effect size = to var)

png("analyses/figs/simdat.png", width=1600,height=1600, res=200)

par(mfrow=c(length(var),4))

for(j in 1:length(var)){
set.seed(123)

N <- 10000

alpha <- sort(rnorm(4))#5 categories for most
x <- seq(-3, 3, length.out = N)
z <- rlogis(N, beta*x)
noise<- rnorm(N, mean=0, sd=var[j])
z<-z+noise
y <- factor(findInterval(z, alpha)+1)#factors on a scale of 1 to 5

pop<-as.data.frame(cbind(x,y))
#now take a sample of the population (pop)
#sample
n<-c(50,100,250,500)#samplesizes to test
cols=c("darkred","goldenrod","darkgreen","darkblue")
for(i in 1:length(n)){
dat<-pop[sample(1:N, n[i], replace=F),]#randomly sample n times from the population
dat$y<-as.factor(dat$y)
fit <- polr(formula = y ~ x, dat=dat)#ordered logistic or probit regreassion
#Generates the association:
coef(fit)#0.2982142 

#plot true population

plot(x,z,type="p", xlab="distance to greening",ylab="response",bty="l",cex=.9, pch=16,col=alpha("gray",alpha=.5), main=paste("Sample size = ",n[i],",SD = ",var[j],sep=""), cex.main=1.1)
abline(a=0,b=beta, lwd=2)

points(dat$x,dat$y,pch=16, col=alpha(cols[i],alpha=.5), cex=1)
abline(a=0,b=coef(fit), lwd=1,col=cols[i])
cis<-round(confint(fit), digits=2)
mtext(paste("Est. eff. size = ",cis[1]," - ",cis[2], sep=""), cex=.8,col=cols[i])
}
mtext(paste("True effect size = ", beta, sep=""),                   # Add main title
      side = 3,
      line = - 1,
      outer = TRUE)
}
dev.off()
