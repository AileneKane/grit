################################
### GRIT HWB data simulation ###
#### Started April 14, 2022 ####
###### By Ailene Ettinger ######
### ailene.ettinger@tnc.org ####
################################
#helpful website for simulating: 
#https://aosmith.rbind.io/2018/04/23/simulate-simulate-part-2/#simulate-simulate-dance-to-the-music
rm(list=ls()) 
options(stringsAsFactors = FALSE)

# libraries
library(dplyr)
library(lme4)
library(scales)
#set working directory
setwd("~/GitHub/grit")

###Below code from https://stats.stackexchange.com/questions/374413/how-to-simulate-likert-scale-data-in-r
set.seed(8649)     # this makes the example exactly reproducible
N      = 10        # this is how much data I'll generate
latent = rnorm(N)  # this is the actual latent variable I want to be measureing

##### generate latent responses to items
item1 = latent + rnorm(N, mean=0, sd=0.2)  # the strongest/least noisy correlate
item2 = latent + rnorm(N, mean=0, sd=0.3)
item3 = latent + rnorm(N, mean=0, sd=0.5)
item4 = latent + rnorm(N, mean=0, sd=1.0)
item5 = latent + rnorm(N, mean=0, sd=1.2)  # the weakest/noisiest

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


#An alternative approach from the same exchnage:One way to generate Likert data is according to a proportional odds model. Here, the underlying distribution of the (latent) response is a logistic random variable with a center ?? that can vary as a function of one or more predictors. The latent variable is then thresholded into as many categories with an arbitrary number of cutpoints. Achieving a target number of response categories is quite difficult, either requiring advanced math or ( more likely) play it by ear.
set.seed(123)
n <- 1e6
beta <- 0.3
alpha <- sort(rnorm(5))
x <- seq(-3, 3, length.out = n)
z <- rlogis(n, beta*x)
y <- factor(findInterval(z, alpha))
library(MASS)
fit <- polr(formula = y ~ x)
Generates the association:
  
  > coef(fit)
x 
0.2982142 
