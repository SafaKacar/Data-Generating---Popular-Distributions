
#
##########################################################################

###  1: Exponential distribution

# Set up parameters
lambda = 2
n = 1000 # sample size
#Generate random variables
u = runif(n)
x = -log(u)/lambda
# Plot the histogram of x
hist(x, prob=TRUE, main ="Exponential(?)")
# Add theoritical density line
y = seq(0,5,0.1)
lines(y,lambda*exp(-lambda*y),col="red")
# Calculate the sample mean and variance 
mean(x)
var(x)

###########################################################################

###  2: Pareto distribution

# Set up parameters
  n = 10000
  a = 3   
  b = 2
#Generate random variables
  u = runif(n)
  x = b*(1-u)^(-1/a)
  hist(x, prob = TRUE, main ="Pareto(2,2)")
  summary(x)
  y <- seq(2, 40, 0.01)
  lines(y, a*b^a/y^(a+1),col="red")


##############################################################################

###  3: Uniform Distribution

uni = function(a,b,n)
{
  u = runif(n)
  x = u*(b-a)+a
  return(x)
}
y = uni(2,5,10000)
hist(y, prob = TRUE, main ="Uniform(a,b)")

###########################################################################

###  4: Discrete random variable
      n = 1000
# Set up storage space for the generated variables.
   x = numeric(n)
# These are the probability masses.
   prob = c(0.3, 0.2, 0.5)
# These are the x’s in the domain.
  d = c(0:2) 
# Generate n rv’s from the desired distribution.
   for (i in 1:n){
	u = runif(1)
      if(u<=prob[1])
        x[i] = d[1]
      else if( u<=sum(prob[1:2]))
        x[i] = d[2]
      else 
        x[i] <- d[3]
   }
# In order to confirm, we look at the relative frequency of each x.
# For this, find the proportion of each number.
  x0 = length(which(x==0))/n
  x1 = length(which(x==1))/n
  x2 = length(which(x==2))/n

  est.prob = c(x0,x1,x2)
  est.prob

  theorical.mean = sum(prob*d)
  est.mean = mean(x)
  theorical.var = sum(prob*d^2)-(sum(prob*d))^2
  est.var = var(x)

theorical.mean 
est.mean 
theorical.var 
est.var


##############################################################################

###  5: Geometric Distribution

n = 1000
p = 0.25
u = runif(n)
x = ceiling(log(1-u)/log(1-p))-1
hist(x, prob=TRUE, main ="Geometric Distribution with p=0.25")
# Add theoritical density line
y = seq(0,20,0.1)
lines(y,p*(1-p)^y,col="red")
mean(x)

################################################################################
#
#       PART-2 
#
##############################################################################################

###   1: Gamma Distribution 
 
n = 1000
t = 3
lam = 2
U = matrix(runif(n*t),t, n)
logU = -log(U) / lam
X = colSums(logU)
hist(X, prob=TRUE, main ="Gamma Distribution (3,2)")
y = seq(0,6,0.01)
lines(y,dgamma(y,3,2),col="red")

###########################################################################################

###  2: Beta Distribution

 n = 1000
 a = 3
 b = 2

 u = rgamma(n, shape=a, rate=1)
 v = rgamma(n, shape=b, rate=1)
 x = u/(u+v)

 q = qbeta(ppoints(n), a, b)
 qqplot(q, x, cex=0.25, xlab="Beta(3,2)", ylab="Sample")
 abline(0, 1)

############################################################################################

###  3: Chi-Square Distribution

n = 1000
v = 2
Z = matrix(rnorm(v*n,0,1),v, n)
SquaredZ = Z^2
X=colSums(SquaredZ)
hist(X, prob=TRUE, main ="Chi-Square Distribution")
y = seq(0,15,0.01)
lines(y,dchisq(y,v),col="red")

#######################################################################################

###  4: Beta Distribution

n = 5000
a = 1
b = 2
X =numeric(n)

for(i in 1:n){
    u=runif(a+b-1)
    sortu=sort(u)
    X[i]=sortu[a]
}
hist(X, prob = TRUE, main ="Beta(1,2)")
y=seq(0,1,0.01)                  
lines(y,dbeta(y,1,2),col="red")
#To find the first and second sample moments:
average=mean(X)
variance=var(X)

#############################################################################
#
#       PART-3 
#
################################################################################

###  1: 

    n = 1000
    k = 0      # counter for accepted
    j = 0      # number of iterations
    y = numeric(n)
    while (k < n) {
        u = runif(1)
        j = j + 1
        x = runif(1)  # random variate from g
        if (u < x^2) {
            # we accept x
            k = k + 1
            y[k] = x
        }
    }
    j 
 hist(y, prob = TRUE, main ="f(x)=3x^2")
 a = seq(0,1,0.01)
 lines(a,3*a^2,col="red")
 summary(y)

################################################################################

###  2: Beta Distribution

 n = 1000
    k = 0      #counter for accepted
    j = 0      #iterations
    y =numeric(n)

    while (k < n) {
        u = runif(1)
        j = j + 1
        x = runif(1)  #random variate from g
        if (x * (1-x) > u) {
            # we accept x
            k = k + 1
            y[k] = x
        }
    }
    j
  hist(y, prob = TRUE, main ="Beta(2,2)")
  a = seq(0,1,0.01)
  lines(a,dbeta(a,2,2,),col="red")
 summary(y)


################################################################################

###  3: 

  n = 10000
  prob = c(0.15, 0.22, 0.33, 0.10, 0.20) # probabilities for desired density
  c= 1.65
  x =  numeric(n) # for random variates
  xy =  numeric(n) # corresponding y values: u*c*q(i)
  rejx =  numeric(n) # for rejected variates
  rejxy =  numeric(n) # corresponding y values: u*c*q(i)
  irv = 1 # initialization of index for r.v’s
  irej = 1 # initialization of index for rejected r.v’s
 
 while(irv <= n){
       y = sample(1:5, 1, replace = TRUE)  # random number for q(i)
      u = runif(1) # random number for comparison
      if (u<=prob[y]/0.33)
     {
             x[irv] = y
             xy[irv] = u*0.33
             irv = irv+1
      }
      else {
              rejx[irej] = y
              rejxy[irej] = u*0.33
              irej = irej+1
             }
 }
plot(x, xy, type="p", col="blue",ylim=c(0,1))
points(rejx, rejxy,col="red") 

x1 = length(which(x==1))/n
x2 = length(which(x==2))/n
x3 = length(which(x==3))/n
x4 = length(which(x==4))/n
x5 = length(which(x==5))/n

est.prob = c(x1, x2, x3, x4, x5)
est.prob
##############################################################################################

#
#   PART 4
# 
#########################################################################

#  1:

n = 10000  # number of iterations
a = 0	    # initialization of the bounds of integral
b = 1

x = runif(n,a,b)            # generate n uniform random numbers between a and b 
f = sqrt(x+sqrt(x))         # find the function valu of x's
estimate = mean(f)    # estimate the value of integral
estimate

fi = function(y){
    r = sqrt(y+sqrt(y))}
integrate(fi,a,b)

#########################################################################

#  2:

n = 10000
a = 0	    
b = 1
x = runif(n,a,b) 
f = x^3        
estimate = mean(f)    
estimate

fi = function(y){
    r = y^3}    
integrate(fi,a,b)

#########################################################################

#  3:

n = 10000
a = 1	    
b = 3
x = runif(n,a,b) 
f = x^2+x        
estimate = (b-a)*mean(f)    
estimate

fi = function(y){
    r = y^2+y}
integrate(fi,a,b)

#########################################################################

#  4: Example 5.3 in text book.

x = seq(0.1, 2.5, length=10)
m = 10000
u = runif(m)
cdf = numeric(length(x))
for (i in 1:length(x)){
      g = x[i]*exp(-(u*x[i])^2/2)
    cdf[i] = mean(g) / sqrt(2*pi) + 0.5
}
Phi = pnorm(x)
print(round(rbind(x, cdf, Phi),3))


########################################################################
#
#       PART-5 
#
################################################################################

###  1: 

n = 10000 # number of iterations.
a = 0     # initilization of the bounds of integral.
b = 1
 x = runif(n,a,b)  # generate n uniform random numbers between a and b.
f = 4*sqrt(1-x^2)  # find the function value of x's.
area = (b-a)*mean(f)  # estimated value of integral.
area 

################################################################################

###  2:

n = 10000
a = 0     
b = pi/3
x = runif(n,a,b)
f = sin(x)
result = (b-a)*mean(f)
result

################################################################################

###  3:

n = 100000
a = 0     
b = 1
x = runif(n,a,b)
f = sqrt((x+1)*x*(x-1)*(x-2))
result = (b-a)*mean(f)
result

c<-seq(-1,1,0.001)
plot(c,sqrt((c+1)*c*(c-1)*(c-2)),main="sqrt((x+1)*x*(x-1)*(x-2))")

fi = function(x){
	sqrt((x+1)*x*(x-1)*(x-2))}
integrate(fi,a,b)


################################################################################

###  4:

n = 10000
alpha =1
beta = 1
u = runif(n)
x = beta*(-log(u))^(1/alpha)
par(mfrow=c(2,2))
 hist(x, prob = TRUE, main ="Weibull Distribution(1,1)")

##############################################################################
#
#   PART 6
# 
#########################################################################

#  1:

n = 10000 # number of iteration
t=0 # counter 

for(i in 1:n){
x = runif(10)   # generate 10 random number from uniform dist.
index = which(x<=0.5) # determine which generated number is less than 0.5 (head)
y = length(index)    # count the head coins
if(y==3|y==6|y==9)   # if the number of head coins is 3 ,6 or 9, increment couter t
t=t+1
}
est.prob = t/n   # estimated probability
est.prob

#########################################################################

#  2:

n = 100000
y =numeric()
for(i in 1:n){
x = sample(c(1:6),2,replace=TRUE)
y[i] = sum(x)
}
round(table(y)/n,3)


#########################################################################

#  3:

m = 10000
n = 100
sig = 3
y = numeric()
for(i in 1:m){
x =rnorm(n,0,sig) 
y[i] = mean(x) 
}
se = sig/sqrt(n)
se
est.mean = mean(y)
est.std = sd(y)
est.mean
est.std


#########################################################################

#  4:

n = 5000               
Y1 = numeric(n)     
lamda = 2             
u=runif(n)         
Y1=-log(u)/(n*lamda)
hist(Y1, prob = TRUE)

###############################################################################

#       PART 7 

###############################################################################

#  1:

T_1 = 3                  # true value of location parameter.
T_2 = 5                  # true value of scale parameter. 
n = c(5, 30, 100)    # sample sizes 
M=5000                 # number of iterations (samples). 
Bias_1 = numeric(3) # creating vector for bias of the est. of location par. (for different n)                                                     
Bias_2 = numeric(3) # creating vector for bias of the est. of scale par. (for different n)                                                      for(j in 1:3){
T1 = numeric(3)                                                    
T2 = numeric(3)
for(j in 1:3){    
    for(i in 1:M){   
        data = matrix(0,M,n[j])    # creating zeros vector for the samples
        data[i,] = rnorm(n[j],T_1,sqrt(T_2))  # generating n random numbers coming from the      
                                                                    # normal dist. with true parameters.
        T1[i] = mean(data[i,])   # calculating T_1 estimator for ith data.
        T2[i] = var(data[i,])      # calculating T_2 estimator for ith data.
    }
   Bias_1[j] = mean(T1)-T_1     # calculating bias for T_1 estimator for jth sample size.
   Bias_2[j] = mean(T2)-T_2    # calculating bias for T_2 estimator for jth sample size.     
}
Bias_1
Bias_2


#########################################################################
###  2: Hypothesis testing

mcdata = read.table("mcdata.txt") 
mcdata =as.vector(mcdata$V1)
n = length(mcdata)
sigma = 7.8
sigxbar = sigma / sqrt(n)
Tobs = (mean(mcdata) - 454) / sigxbar
qqnorm(mcdata)
qqline(mcdata)
M = 1000
Tm = numeric(M)
for(i in 1:M){
      xs = rnorm(n, 454, sigma) 
     Tm[i] = (mean(xs) - 454) / sigxbar
}
alpha = 0.05
cv =  quantile(Tm,alpha)



##########################################################################################

###  3: Hypothesis testing – p-value

mcdata = read.table("mcdata.txt")  # read data
mcdata =as.vector(mcdata$V1)      # define data as a vector
n = length(mcdata)                          # sample size
sigma = 7.8                                     # population std. dev.
sigxbar = sigma / sqrt(n)                # sample std. dev. 
Tobs = mean(mcdata)                    # Observed value of the test statistic
M = 1000                                       # Number of Monte Carlo trials
Tm = numeric(M)                          # Storage for test statistics from the MC trials
for(i in 1:M){
      xs = rnorm(n, 454, sigma)       # Generate a random sample under H_0
     Tm[i] = mean(xs)
}
alpha = 0.05
ind = which(Tm <= Tobs)              # Get the p-value. Find all of the values from the simulation 
pvalhat = length(ind)/M              # that are below the observed value of the test statistic.

###################################################################################################

###  4: Hypothesis testing – Type I error

mcdata = read.table("mcdata.txt")  # read data
mcdata =as.vector(mcdata$V1)      # define data as a vector
n = length(mcdata)                          # sample size
mu = 454                                        # population mean
sigma = 7.8                                     # population std. dev.
sigxbar = sigma / sqrt(n)                # sample std. dev. 
Tobs = mean(mcdata)                    # Observed value of the test statistic
M = 1000                                       # Number of Monte Carlo trials
Tm = numeric(M)                          # Storage for test statistics from the MC trials
alpha = 0.05
cv =  qnorm(alpha,mu,sigxbar)              # Get the critical value for xbar
Im = 0
for(i in 1:M){
      xs = rnorm(n, 454, sigma)       # Generate a random sample under H_0
     Tm[i] = mean(xs) 
     if (Tm[i]<=cv)
        { Im = Im +1}
}
alphahat = Im / M
alphahat 

####################################################################################################

###  5: Hypothesis testing – Type II error

mcdata = read.table("mcdata.txt")  # read data
mcdata =as.vector(mcdata$V1)      # define data as a vector
n = length(mcdata)                          # sample size
mu = 454                                        # population mean
sigma = 7.8                                     # population std. dev.
sigxbar = sigma / sqrt(n)                # sample std. dev. 
Tobs = mean(mcdata)                    # Observed value of the test statistic
M = 1000                                       # Number of Monte Carlo trials
Tm = numeric(M)                          # Storage for test statistics from the MC trials
alpha = 0.05
cv =  qnorm(alpha,mu,sigxbar)              # Get the critical value for xbar
mualt = seq(445,456,1)
betahat = numeric(length(mualt))
for(j in 1: length(mualt)){
    Im = 0
    mu = mualt[j]
    for(i in 1:M){
         xs = rnorm(n, mu, sigma)       # Generate a random sample under H_0
         Tm[i] = mean(xs) 
         if (Tm[i] > cv)
          { Im = Im +1}
     }
betahat[j] = Im / M
}
powhat = 1-betahat
plot(mualt, powhat,type ="o",main="Power Curve")

###################################################################################################

#       PART 8 

###############################################################################

#  1: Bias of estimators

sigma = 3
 M = 1000
 n = 5
 s2.hat = rep(NA, M)
 s.hat = rep(NA, M)
 for (i in 1:M) {
 x = rnorm(n, 0, sigma)
 s2.hat[i] = 1/n * sum((x - mean(x))^2)
 s.hat[i] = sqrt(1/(n - 1) * sum((x - mean(x))^2))
 }
bias_var = mean(s2.hat - sigma^2)
se_var = sd(s2.hat - sigma^2)
bias_sd = mean(s.hat -sigma)
se_sd = sd(s.hat - sigma)


#########################################################################

###  2: Hypothesis testing

ks = seq(0,12)
counts = c(7,45,181,478,829,1112,1343,1033,670,286,104,24,3)
T1_obs = sum(ks*counts)/6115
T1s = numeric(1000)
for (i in 1:1000) {
X = rbinom(6115, 12, 0.5)
T1s[i] = mean(X)
}
mean(T1s)
T1_pvalue = length(which(T1s<T1_obs))/1000 * 2


################################################################################

###  3: Hypothesis testing 

shelf = c(108,124,124,106,115,138,163,159,134,139)
n = length(shelf)                          # sample size                        
sigxbar = sd(shelf) / sqrt(n)                # sample std. dev. 
Tobs = (mean(shelf) - 120) / sigxbar     # Observed value of the test statistic
qqnorm(shelf)                             # Normal probability plot
qqline(shelf)
sdev = sd(shelf)
M = 1000                                       # Number of Monte Carlo trials
Tm = numeric(M)                          # Storage for test statistics from the MC trials
for(i in 1:M){
      xs = rnorm(n, 120, sdev)       # Generate a random sample under H_0
     Tm[i] = (mean(xs) - 120) / sigxbar
}
alpha = 0.01
cv =  quantile(Tm,1-alpha)              # Get the critical value for alpha

ind = which(Tm >= Tobs)           # Get the p-value. Find all of the values from the simulation 
pvalhat = length(ind)/M      # that are below the observed value of the test statistic.


##########################################################################################

###  4: MSE

    n = 20
    m = 1000
    tmean = numeric(m)
    med = numeric(m)
    x_bar = numeric(m)
    for (i in 1:m) {
        x = sort(rnorm(n))
        tmean[i] = sum(x[2:(n-1)]) / (n-2)
	  med[i] = median(x)
	  x_bar[i] = mean(x)
        }
    mse_tmean <- var(tmean) + mean(tmean)^2
# or we can estimate mse by mean(tmean^2)
    mse_med <- var(med) + mean(med)^2
# or we can estimate mse by mean(med^2)
    mse_x_bar <- var(x_bar) + mean(x_bar)^2
# or we can estimate mse by mean(x_bar^2)
    cat('bias_tmean:', mean(tmean), "\n", 'bias_med:', mean(med), "\n", 'bias_x_bar', mean(x_bar), "\n")
    cat('mse_tmean:', mse_tmean, "\n", 'mse_med:', mse_med, "\n", 'mse_x_bar', mse_x_bar, "\n")  

#############################################################################################

#       PART 9 

###############################################################################

#  1: 

library(moments)
forearm = read.table("forearm.txt",header=F)
head(forearm)
forearm = forearm$V1
n = length(forearm)
B = 100 			 # number of bootstrap replicates.
S = numeric(B)               
sample_skew = skewness(forearm)   # get the value of the statistic of interest.

for(i in 1:B){
	inds = sample(1:n, size=n, replace=TRUE)
	xboot = forearm[inds]    # Extract these from the data.
	S[i] = skewness(xboot)
}

est_skew = mean(S)
seb = sd(S)         #calculate the standard error of this statistic.

sample_skew
est_skew
seb

###############################################################################

#  2: 
 
biasb = est_skew - sample_skew   # estimated bias in the sample skewness.

################################################################################

#  3: 

library(bootstrap)
law
B = 2000        # number of replicate
n = nrow(law)  # sample size
R = numeric(B) # storage for replicates

for(b in 1:B){
	i = sample(1:n, size=n, replace=TRUE) # randomly select the indices
	LSAT = law$LSAT[i]    # i is a vector of indices
	GPA = law$GPA[i]
	R[b] = cor(LSAT,GPA)
}
se.R = sd(R)
se.R

library(boot)

r = function(x,i){
	cor(x[i,1],x[i,2])
}

obj = boot(data=law, statistic=r, R=2000)
obj

R = cor(law82$LSAT,law82$GPA)
R
R.hat = cor(law$LSAT,law$GPA)
bias = mean(R)-R.hat
bias

################################################################################


## PART 9-x DISTRIBUTION FITTING


## gofExp.test Goodness-of-fit test for exponential distribution

library(Renext)

x <- rexp(30)
res <- gofExp.test(x)
res
hist(x)
summary(x)

y<-rnorm(30,15,2)
res <- gofExp.test(y)
res
hist(y)
summary(y)


## Kolmogorov-Smirnov Tests

x <- rexp(30,2)
y<- rexp(30,2)
ks.test(x,y)

x <- rexp(30,2)
y<-rnorm(30,15,2)
ks.test(x,y)

y<-rnorm(30,15,2)
ks.test(y, "pgamma", 3, 2) # two-sided, exact
ks.test(y, "pnorm", 15, 2) # two-sided, exact

## Shapiro-Wilk Normality Test

shapiro.test(rnorm(100, mean = 5, sd = 3))
shapiro.test(runif(100, min = 2, max = 4))


## Maximum-likelihood Fitting of Univariate Distributions

library(vcd)
library(MASS)

# data generation
ex <- rexp(10000, rate = 1.85) # generate some exponential distribution
control <- abs(rnorm(10000)) # generate some other distribution

# estimate the parameters
fit1 <- fitdistr(ex, "exponential") 
fit1
fit2 <- fitdistr(control, "exponential")
fit2

# goodness of fit test
ks.test(ex, "pexp", fit1$estimate) # p-value > 0.05 -> distribution not refused
ks.test(control, "pexp", fit2$estimate) #  significant p-value -> distribution refused

# plot a graph
hist(ex, freq = FALSE, breaks = 100, xlim = c(0, quantile(ex, 0.99)))
curve(dexp(x, rate = fit1$estimate), col = "red", add = TRUE)



## goodfit Goodness-of-fit Tests for Discrete Data

library(vcd)

## Simulated data examples:

dummy <- rbinom(100, size = 6, prob = 0.5)
gf1 <- goodfit(dummy, type = "binomial", par = list(size = 6))
summary(gf1)
plot(gf1)

## Real data examples:
data("HorseKicks")
HK.fit <- goodfit(HorseKicks)
summary(HK.fit)
plot(HK.fit)

data("Federalist")
F.fit <- goodfit(Federalist)
summary(F.fit)

## try negative binomial distribution
F.fit <- goodfit(Federalist, type = "nbinomial")
summary(F.fit)
plot(F.fit)
