###################################################
## Section 2.1
# Generating Uniform Samples
u =runif(2000) #default U(0,1)
hist(u, probability = TRUE, col="gray", border = "white")##plot histogram

## QQ plot for runif data against true theoretical distribution
qqplot(x=qunif(ppoints(500)), y=u, main=expression("Q-Q plot for Unif(0,1)"))
##ppoints is used to generate a sequence of probability points.
#qunif takes a vector of Probabilities. so we use ppoint.
qqline(y=u, distribution = qunif, probs = c(0.1,0.6),col=2) #draws a line which connects the probs..

## autocorrelation function
acf(x=u, main="Autocorrelation of samples")

## Generation of Uniform Samples
v=10*runif(1000,0,1)
hist(v, col = "gray", border = "white")

########################################################
## Section 2.2 ---> TRANSFORMATION METHODS.
# Sampling from a binoial distribution,
# once we have generated n bernoulli samples we can sum it to generate binomial samples;
N = 10000 # num of unif and binomial samples
n =10# num of bernoulli trials
p=0.3
u = runif(n=(n*N))
x = as.numeric(u<=p)
m=matrix(data = x, nrow =N, ncol=n)
Bin_samples = rowSums(m) ##Binomial Samples

## Visualizations
par(mfrow=c(1,2), mar=c(3,4,1,2))
hist(Bin_samples, probability = TRUE, main = "Binom(10, 0.3) from Unif(0,1)", col = "blue", border="white")
hist(rbinom(n=N, size=n, prob = p), probability = TRUE, main = "rbinom(10, 0.3)", col = "gray", border="white")

#######################################################
## Section 2.3 ----> Inverse Transform Method
## Generate 1000 samples from a exponential distribution
# let lambda =5
N =10^4
u=runif(N)
finv <- function(u){(-0.2)*log(u)}
outsamples = finv(u)
summary(outsamples)
summary(rexp(N,5))## from the R package itself.

## Visualizations
par(mfrow=c(1,2), mar=c(3,4,1,2))
hist(outsamples, probability = TRUE, main = "Inverse Trans", col = "gray", border = "white")
lines(x=ppoints(200), y=dexp(x=ppoints(200), rate=5), col="blue")

hist(rexp(n=N, rate = 5), probability = TRUE, main = "rexp", col = "gray", border = "white")
lines(x=ppoints(200), y=dexp(x=ppoints(200), rate=5), col="blue")

## Pareto Distribution
set.seed(122)
n=1000
u=runif(n)
a=3
b=2
pareto_inv <- function(u){b*(1-u)^(-1/a)}
pareto <- function(x){((b/x)^a)*(a/x)}
pareto_sample = pareto_inv(u)
summary(pareto_sample)

## Visualization
par(mfrow=c(1,1), mar=c(3,4,1,2))
hist(pareto_sample, probability = TRUE, breaks = 15, xlim = c(0,20), col = "gray", border = "white", 
     main = "Inv_transform Pareto(3,2)")
curve(pareto(x), from = 0, to=40, add = TRUE, col = "blue")

## Inverse Transform Method for Discrete Scenario.
n=10000
u=runif(n)

inv <- function(u){
  if(u<=0.1) x<- 0
  if(u>0.1 && u<=0.3) x<-1
  if(u>0.3 && u<=0.5) x<-2
  if(u>0.5 && u<=0.7) x<-3
  if(u>0.7 && u<=1) x<-4
  return(x)
} 

results = sapply(X=u, inv)
z=table(results)/n
barplot(z, border = "white", col = "lightblue", xlab = "x")

#################################################
### Section 2.4 --> Accept Reject Method
M = 1.5 #since f = beta(2,2) has a maximum at 1.5
X =rep(NA, 5) # vector of NA's
set.seed(123)
f <- function(x){6*x*(1-x)} # pdf of beta(2,2)
g <- function(x){1} # pdf of unif(0,1) is 1 in (0,1)
n=10000

# Now generating the samples
for(i in 1:5){
  print(paste("run ", i))
  u=round(runif(1),5)
  y=round(runif(1),5)
  accept <- u<=f(y)/(M*g(y))
  print(paste("U: ", u, "and Y: ", y, "and f/M*g", f(y)/(M*g(y))))
  print(paste("Accept? ", accept))
  if(accept){X[i] <- y}
}

## suppose we needed 1000 samples
i=0
M = 1.5 #since f = beta(2,2) has a maximum at 1.5
n=1000
X =rep(NA, n)
while (sum(is.na(X))) {
  u=round(runif(1),5)
  y=round(runif(1),5)
  accept <- u<=f(y)/(M*g(y))
  if(accept){
    i = i+1
    X[i] <- y
    }
}

summary(X)
qbeta(p=c(0, 0.25,0.5,0.75,1),2,2)

## PERFORMANCE EVALUATION
n=1000;u=runif(N);y=runif(N);M=1.5
accept <- u*M<f(y)/g(y)
mean(accept) #acceptance rate
print(1/M) ## probability of accepatance

## Visualizations
plot(y, u*M,  col=as.numeric(accept)+3,xlim = c(-0.2,1.2), ylim = c(0,2), main="Accept-Reject with M=1.5")
curve(expr = f, from = 0, to=1, xlim = c(-0.5,1.5), ylim=c(0,2), xlab = "x", ylab = "density", add=TRUE,lwd=2,main="Beta(2,2)")

### Generate Rayleigh Samples
N=10000
Z=rnorm(n=2*N)
Z=matrix(data = Z, nrow = N, ncol = 2)
transformation <- function(vec){
  R=sqrt(sum(vec^2))
  return(R)
}

rout =apply(Z, MARGIN = 1,FUN = transformation)
summary(rout)

## Theoretical mean
print(sqrt(pi/2))
hist(rout, col = "gray", border = "white")


