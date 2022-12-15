n <- 14  # numbers of subjects

ntrials <- 1000 # numbers of trials

intercept <- 0.05

beta1 <- 20



r2 <- 0.3 #

X1 = c(1:n)

for (i in c(1:n)){
  X <- rnorm(ntrials, 2, 4)
  
  e <- rnorm(ntrials)
  
  e <- resid(lm(e ~ X))
  
  e <- e * sqrt((1-r2)/r2*ssr/(sum(e^2)))
  
  X <- X + e
  
  X1[i] <- mean(X)
  }

yhat <- intercept + beta1*X1 

ssr <-sum((yhat-mean(yhat))^2)

e <- rnorm(n)

e <- resid(lm(e ~ X1))

e <- e * sqrt((1-r2)/r2*ssr/(sum(e^2)))

y <- yhat + e

mydata <- data.frame(y=y, yhat=yhat, X1=X1)

summary(lm(y ~ X1 , mydata))


