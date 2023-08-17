# Math Camp Day 4 — The Regression Coefficient Formula in Action

# Simulate Data

  data <- data.frame(x = rep(NA,100), y = rep(NA, 100), e = rep(NA, 100))
  data$x <- rnorm(100, mean = 0, sd = 5)
  
  b0 <- 0
  b1 <- 1
  data$e = rnorm(100, mean = 0, sd = 5)
  
  data$y <- b0 + b1*data$x + data$e
  
  lm <- lm(y ~ x, data = data)
  summary(lm)
  plot(data$x, data$y)

# What is the formula for a beta coefficient?

    #Summation of "demeaned" X times "demeaned" Y
    #divided by Summation of "demeaned" X squared
  
  #Take the mean of the independent variable and subtract from the actual values
  x_mean <- mean(data$x)
  data$x_demean <- data$x - x_mean
  
  #Take the mean of the dependent variable and subtract from the actual values
  y_mean <- mean(data$y)
  data$y_demean <- data$y - y_mean
  
  #Multiply the demeaned X and Y
  data$xtimesy <- data$x_demean * data$y_demean
  
  #Take the sum across all observations
  numerator <- sum(data$xtimesy)
  
  #Square the demeaned X and sum across all observations
  data$x_demeansq <- data$x_demean^2
  denominator <- sum(data$x_demeansq)
  
  #Divide Summation of "demeaned" X times "demeaned" Y by 
    #Summation of "demeaned" X squared
  beta <- numerator / denominator
  beta
  alpha <- y_mean - beta*x_mean
  alpha
  lm
  
  abline(lm, col = "red")
  
  #When do we have larger values of beta?
    #When the numerator is large (positive or negative) and the denominator is small
  
  #When is the numerator large?
    #Numerator is very similar to covariance of X and Y
    numerator / (nrow(data) - 1)
    cov(data$x,data$y)
    #When X and Y are both signed the same, X times Y is positive
    #When X and Y are signed differently, X times Y is negative
    #Remember, we're working with demeaned data
    #If X and Y have no relationship, you will have a lot of positive *and* negative values, 
      # so the sum will be close to 0
    hist(data$x_demean)
    hist(data$y_demean)
    
    hist(data$xtimesy, xlim = c(-(max(data$xtimesy)),max(data$xtimesy)))
    
    #Counter-example
    x_new <- rnorm(100, mean = 0, sd = 5)
    y_new <- rnorm(100, mean = 0, sd = 5)
    uncorrelated <- (x_new - mean(x_new))*(y_new - mean(y_new))
    hist(uncorrelated)
  
  #When is the denominator small?
    #Denominator is very similar to variance (standard deviation squared) of X
    denominator / (nrow(data) - 1)
    var(data$x)
    #Denominator is smaller when there is less variance in X
    #The denominator is largely a "correction" based on the scale of X
    hist(data$x)
    