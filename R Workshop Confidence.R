#Simulated Regression Example
  #This coefplot package is optional, but a useful illustration
    #install.packages("coefplot")
    library(coefplot)
  
  #This section simulates data
  #Don't be too concerned about *how* this works, only what it is doing:
    #We are taking 1,000 samples of data (an X and Y variable) and running a regression on each
    #Confidence intervals around our estimates of the effect of X and Y should contain the true value of that effect 95% of the time (or 90, 99, etc.)
    #We know that the true effect of X on Y is 2; this is defined in the simulation
      example <- data.frame(x = rep(NA,1000),
                             se = rep(NA,1000))
      
      for (i in 1:1000) {
        x <- rnorm(100, mean = 0, sd = 1)
      
        b0 <- 0
        b1 <- 2
        e = rnorm(100, mean = 0, sd = 2)
        
        y <- b0 + b1*x + e
        
        lm <- lm(y ~ x)
        example$x[i] <- summary(lm)$coefficients[2,1]
        example$se[i] <- summary(lm)$coefficients[2,2]
      }
  
  #In real data analysis, we would only be able to look at one model
      #This summary and coefplot only looks at the last sample drawn. 
      summary(lm)
      coefplot(lm)
      #We can calculate a 95% confidence interval around that estimate by taking the estimate ± (1.96 * Standard Error)
      #That interval will contain the true estimate (2) 95% of the time across repeated samples
      #Does it in this case?
      
  
  #The "example" dataframe contains the estimate and standard error for all 1,000 of our random samples
      #We can calculate confidence intervals for all of them
      #qnorm(1 - .05/2) is simply the formal way of calculating 1.96
      example$lower95 <- example$x - (qnorm(1 - .05/2) * example$se)
      example$higher95 <- example$x + (qnorm(1 - .05/2) * example$se)
      #We can also make a variable called "test95" which is equal to 1 if the confidence interval contains the true value (2) and is equal to 0 otherwise
      example$test95 <- ifelse(example$lower95 < 2 & example$higher95 > 2, 1, 0)
  
      #Let's also do this for a 99% confidence interval for good measure
      example$lower99 <- example$x - (qnorm(1 - .01/2) * example$se)
      example$higher99 <- example$x + (qnorm(1 - .01/2) * example$se)
      example$test99 <- ifelse(example$lower99 < 2 & example$higher99 > 2, 1, 0)
  
  #This histogram shows the distribution of estimates for X. Notice that it is centered on 2 and normally distributed
  hist(example$x)
  
  #If theory is correct, 95% of the 95% confidence intervals will contain the value 2
  prop.table(table(example$test95))
  
  #And 99% of the 99% confidence intervals will contain the value 2
  prop.table(table(example$test99))
  
  #The following chart takes 100 of our random samples and plots them with their 95% confidence intervals
  #The blue intervals contain the true value 2
  #The red intervals do not contain the true value 2. Since we have 100 samples, there should be roughly 5 red lines (your mileage may vary due to randomness!)
    example$index <- c(1:1000)
    
    example.sub1 <- subset(example, example$index <= 100 & example$test95 == 1)
    example.sub2 <- subset(example, example$index <= 100 & example$test95 == 0)
    plot(example.sub1$index, example.sub1$x, ylim = c(1,3), col = "blue", ylab = "Estimated Effect of X on Y", xlab = "Index")
    arrows(example.sub1$index, example.sub1$lower95, example.sub1$index, example.sub1$higher95, col = "blue", length = 0, angle = 90, code = 3)
    points(example.sub2$index, example.sub2$x, ylim = c(0.7,3.2), col = "red")
    arrows(example.sub2$index, example.sub2$lower95, example.sub2$index, example.sub2$higher95, col = "red", length = 0, angle = 90, code = 3)
    abline(h = 2)
  #In real life, we will only observe one of these samples, and we won't know the true value. Its confidence interval helps us articulate uncertainty:
    example.sub3 <- subset(example, example$index == 50)
    plot(example.sub3$index, example.sub3$x, ylim = c(1,3), ylab = "Estimated Effect of X on Y", xlab = "Index")
    arrows(example.sub3$index, example.sub3$lower95, example.sub3$index, example.sub3$higher95, length = 0, angle = 90, code = 3)
    #We don't know for sure that our estimate of the effect of X on Y is correct
    #We are 95% sure that the true value falls in our interval.
    #But there's a 5% chance that we're in the "red" universe, and our interval does not contain the true value!