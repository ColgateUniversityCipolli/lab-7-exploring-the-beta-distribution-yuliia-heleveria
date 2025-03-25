#why are two diffrent values returned for MOM and MLE

################################################################################
# LAB 7-8 R CODE
# YULIIA HELEVERIA
# MATH 240 - SPRING 2025
################################################################################

################################################################################
# Load Libraries
################################################################################
library(tidyverse)
library(e1071)
library(cumstats)
library(patchwork)
library(nleqslv)

################################################################################
# TASK 1: describe the population distribution
################################################################################

################################################################################
#first case - beta(2, 5)
################################################################################
alpha.first <- 2 #define alpha and beta
beta.first <- 5

#plot the distribution
first.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha.first, beta.first))          # compute the beta PDF

ggplot(data= first.dist)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) +                 # plot beta dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom


#calculate mean, variance, skewness, excess kurtosis
mean.first <- alpha.first/(alpha.first+beta.first) #calculate mean
var.first <- (alpha.first*beta.first)/((alpha.first+beta.first)^2* #calculate variance
                                         (alpha.first+beta.first+1)) 
skew.first <- (2*(beta.first-alpha.first)*sqrt(alpha.first+beta.first+1))/ #calculate skewness
  ((alpha.first+beta.first+2)*sqrt(alpha.first*beta.first))
kurt.first <- (6*((alpha.first-beta.first)^2*(alpha.first+beta.first+1) - #calculate kurtosis
                    alpha.first*beta.first*(alpha.first+beta.first+2)))/
  (alpha.first*beta.first*(alpha.first+beta.first+2)*(alpha.first+beta.first+3))

#compute the table for these variables
stats.first.tibble <- tibble(mean = mean.first, #input calculated variables into the table
                             variance = var.first,
                             skewness = skew.first,
                             excess_kurtosis = kurt.first)

################################################################################
#second case - beta(5, 5)
################################################################################
alpha.second <- 5 #define alpha and beta
beta.second <- 5

#plot the distribution
second.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha.second, beta.second))         # compute the beta PDF

ggplot(data= second.dist)+                                            # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,5)")) +                 # plot beta dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom


#calculate mean, variance, skewness, excess kurtosis
mean.second <- alpha.second/(alpha.second+beta.second) #calculate mean
var.second <- (alpha.second*beta.second)/((alpha.second+beta.second)^2* #calculate variance
                                         (alpha.second+beta.second+1)) 
skew.second <- (2*(beta.second-alpha.second)*sqrt(alpha.second+beta.second+1))/ #calculate skewness
  ((alpha.second+beta.second+2)*sqrt(alpha.second*beta.second))
kurt.second <- (6*((alpha.second-beta.second)^2*(alpha.second+beta.second+1) - #calculate kurtosis
                    alpha.second*beta.second*(alpha.second+beta.second+2)))/
  (alpha.second*beta.second*(alpha.second+beta.second+2)*(alpha.second+beta.second+3))
#compute the table for these variables
stats.second.tibble <- tibble(mean = mean.second, #input calculated variables into the table
                             variance = var.second,
                             skewness = skew.second,
                             excess_kurtosis = kurt.second)

################################################################################
#third case - beta(5, 2)
################################################################################
alpha.third <- 5 #define alpha and beta
beta.third <- 2

#plot the distribution
third.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha.third, beta.third))          # compute the beta PDF

ggplot(data= third.dist)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,2)")) +                 # plot beta dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom


#calculate mean, variance, skewness, excess kurtosis
mean.third <- alpha.third/(alpha.third+beta.third) #calculate mean
var.third <- (alpha.third*beta.third)/((alpha.third+beta.third)^2* #calculate variance
                                            (alpha.third+beta.third+1)) 
skew.third <- (2*(beta.third-alpha.third)*sqrt(alpha.third+beta.third+1))/ #calculate skewness
  ((alpha.third+beta.third+2)*sqrt(alpha.third*beta.third))
kurt.third <- (6*((alpha.third-beta.third)^2*(alpha.third+beta.third+1) - #calculate kurtosis
                     alpha.third*beta.third*(alpha.third+beta.third+2)))/
  (alpha.third*beta.third*(alpha.third+beta.third+2)*(alpha.third+beta.third+3))
#compute the table for these variables
stats.third.tibble <- tibble(mean = mean.third, #input calculated variables into the table
                              variance = var.third,
                              skewness = skew.third,
                              excess_kurtosis = kurt.third)

################################################################################
#fourth case - beta(0.50, 0.50)
################################################################################
alpha.fourth <- 0.5 #define alpha and beta
beta.fourth <- 0.5

#plot the distribution
fourth.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha.fourth, beta.fourth))         # compute the beta PDF

ggplot(data= fourth.dist)+                                             # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(0.50,0.50)")) +           # plot beta dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom


#calculate mean, variance, skewness, excess kurtosis
mean.fourth <- alpha.fourth/(alpha.fourth+beta.fourth) #calculate mean
var.fourth <- (alpha.fourth*beta.fourth)/((alpha.fourth+beta.fourth)^2* #calculate variance
                                         (alpha.fourth+beta.fourth+1)) 
skew.fourth <- (2*(beta.fourth-alpha.fourth)*sqrt(alpha.fourth+beta.fourth+1))/ #calculate skewness
  ((alpha.fourth+beta.fourth+2)*sqrt(alpha.fourth*beta.fourth))
kurt.fourth <- (6*((alpha.fourth-beta.fourth)^2*(alpha.fourth+beta.fourth+1) - #calculate kurtosis
                    alpha.fourth*beta.fourth*(alpha.fourth+beta.fourth+2)))/
  (alpha.fourth*beta.fourth*(alpha.fourth+beta.fourth+2)*(alpha.fourth+beta.fourth+3))
#compute the table for these variables
stats.fourth.tibble <- tibble(mean = mean.fourth, #input calculated variables into the table
                             variance = var.fourth,
                             skewness = skew.fourth,
                             excess_kurtosis = kurt.fourth)

################################################################################
# TASK 2: compute the moments
################################################################################

#function that computes kth centered and uncentered moment for continous distribution
beta.moment <- function(alpha, beta, k, centered){
  moment <- 0
  if (centered == F){ #uncentered moments
    moment <- integrate(function(x){x^k*dbeta(x, alpha, beta)},
              lower = 0, upper = 1)$value
  }else{ #centered moments
    mu <- integrate(function(x){x*dbeta(x, alpha, beta)},
                    lower = 0, upper = 1)$value #calculate mu for intergration
    moment <- integrate(function(x){(x- mu)^k *dbeta(x, alpha, beta)},
                        lower = 0, upper = 1)$value
  }
  return(moment) #return the calculated moment
}

#confirm the function is working by computing population-level characteristics
#compute for beta(2, 5) - values matched
beta.first.mean <- beta.moment(alpha.first, beta.first, 1, F) #compute mean
beta.first.var <- beta.moment(alpha.first, beta.first, 2, T) #compute variance
beta.first.skew <- beta.moment(alpha.first, beta.first, 3, T)/
  (beta.moment(alpha.first, beta.first, 2, T))^(3/2) #compute skewness
beta.first.kurt <- beta.moment(alpha.first, beta.first, 4, T)/
  (beta.moment(alpha.first, beta.first, 2, T))^(2) - 3 #compute excess kurtosis

#compute for beta(5, 5) - values matched
beta.second.mean <- beta.moment(alpha.second, beta.second, 1, F) #compute mean
beta.second.var <- beta.moment(alpha.second, beta.second, 2, T) #compute variance
beta.second.skew <- beta.moment(alpha.second, beta.second, 3, T)/
  (beta.moment(alpha.second, beta.second, 2, T))^(3/2) #compute skewness
beta.second.kurt <- beta.moment(alpha.second, beta.second, 4, T)/
  (beta.moment(alpha.second, beta.second, 2, T))^(2) - 3 #compute excess kurtosis

#compute for beta(5, 2) - values matched
beta.third.mean <- beta.moment(alpha.third, beta.third, 1, F) #compute mean
beta.third.var <- beta.moment(alpha.third, beta.third, 2, T) #compute variance
beta.third.skew <- beta.moment(alpha.third, beta.third, 3, T)/
  (beta.moment(alpha.third, beta.third, 2, T))^(3/2) #compute skewness
beta.third.kurt <- beta.moment(alpha.third, beta.third, 4, T)/
  (beta.moment(alpha.third, beta.third, 2, T))^(2) - 3 #compute excess kurtosis

#compute for beta(0.50, 0.50) - values matched
beta.fourth.mean <- beta.moment(alpha.fourth, beta.fourth, 1, F) #compute mean
beta.fourth.var <- beta.moment(alpha.fourth, beta.fourth, 2, T) #compute variance
beta.fourth.skew <- beta.moment(alpha.fourth, beta.fourth, 3, T)/
  (beta.moment(alpha.fourth, beta.fourth, 2, T))^(3/2) #compute skewness
beta.fourth.kurt <- beta.moment(alpha.fourth, beta.fourth, 4, T)/
  (beta.moment(alpha.fourth, beta.fourth, 2, T))^(2) - 3 #compute excess kurtosis

################################################################################
# TASK 3: do data summaries help?
################################################################################
set.seed(7272) #set seed

################################################################################
#first case - beta(2, 5)
################################################################################
sample.size <- 500 # Specify sample details
beta.first.sample <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha.first,   # alpha parameter
                     shape2 = beta.first)    # beta parameter
#plot the histogram for distribution
beta.first.hist <- ggplot()+
  geom_histogram(data = tibble(beta.first.sample), aes(x = beta.first.sample, y=after_stat(density)),
                 color = "black", fill = "lightgray")+ #plot histogram
  #plot sample density
  stat_density(data = tibble(beta.first.sample), aes(x = beta.first.sample, y=after_stat(density), color="BetaSample(2,5)"),
               geom = "line")+
  #plot actual Beta density
  geom_line(data = first.dist, aes(x = x, y=beta.pdf, color="Beta(2,5)"))+
  theme_bw()+
  labs(
    title = "Histogram and Estimated Density of Beta Sample",
    x = "Sample of Beta Distribution",
    y = "Density"
  )+
  scale_color_manual( #complete the legend
    name = "Density",
    values = c("BetaSample(2,5)" = "red", "Beta(2,5)" = "blue"),
    labels = c("Beta Population(2,5)", "Beta Sample(2,5)"))+
  theme(legend.position = "bottom")

#include numerical summaries
beta.first.summary <- tibble(beta.first.sample) %>%
  summarize(mean = mean(beta.first.sample),
            variance = var(beta.first.sample),
            skewness = skewness(beta.first.sample),
            kurtosis = kurtosis(beta.first.sample))

################################################################################
#second case - beta(5, 5)
################################################################################
beta.second.sample <- rbeta(n = sample.size,  # sample size
                           shape1 = alpha.second,   # alpha parameter
                           shape2 = beta.second)    # beta parameter
#plot the histogram for distribution
beta.second.hist <- ggplot()+
  geom_histogram(data = tibble(beta.second.sample), aes(x = beta.second.sample, y=after_stat(density)),
                 color = "black", fill = "lightgray")+ #plot histogram
  #plot sample density
  stat_density(data = tibble(beta.second.sample), aes(x = beta.second.sample, y=after_stat(density),
                                                      color="BetaSample(5,5)")
               , geom = "line")+
  #plot actual Beta density
  geom_line(data = second.dist, aes(x = x, y=beta.pdf, color="Beta(5,5)"))+
  theme_bw()+
  labs(
    title = "Histogram and Estimated Density of Beta Sample",
    x = "Sample of Beta Distribution",
    y = "Density"
  )+
  scale_color_manual( #complete the legend
    name = "Density",
    values = c("BetaSample(5,5)" = "red", "Beta(5,5)" = "blue"),
    labels = c("Beta Population(5,5)", "Beta Sample(5,5)"))+
  theme(legend.position = "bottom")

#include numerical summaries
beta.second.summary <- tibble(beta.second.sample) %>%
  summarize(mean = mean(beta.second.sample),
            variance = var(beta.second.sample),
            skewness = skewness(beta.second.sample),
            kurtosis = kurtosis(beta.second.sample))

################################################################################
#third case - beta(5, 2)
################################################################################
beta.third.sample <- rbeta(n = sample.size,  # sample size
                            shape1 = alpha.third,   # alpha parameter
                            shape2 = beta.third)    # beta parameter
#plot the histogram for distribution
beta.third.hist <- ggplot()+
  geom_histogram(data = tibble(beta.third.sample), aes(x = beta.third.sample, y=after_stat(density)),
                 color = "black", fill = "lightgray")+ #plot histogram
  #plot sample density
  stat_density(data = tibble(beta.third.sample), aes(x = beta.third.sample, y=after_stat(density),
                                                     color="BetaSample(5,2)"),
               geom = "line")+
  #plot actual Beta density
  geom_line(data = third.dist, aes(x = x, y=beta.pdf, color="Beta(5,2)"))+
  theme_bw()+
  labs(
    title = "Histogram and Estimated Density of Beta Sample",
    x = "Sample of Beta Distribution",
    y = "Density"
  )+
  scale_color_manual( #complete the legend
    name = "Density",
    values = c("BetaSample(5,2)" = "red", "Beta(5,2)" = "blue"),
    labels = c("Beta Population(5,2)", "Beta Sample(5,2)"))+
  theme(legend.position = "bottom")

#include numerical summaries
beta.third.summary <- tibble(beta.third.sample) %>%
  summarize(mean = mean(beta.third.sample),
            variance = var(beta.third.sample),
            skewness = skewness(beta.third.sample),
            kurtosis = kurtosis(beta.third.sample))

################################################################################
#fourth case - beta(0.50, 0.50)
################################################################################
beta.fourth.sample <- rbeta(n = sample.size,  # sample size
                            shape1 = alpha.fourth,   # alpha parameter
                            shape2 = beta.fourth)    # beta parameter
#plot the histogram for distribution
beta.fourth.hist <- ggplot()+
  geom_histogram(data = tibble(beta.fourth.sample), aes(x = beta.fourth.sample, y=after_stat(density)),
                 color = "black", fill = "lightgray")+ #plot histogram
  #plot sample density
  stat_density(data = tibble(beta.fourth.sample), aes(x = beta.fourth.sample, y=after_stat(density),
                                                      color="BetaSample(0.50,0.50)"),
               geom = "line")+
  #plot actual Beta density
  geom_line(data = fourth.dist, aes(x = x, y=beta.pdf, color="Beta(0.50,0.50)"))+
  theme_bw()+
  labs(
    title = "Histogram and Estimated Density of Beta Sample",
    x = "Sample of Beta Distribution",
    y = "Density"
  )+
  scale_color_manual( #complete the legend
    name = "Density",
    values = c("BetaSample(0.50,0.50)" = "red", "Beta(0.50,0.50)" = "blue"),
    labels = c("Beta Population(0.50,0.50)", "Beta Sample(0.50,0.50)"))+
  theme(legend.position = "bottom")

#include numerical summaries
beta.fourth.summary <- tibble(beta.fourth.sample) %>%
  summarize(mean = mean(beta.fourth.sample),
            variance = var(beta.fourth.sample),
            skewness = skewness(beta.fourth.sample),
            kurtosis = kurtosis(beta.fourth.sample))

################################################################################
# TASK 4: is sample size important?
################################################################################
#compute cumulative summaries for beta(2,5)
cum.mean <- cummean(beta.first.sample) #cumulative mean
cum.var <- cumvar(beta.first.sample) #cumulative variance
cum.skew <- cumskew(beta.first.sample) #cumulative skewness
cum.kurt <- cumkurt(beta.first.sample) -3 #cumulative kurtosis

mean.plot <- ggplot(data = tibble(beta.first.sample))+ #plot for cumulative mean
  geom_line(aes(x=1:500, y = cum.mean), na.rm = T)+
  theme_bw()+
  geom_hline(yintercept=mean.first)+  #add true value from task one
  labs(x = "Sample size",
       y = "Cumulative mean")

var.plot <- ggplot(data = tibble(beta.first.sample))+ #plot for cumulative variance
  geom_line(aes(x=1:500, y = cum.var), na.rm = T)+
  theme_bw()+
  geom_hline(yintercept=var.first)+  #add true value from task one
  labs(x = "Sample size",
       y = "Cumulative variance")

skew.plot <- ggplot(data = tibble(beta.first.sample))+ #plot for cumulative skewness
  geom_line(aes(x=1:500, y = cum.skew), na.rm = T)+
  theme_bw()+
  geom_hline(yintercept=skew.first)+  #add true value from task one
  labs(x = "Sample size",
       y = "Cumulative skewness")

kurt.plot <- ggplot(data = tibble(beta.first.sample))+  #plot for cumulative kurtosis
  geom_line(aes(x=1:500, y = cum.kurt), na.rm = T)+
  theme_bw()+
  geom_hline(yintercept=kurt.first)+  #add true value from task one
  labs(x = "Sample size",
       y = "Cumulative kurtosis")

#combine the plots
combined.plots <- (mean.plot + var.plot)/(skew.plot+kurt.plot)

#do the loop iteration to simulate new data
for (i in 2:50){
  set.seed(7272+i) #set the seed
  beta.sample <- rbeta(n = sample.size,  # sample size
                             shape1 = alpha.first,   # alpha parameter
                             shape2 = beta.first)    # beta parameter
  new.data <- tibble( #create tibble with cumulative summaries
    index = 1:sample.size,
    mean = cummean(beta.sample),
    variance = cumvar(beta.sample),
    skewness = cumskew(beta.sample),
    kurtosis = cumkurt(beta.sample) -3
  )

  #Add new line for cumulative statistics of mean, varinace, skewness, and kurtosis
  mean.plot <- mean.plot +
    geom_line(data = new.data, aes(x=index, y= mean), color =i, na.rm = T)
  var.plot <- var.plot +
    geom_line(data = new.data, aes(x=index, y= variance), color =i, na.rm = T)
  skew.plot <- skew.plot +
    geom_line(data = new.data, aes(x=index, y= skewness), color =i, na.rm = T)
  kurt.plot <- kurt.plot +
    geom_line(data = new.data, aes(x=index, y= kurtosis), color =i, na.rm = T)
}
#combine all new plots together
combined.plots.new.sample <- (mean.plot + var.plot)/(skew.plot+kurt.plot)

################################################################################
# TASK 5: how can we model the variation?
################################################################################
num.iterations <- 1000 #number of times we will iterate
#empty vectors to store statistics of interest
mean.vector = rep(NA, num.iterations)
var.vector = rep(NA, num.iterations)
skew.vector = rep(NA, num.iterations)
kurt.vector = rep(NA, num.iterations)

#compute statistics for 1000 samples 
for (i in 1:num.iterations){
  set.seed(7272+i) #set seed to work with same samples
  #simulate new distribution
  beta.sample <- rbeta(n = sample.size,        # sample size
                       shape1 = alpha.first,   # alpha parameter
                       shape2 = beta.first)    # beta parameter
  #compute statistics for the new distribution
  mean.vector[i] <- mean(beta.sample) #compute mean
  var.vector[i] <-var(beta.sample) #compute variance
  skew.vector[i] <- skewness(beta.sample) #compute skewness
  kurt.vector[i] <- kurtosis(beta.sample) #compute kurtosis
}

#create tibble to plot the data
beta.tibble <- tibble( #add computed statistics into the tibble
  mean = mean.vector,
  variance = var.vector,
  skewness = skew.vector,
  kurtosis = kurt.vector)

#plot histogram for each statistics
#plot a histogram for mean
mean.hist <- ggplot()+
  geom_histogram(data = beta.tibble, aes(x= mean, y = ..density..),
                 fill = "lightblue", color = "black")+ #create a histogram
  theme_bw()+ #put white background
  geom_density(data = beta.tibble, aes (x= mean), , color = "red")+ #create a density line
  labs(x = "Mean", 
       y = "Density",
       title = "Sampling Distibution of Mean of Beta(2,5)")
  
#plot a histogram for variance
var.hist <- ggplot()+
  geom_histogram(data = beta.tibble, aes(x= variance, y = ..density..),
                 fill = "lightblue", color = "black")+ #create a histogram
  theme_bw()+ #put white background
  geom_density(data = beta.tibble, aes (x= variance), color = "red")+ #create a density line
  labs(x = "Variance", 
       y = "Density",
       title = "Sampling Distibution of Variance of Beta(2,5)")

#plot a histogram for skewness
skew.hist <- ggplot()+
  geom_histogram(data = beta.tibble, aes(x= skewness, y = ..density..),
                 fill = "lightblue", color = "black")+ #create a histogram
  theme_bw()+ #put white background
  geom_density(data = beta.tibble, aes (x= skewness), color = "red")+ #create a density line
  labs(x = "Skewness", 
       y = "Density",
       title = "Sampling Distibution of Skewness of Beta(2,5)")

#plot a histogram for kurtosis
kurt.hist <- ggplot()+
  geom_histogram(data = beta.tibble, aes(x= kurtosis, y = ..density..),
                 fill = "lightblue", color = "black")+ #create a histogram
  theme_bw()+ #put white background
  geom_density(data = beta.tibble,
               aes (x= kurtosis),
               color = "red")+ #create a density line
  labs(x = "Kurtosis", 
       y = "Density",
       title = "Sampling Distibution of Kurtosis of Beta(2,5)")

################################################################################
# TASK 6: collect and clean data
################################################################################
#open prepossessed world bank death data 
death.data <- read_csv("WorldBankDeathData.csv")

#modify data to keep country, country code, and year 2022
death.data.2022 <- death.data |>
  select("Country Name", "Country Code", "2022")

#convert number of death per 1000 citizens to a rate to support beta distribution
death.data.2022 <- death.data.2022|>
  mutate("2022" = death.data.2022$"2022"/1000) #divide each death rate by 1000

################################################################################
# TASK 7: what are alpha and beta?
################################################################################
#get data for moments estimate
data.2022 <- death.data.2022$"2022"
data.2022 <- data.2022[!is.na(data.2022)] #remove NA in data

#compute method of moments estimates
MOM.beta <- function(data, par){
  alpha <-par[1] #get alpha and beta
  beta <- par[2]
 
  #compute the first moment  
  EX1 = alpha/(alpha+beta)
  m1 <- mean(data, na.rm=T)
  #compute the second moment
  EX2 = ((alpha+1)*alpha)/((alpha+beta+1)*(alpha+beta))
  m2 <- mean(data^2, na.rm=T)
  
  to.return <- c(EX1-m1, EX2-m2) #both sides of each of two moments should be equal
  return(to.return) # Goal: find alpha and beta so both of there parameters are 0
}

MOM.sol <- nleqslv(x = c(8, 1000), #vector for the initial guess of alpha and beta
        fn = MOM.beta,
        data = data.2022)

#compute maximum likelihood estimates
llbeta <- function(data, par, neg=FALSE){
  alpha <- par[1] #get alpha and beta
  beta <- par[2]
  
  loglik <- sum(log(dbeta(x=data, shape1 = alpha, shape2 = beta)), na.rm = T)
  
  return(ifelse(neg, -loglik, loglik))
}

MLE.sol <- optim(par = c(8, 1000),
      fn = llbeta,
      data=data.2022,
      neg = T)

#superimpose distributions for MOM and MLE
ggdat.death <- tibble(x = seq(0,0.023,length.out=1000)) |>
  mutate(pdf.mom = dbeta(x=x, shape1=MOM.sol$x[1], shape2=MOM.sol$x[2]),
         pdf.mle = dbeta(x=x, shape1=MLE.sol$par[1], shape2=MLE.sol$par[2]))

#plot a histogram of death data
hist.2022 <- ggplot(data = tibble(data.2022))+
  geom_histogram(aes(x = data.2022, y = after_stat(density)),
                 color = "lightgray")+ #plot the histogram
  geom_line(data=ggdat.death,
            aes(x=x, y=pdf.mom, color="MOM"))+ #superimpose MOM distribution
  geom_line(data=ggdat.death,
            aes(x=x, y=pdf.mle, color="MLE"))+ #superimpose MLE distribution
  theme_bw()+ #remove the gray background
  geom_hline(yintercept = 0)+ #add x-axis
  xlab("Number of death per 1000 citizens (2022)")+ #add labels to x- and y-axis
  ylab("Density")+
  labs(color = "")

################################################################################
# TASK 8: which estimators should we use?
################################################################################
alpha <- 8 #define alpha and beta
beta <- 950
num <- 266 #define sample size 

#define storage for MOM and MLE results
MOMs <- rep(NA, 1000)
MLEs <- rep(NA, 1000)

#for loop to simulate new data
for (i in 1:1000){
  set.seed(7272+i) #set seed so everyone works with the same samples
  simulate.new.dat <- rbeta(n = num, shape1 = alpha, shape2 = beta) #simulate new data
  #compute and store MOM estimate
  MOMs[i] <- nleqslv(x = c(8, 950), 
                 fn = MOM.beta,
                 data = simulate.new.dat)
  #compute and store MLE estimate
  MLEs[i] <- optim(par = c(8, 950),
                fn = llbeta,
                data=simulate.new.dat,
                neg = T)
}




