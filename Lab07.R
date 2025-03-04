#load libraries
library(tidyverse)
library(e1071)

#Task 1 - describe the population distribution

#first case 
alpha.first <- 2 #define alpha and beta
beta.first <- 5

#plot the distribution
first.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha.first, beta.first))                      # compute the beta PDF

ggplot(data= first.dist)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(2,5)")) +                 # plot beta dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom


#calculate mean, variance, skewness, excess kurtosis
mean.first <- alpha.first/(alpha.first+beta.first)
var.first <- (alpha.first*beta.first)/((alpha.first+beta.first)^2*
                                         (alpha.first+beta.first+1)) 
skew.first <- (2*(beta.first-alpha.first)*sqrt(alpha.first+beta.first+1))/
  ((alpha.first+beta.first+2)*sqrt(alpha.first*beta.first))
kurt.first <- (6*((alpha.first-beta.first)^2*(alpha.first+beta.first+1) - 
                    alpha.first*beta.first*(alpha.first+beta.first+2)))/
  (alpha.first*beta.first*(alpha.first+beta.first+2)*(alpha.first+beta.first+3))
#compute the table for these variables
stats.first.tibble <- tibble(mean = mean.first,
                             variance = var.first,
                             skewness = skew.first,
                             excess_kurtosis = kurt.first)

#second case
alpha.second <- 5 #define alpha and beta
beta.second <- 5

#plot the distribution
second.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha.second, beta.second))                      # compute the beta PDF

ggplot(data= second.dist)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,5)")) +                 # plot beta dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom


#calculate mean, variance, skewness, excess kurtosis
mean.second <- alpha.second/(alpha.second+beta.second)
var.second <- (alpha.second*beta.second)/((alpha.second+beta.second)^2*
                                         (alpha.second+beta.second+1)) 
skew.second <- (2*(beta.second-alpha.second)*sqrt(alpha.second+beta.second+1))/
  ((alpha.second+beta.second+2)*sqrt(alpha.second*beta.second))
kurt.second <- (6*((alpha.second-beta.second)^2*(alpha.second+beta.second+1) - 
                    alpha.second*beta.second*(alpha.second+beta.second+2)))/
  (alpha.second*beta.second*(alpha.second+beta.second+2)*(alpha.second+beta.second+3))
#compute the table for these variables
stats.second.tibble <- tibble(mean = mean.second,
                             variance = var.second,
                             skewness = skew.second,
                             excess_kurtosis = kurt.second)

#third case
alpha.third <- 5 #define alpha and beta
beta.third <- 2

#plot the distribution
third.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha.third, beta.third))                      # compute the beta PDF

ggplot(data= third.dist)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(5,2)")) +                 # plot beta dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom


#calculate mean, variance, skewness, excess kurtosis
mean.third <- alpha.third/(alpha.third+beta.third)
var.third <- (alpha.third*beta.third)/((alpha.third+beta.third)^2*
                                            (alpha.third+beta.third+1)) 
skew.third <- (2*(beta.third-alpha.third)*sqrt(alpha.third+beta.third+1))/
  ((alpha.third+beta.third+2)*sqrt(alpha.third*beta.third))
kurt.third <- (6*((alpha.third-beta.third)^2*(alpha.third+beta.third+1) - 
                     alpha.third*beta.third*(alpha.third+beta.third+2)))/
  (alpha.third*beta.third*(alpha.third+beta.third+2)*(alpha.third+beta.third+3))
#compute the table for these variables
stats.third.tibble <- tibble(mean = mean.third,
                              variance = var.third,
                              skewness = skew.third,
                              excess_kurtosis = kurt.third)

#fourth case
alpha.fourth <- 0.5 #define alpha and beta
beta.fourth <- 0.5

#plot the distribution
fourth.dist <- tibble(x = seq(-0.25, 1.25, length.out=1000))|>   # generate a grid of points
  mutate(beta.pdf = dbeta(x, alpha.fourth, beta.fourth))                      # compute the beta PDF

ggplot(data= fourth.dist)+                                              # specify data
  geom_line(aes(x=x, y=beta.pdf, color="Beta(0.50,0.50)")) +                 # plot beta dist
  geom_hline(yintercept=0)+                                            # plot x axis
  theme_bw()+                                                          # change theme
  xlab("x")+                                                           # label x axis
  ylab("Density")+                                                     # label y axis
  scale_color_manual("", values = c("black", "grey"))+                 # change colors
  theme(legend.position = "bottom")                                    # move legend to bottom


#calculate mean, variance, skewness, excess kurtosis
mean.fourth <- alpha.fourth/(alpha.fourth+beta.fourth)
var.fourth <- (alpha.fourth*beta.fourth)/((alpha.fourth+beta.fourth)^2*
                                         (alpha.fourth+beta.fourth+1)) 
skew.fourth <- (2*(beta.fourth-alpha.fourth)*sqrt(alpha.fourth+beta.fourth+1))/
  ((alpha.fourth+beta.fourth+2)*sqrt(alpha.fourth*beta.fourth))
kurt.fourth <- (6*((alpha.fourth-beta.fourth)^2*(alpha.fourth+beta.fourth+1) - 
                    alpha.fourth*beta.fourth*(alpha.fourth+beta.fourth+2)))/
  (alpha.fourth*beta.fourth*(alpha.fourth+beta.fourth+2)*(alpha.fourth+beta.fourth+3))
#compute the table for these variables
stats.fourth.tibble <- tibble(mean = mean.fourth,
                             variance = var.fourth,
                             skewness = skew.fourth,
                             excess_kurtosis = kurt.fourth)

#Task 2: compute the moments
#function that computes centered and uncentered moments
beta.moment <- function(alpha, beta, k, centered){
  moment <- 0
  if (centered == F){ #uncentered moments
    moment <- integrate(function(x){x^k*dnorm(x, mean=alpha, sd=beta)},
              lower = -Inf, upper = Inf)
  }else{ #centered moments
    moment <- integrate(function(x){(x- dnorm(x, mean=alpha, sd=beta))^k},
                        lower = -Inf, upper = Inf)
  }
  return(moment)
}

#confirm the function is working by computing population-level characteristics
beta.moment(2, 5, 20, T)
beta.moment(2, 5, 20, F)
beta.moment(5, 5, 1000, F)

#Task 3
set.seed(7272) #set seed

#first distribution 
sample.size <- 500 # Specify sample details
beta.first.sample <- rbeta(n = sample.size,  # sample size
                     shape1 = alpha.first,   # alpha parameter
                     shape2 = beta.first)    # beta parameter
#plot the histogram for distribution
beta.first.hist <- ggplot()+
  geom_histogram(data = tibble(beta.first.sample), aes(x = beta.first.sample, y=after_stat(density)),
                 color = "black", fill = "lightgray")+ #plot histogram
  #plot sample density
  geom_density(data = tibble(beta.first.sample), aes(x = beta.first.sample, y=after_stat(density), color="BetaSample(2,5)"))+
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
    labels = c("Beta Population", "Beta Sample"))+
  theme(legend.position = "bottom")

#include numerical summaries
beta.first.summary <- tibble(beta.first.sample) %>%
  summarize(mean = mean(beta.first.sample),
            variance = var(beta.first.sample),
            skewness = skewness(beta.first.sample),
            excess_kurtosis = kurtosis(beta.first.sample))

#second distribution
beta.second.sample <- rbeta(n = sample.size,  # sample size
                           shape1 = alpha.second,   # alpha parameter
                           shape2 = beta.second)    # beta parameter
#plot the histogram for distribution
beta.second.hist <- ggplot()+
  geom_histogram(data = tibble(beta.second.sample), aes(x = beta.second.sample, y=after_stat(density)),
                 color = "black", fill = "lightgray")+ #plot histogram
  #plot sample density
  geom_density(data = tibble(beta.second.sample), aes(x = beta.second.sample, y=after_stat(density), color="BetaSample(5,5)"))+
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
    labels = c("Beta Population", "Beta Sample"))+
  theme(legend.position = "bottom")

#include numerical summaries
beta.second.summary <- tibble(beta.second.sample) %>%
  summarize(mean = mean(beta.second.sample),
            variance = var(beta.second.sample),
            skewness = skewness(beta.second.sample),
            excess_kurtosis = kurtosis(beta.second.sample))

#third distribution
beta.third.sample <- rbeta(n = sample.size,  # sample size
                            shape1 = alpha.third,   # alpha parameter
                            shape2 = beta.third)    # beta parameter
#plot the histogram for distribution
beta.third.hist <- ggplot()+
  geom_histogram(data = tibble(beta.third.sample), aes(x = beta.third.sample, y=after_stat(density)),
                 color = "black", fill = "lightgray")+ #plot histogram
  #plot sample density
  geom_density(data = tibble(beta.third.sample), aes(x = beta.third.sample, y=after_stat(density), color="BetaSample(5,2)"))+
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
    labels = c("Beta Population", "Beta Sample"))+
  theme(legend.position = "bottom")

#include numerical summaries
beta.third.summary <- tibble(beta.third.sample) %>%
  summarize(mean = mean(beta.third.sample),
            variance = var(beta.third.sample),
            skewness = skewness(beta.third.sample),
            excess_kurtosis = kurtosis(beta.third.sample))

#fourth distribution
beta.fourth.sample <- rbeta(n = sample.size,  # sample size
                            shape1 = alpha.fourth,   # alpha parameter
                            shape2 = beta.fourth)    # beta parameter
#plot the histogram for distribution
beta.fourth.hist <- ggplot()+
  geom_histogram(data = tibble(beta.fourth.sample), aes(x = beta.fourth.sample, y=after_stat(density)),
                 color = "black", fill = "lightgray")+ #plot histogram
  #plot sample density
  geom_density(data = tibble(beta.fourth.sample), aes(x = beta.fourth.sample, y=after_stat(density), color="BetaSample(0.50,0.50)"))+
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
    labels = c("Beta Population", "Beta Sample"))+
  theme(legend.position = "bottom")

#include numerical summaries
beta.fourth.summary <- tibble(beta.fourth.sample) %>%
  summarize(mean = mean(beta.fourth.sample),
            variance = var(beta.fourth.sample),
            skewness = skewness(beta.fourth.sample),
            excess_kurtosis = kurtosis(beta.fourth.sample))
