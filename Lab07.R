#load libraries
library(tidyverse)

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

