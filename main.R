# Load packages
library(tidyr)
library(dplyr)
library(naniar)
library(ggplot2)
library(CGPfunctions)
require(graphics) 
library(cowplot) # used for correlation
library(car) # used for VIF
library(scatterplot3d) #used for graph
library(corrplot) # used for correlation

str(data)

dat <- data #Use 'dat' instead of data to preserve our original data
da<-dat

table(da$target)

da$target[da$target == 0] <- "Low Chance"
da$target[da$target == 1] <- "High Chance"

da$sex[da$sex == 0] <- "Females"
da$sex[da$sex == 1] <- "Males"

ggplot(da, aes(x=target)) + geom_bar(fill='black')
ggplot(da, aes(x=age)) + geom_bar(fill='blue')
plot(density(da$age), xlab = "Age", main = "Density plot of Age")
PlotXTabs(da, sex, target)