## Descriptive Statistics

library(boot)
library(table1)

tab <- td[td$ca!= "4" & td$thal!= "0",]

tab$target <- 
  factor(tab$target, 
         levels=c("0","1"),
         labels=c("Low Probability","High Probability"))
tab$sex <- 
  factor(tab$sex, 
         levels=c("0","1"),
         labels=c("Female","Male"))
tab$cp <- 
  factor(tab$cp, 
         levels=c("0","1","2","3"),
         labels=c("Asymptomatic","Typical Angina","Atypical Angina", "Non-anginal Pain"))
tab$fbs <- 
  factor(tab$fbs, 
         levels=c("0","1"),
         labels=c("False","True"))
tab$restecg <- 
  factor(tab$restecg, 
         levels=c("0","1","2"),
         labels=c("Normal","ST-T wave abnormality","Ventricular Hypertrophy"))
tab$exang <- 
  factor(tab$exang, 
         levels=c("0","1"),
         labels=c("No","Yes"))
tab$slope <- 
  factor(tab$slope, 
         levels=c("0","1","2"),
         labels=c("Downsloping","Upsloping","Flat"))
tab$thal <- 
  factor(tab$thal, 
         levels=c("1","2","3"),
         labels=c("Normal","Fixed Defect","Reversable Defect"))

label(tab$age)          <- "Age"
label(tab$sex)      <- "Sex"
label(tab$cp)         <- "Chest Pain Type"
label(tab$trestbps)       <- "Blood Pressure"
label(tab$chol)        <- "Serum Cholestrol"
label(tab$fbs)   <- "Blood Sugar"
label(tab$restecg)  <- "Electrocardiography"
label(tab$thalach)      <- "Max Heart Rate"
label(tab$exang)      <- "Angina Status"
label(tab$oldpeak) <- "ST Depression"
label(tab$slope)       <- "Slope"
#label(tab$ca)       <- "Major Vessels"
label(tab$thal)       <- "Thalassemia Status"

# 
units(tab$age)          <- "years"
units(tab$fbs)      <- "> 120 mg/dl"
units(tab$chol)         <- "mg/dl"
units(tab$thalach)       <- "bpm"
units(tab$trestbps)        <- "mmHg"


tab1 <- table1(~ age + sex + cp + trestbps + chol + fbs + restecg + thalach + exang + oldpeak + slope + thal | target, data=tab, caption = "Table 1 - Summary of Sample Characteristics", overall="Total", render.missing=NULL)
tab1
knitr::kable(tab1, format = "latex", booktabs = TRUE)


## Exploratory Data Analysis

library(tidyr)
library(dplyr)
library(naniar)
library(ggplot2)
library(CGPfunctions)
require(graphics) 

dat <- data #Use 'dat' instead of data to preserve our original data
da<-dat

table(da$target)

da$target[da$target == 0] <- "Low Proabability"
da$target[da$target == 1] <- "High Probablity"

da$sex[da$sex == 0] <- "Females"
da$sex[da$sex == 1] <- "Males"

ggplot(da, aes(x=target)) + geom_bar(fill='black')
ggplot(da, aes(x=age)) + geom_bar(fill='blue')
plot(density(da$age), xlab = "Age", main = "Density plot of Age")
PlotXTabs(da, sex, target)

hist(dat$chol, main = "Histogram of serum cholestrol values")
hist(dat$trestbps, main = "Histogram of resting blood pressure values")

colnames(dat)[which(names(dat) == "thalach")] <- "maxHR"
hist(dat$maxHR, main = "Histogram of maximum heart rate acheived")

names <- c('sex' ,'cp', 'fbs', 'restecg', 'exang', 'slope', 'ca', 'thal', 'target')
dat[,names] <- lapply(dat[,names] , factor)
str(dat)
summary(dat)

boxplot(dat$trestbps ~ dat$sex)
boxplot(dat$age ~ dat$sex)


## Support Vector Machines

# Split the dataset into training and testing sets
set.seed(2023)
train_index <- sample(1:nrow(triald), 0.8*nrow(triald))
train_data <- triald[train_index, ]
test_data <- triald[-train_index, ]

# Build the SVM model
svm_model1 <- svm(target ~ ., data = train_data, kernel = "radial", cost = 1)

# Predict on the testing set
svm_pred1 <- predict(svm_model1, newdata = test_data)

# Evaluate the performance of the model
table(svm_pred1, test_data$target)