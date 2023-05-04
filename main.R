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
<<<<<<< HEAD
=======
library(cowplot) # used for correlation
library(car) # used for VIF
library(scatterplot3d) #used for graph
library(corrplot) # used for correlation


str(data)
>>>>>>> 5326794c348314bbf8858393b0efb71638477d76

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

<<<<<<< HEAD
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
=======


########### Nahid

#1[Reading libraries]
library(caret)
library(naniar)
library(mltools)
library(questionr)
library(corrplot)
library(caTools)
library(data.table)
library(mltools)
library(rpart)
library(rpart.plot)
library(plyr)
options(warn=-1)
options(repr.plot.width=20, repr.plot.height = 12)

#2[Load and Explain dataset]

# Read in a CSV file named "heart.csv" located in the "data" directory relative to the current working directory
dataset <- read.csv("./data/heart.csv")

# Display the first few rows of the dataset to check that it was read in correctly
head(dataset)

#About columns
"""
heart.csv dataset has the following columns:
age: the age of the patient (in years)
sex: the patient's sex (1 = male, 0 = female)
cp: the type of chest pain experienced by the patient (1 = typical angina, 2 = atypical angina, 3 = non-anginal pain, 4 = asymptomatic) trestbps: the patient's resting blood pressure (in mm Hg on admission to the hospital)
chol: the patient's cholesterol measurement (in mg/dl)
fbs: the patient's fasting blood sugar (> 120 mg/dl or not; 1 = true, 0 = false)
restecg: resting electrocardiographic results (0 = normal, 1 = having ST-T wave abnormality, 2 = showing probable or definite left ventricular hypertrophy)
thalach: the patient's maximum heart rate achieved during exercise
exang: whether the patient experienced exercise-induced angina (1 = yes, 0 = no)
oldpeak: ST depression induced by exercise relative to rest
slope: the slope of the peak exercise ST segment (1 = upsloping, 2 = flat, 3 = downsloping)
ca: the number of major vessels (0-3) colored by fluoroscopy
thal: a categorical variable indicating the patient's thalassemia status (3 = normal, 6 = fixed defect, 7 = reversable defect)
"""
#3[Deal with missing values]

# Check for missing values
vis_miss(dataset) + 
  ggtitle("Missing values for each variable")+
  theme(plot.title = element_text(hjust = 0.5, size =30), axis.title.y = element_text(size =25), axis.text = element_text(size =18), 
        legend.text = element_text(size =20))

# The plot showed that there were no missing values.
# It seems like a data cleaning and preprocessing step was taken to ensure the quality and integrity of the data before further analysis was done.

#4[Data Cleaning]

#Lableing data
dataset <- distinct(dataset)
dataset$sex <- factor(dataset$sex, labels = c("female","male"))
dataset$cp <- factor(dataset$cp)
dataset$restecg <- factor(dataset$restecg)
dataset$exang <- factor(dataset$exang, labels = c("no","yes"))
dataset$fbs <- factor(dataset$fbs, labels = c("no","yes"))
dataset$slope <- factor(dataset$slope)
dataset$ca <- factor(dataset$ca)
dataset$target <- factor(dataset$target)
df<-dataset[dataset$ca!="4" & dataset$thal !="0",]
df$thal <- factor(dataset2$thal, labels = c("normal","fixed defect","reversable defect"))

#5[show the cleaned data]

# Use the 'glimpse' function from the 'dplyr' package to display a concise summary of the 'df' dataframe
glimpse(df)
# Use the 'head' function to display the first few rows of the 'df' dataframe
head(df)
# Use the 'summary' function to generate a summary of the 'df' dataframe, which includes measures of central tendency, variability, and other descriptive statistics for each column
summary(df)
# Number of observations reduced to 296 from 303
# 9 variables converted to factors, 5 remain numeric

#6 [Check the importance of variables]

# Although it is evident that certain variables will be crucial for the random_forest_model, it is still necessary to evaluate the importance of each variable using statistical tests. 
# This will help to ensure that only the most relevant variables are included in the random_forest_model, 
# thereby improving its accuracy and reducing overfitting.

library(tidyverse)
library(rstatix)

# create a data frame with v-cramer factor values for categorical variables
data.frame(value = apply(df[,c(2,3,6,7,9,11,12,13)],2, FUN = function(x) {cramer.v(table(df$target,x))})) %>%
  
# plot the v-cramer values using ggplot2
ggplot(aes(x = colnames(df[,c(2,3,6,7,9,11,12,13)]), y=value))+

# create a bar plot of v-cramer values
geom_col( fill = "green", color ="grey", alpha = 0.6, width = 0.7)+

# add plot title and axis labels
labs(title ="V-Cramer plot", y = "Value", x ="Variables")+

# customize the y-axis scale
scale_y_continuous(breaks = seq(0,0.6,0.05))+

# customize plot theme
theme(plot.background = element_rect(fill = "#fff8ab"), panel.background = element_rect(fill = "#fff8ab"), 
    legend.background = element_rect(fill = "#fff8ab"), 
    axis.line = element_line(colour = "#636360"), plot.title = element_text(size =30,hjust =0.5), plot.title.position = "plot", panel.grid.major.x = element_blank(),
    panel.grid = element_line(colour = "grey"), axis.title.y = element_text(size =28), axis.text = element_text(size =25),
    axis.title.x = element_text(size =28))

#7 [Deal with out liers]

outliers <- function(x)
{
  d <- which(x<=(mean(x)-3*sd(x))) # down border
  u <- which(x>=(mean(x)+3*sd(x))) # up border
  if(length(d)>0) # replacing bottom observations if needed
  {
    m_d <- min(x[-d])
    x[d]<- m_d
  }
  if(length(u)>0) # replacing upper observations if needed
  {
    m_u <- max(x[-u])
    x[u]<-m_u
  }
  return(x) 
}
df[,c(1,4,5,8,10)] <- apply(df[,c(1,4,5,8,10)], 2, outliers)

#8 [split data] 

# 80% training set and 20% test set
set.seed(2023)

#create a vector of row indices for the training data
train_indices <- createDataPartition(df$target, p = 0.8, list = FALSE)
#split the data into training and test sets using the indices
train_set <- df[train_indices, -c(6, 7)]
test_set <- df[-train_indices, -c(6, 7)]

#9 [Random Forest]

# create a vector of seeds
seeds <- as.vector(c(1:11), mode = "list")

# set the last seed in the vector to 1
seeds[[11]] <- 1

# train a random forest model using the 'train' function from the 'caret' package
random_forest_model <- train(
  method ="rf", # set the method to random forest
  train_set[,-12], # set the predictors
  train_set$target, # set the target variable
  metric = "Accuracy", # set the evaluation metric
  trControl = trainControl(method = "repeatedcv", number =10, repeats = 10), # set the cross-validation method
  seeds=seeds, # set the seeds for the random number generator
  tuneGrid = expand.grid(.mtry=c(2,3,4,5,8)) # set the hyperparameters to tune
)

# print the trained model
random_forest_model

# use the trained model to make predictions on the test set
predictions2 <- predict(random_forest_model$finalModel, test_set, type = "class")

# print the confusion matrix for the training dataset
print("training dataset")
confusionMatrix(train_set$target, predict(random_forest_model$finalModel, train_set, type ="class"))

# print the confusion matrix for the test dataset
print("test dataset")
con2 <- confusionMatrix(test_set$target, predictions2)
con2

# plot the feature importance
plot(random_forest_model)

# plot the ROC curve and calculate the AUC score
colAUC(predict(random_forest_model$finalModel, test_set, type = "prob"), test_set$target, plotROC = T)
>>>>>>> 5326794c348314bbf8858393b0efb71638477d76
