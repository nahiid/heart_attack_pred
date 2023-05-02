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



########### Nahid
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