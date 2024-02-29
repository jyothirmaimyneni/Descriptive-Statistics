# Load required libraries
library(tidyverse)
library(car)
library(lmtest)

# Load the dataset
house_data <- read.csv("HousePrice Aug 2023.csv")
str(house_data)

# Explore the data - Descriptive Statistics and Visualizations
summary(house_data)
head(house_data)

# Scatterplot matrix for numeric variables//Figure.1
numeric_vars <- house_data %>%
  select_if(is.numeric)
pairs(numeric_vars)

# Initial Model//Figure.2
initial_model <- lm(price ~ ., data = house_data)
summary(initial_model)
#Figure.3
par(mfrow=c(2,2))
plot(initial_model)

library(car)
ncvTest(initial_model)

# Model Building and Diagnostics
# (Replace with your own stepwise selection process and diagnostic checks)
#Figure.5
library(MASS)
final_model <- stepAIC(initial_model)
par(mfrow=c(2,2))
plot(final_model)

# Model Assumptions and Diagnostics
# Linearity, Homoscedasticity, Normality, Independence//Figure.6,7,8,12
plot(final_model)

# Check for Multicollinearity using VIF//Figure.9
vif(final_model)

# Test for Heteroscedasticity//Figure.10
bptest(final_model)

# Test for Normality of Residuals//Figure.11
shapiro.test(residuals(final_model))

# Gauss-Markov Assumption (Homoscedasticity, No Perfect Multicollinearity, No Endogeneity)
# (Check and interpret the diagnostic tests accordingly)

# Summary of the Final Model
summary(final_model)

# Model Performance and Fit
# R-squared and Adjusted R-squared
rsquared <- summary(final_model)$r.squared
adj_rsquared <- summary(final_model)$adj.r.squared

# Print model summary
cat("R-squared:", rsquared, "\n")
cat("Adjusted R-squared:", adj_rsquared, "\n")
