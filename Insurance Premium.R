library(tidyverse)
library(corrplot)
library(car)
library(lmtest)
library(ggplot2)
library(dplyr)

insurance <- read.csv("https://www.openml.org/data/get_csv/1586227/phpMYEkMl")

#EDA
summary(insurance_1)
# Histogram of charges(Right skewed)
ggplot(insurance_1, aes(x = charges)) + 
  geom_histogram(binwidth = 1000, fill = "blue", color = "black") +
  ggtitle("Distribution of charges") +
  xlab("Charges")
# Box plot of charges by smoker status
ggplot(insurance_1, aes(x = smoker, y = charges)) +
  geom_boxplot(fill = "blue", color = "black") +
  ggtitle("Distribution of charges by smoker status") +
  xlab("Smoker") +
  ylab("Charges")
# Scatter plot of charges and bmi
ggplot(insurance_1, aes(x = bmi, y = charges)) + 
  geom_point(color = "blue") +
  ggtitle("Relationship between bmi and charges") +
  xlab("BMI") +
  ylab("Charges")
# Correlation plot of numerical variables
numerical_vars <- c("age", "bmi", "children", "charges")
numerical_data <- insurance_1 %>% select(all_of(numerical_vars))

cor_matrix <- cor(numerical_data)
corrplot(cor_matrix, method = "number", type = "lower")

install.packages('fastDummies')
library('fastDummies')

# Make dummy variables of two columns:
  data1 <- dummy_cols(insurance_1, select_columns = c('smoker','region'))
head(data1)

#removing the columns 
data2 <- dummy_cols(insurance_1, select_columns = c('smoker','region'),
                    remove_selected_columns = TRUE)
#fitting model
model <- lm(charges ~ age + bmi + children + smoker + region, data = insurance_1)
summary(model)                           

#Transformations
# Applying log transformation to the right-skewed variables
insurance_1$log_charges <- log(insurance_1$charges)
hist(insurance_1$log_charges)

# sq root transformation
# Applying square root transformation to the right-skewed variables
insurance_1$sqrt_charges <- sqrt(insurance_1$charges)
hist(insurance_1$sqrt_charges)


df = data.frame(insurance_1)
#fitting model
model1 <- lm(log_charges ~ age + bmi + children + smoker + region, data = df)
summary(model1) 

#VIF for multicollinearity
# Load the "car" package
library(car)
library(caTools)
# Calculate VIF values for model1 predictors
faraway::vif(model1)
#for checking multi-collinearity and 
#we can carry on since the maximum threshold is 10



#Residual Analysis
# Extract the residuals from the model
residuals <- residuals(model1)

# Create a histogram of the residuals to check for normality
hist(residuals, breaks = 30, col = "grey", xlab = "Residuals", main = "Histogram of Residuals")

# Create a QQ-plot of the residuals to check for normality
qqnorm(residuals)
qqline(residuals)

# Create a scatterplot of the residuals versus the fitted values to check for heteroscedasticity
plot(model1$fitted.values, residuals, xlab = "Fitted Values", ylab = "Residuals", main = "Residuals vs Fitted Values")


# Check model assumptions for transformed data
par(mfrow=c(2,2))
plot(model1)
summary(model1)

faraway::vif(model1)
#for checking multi-collinearity and 
#we can carry on since the maximum threshold is 10

library(tseries)
jarque.bera.test(residuals)
# the residuals are not normally distributed

dwtest(model1)
# to check serial correlation
# Since we get p value > 0.05, hence there is no correlation in residuals

bptest(model1)
# the value is less than .05. 
# hence the residuals are heteroscedastic.
