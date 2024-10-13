#Adding required libraries
library(dplyr)
require(aod)
require(car)

#Download dataset
URL <- "https://archive.ics.uci.edu/ml/machine-learning-databases/forest-fires/forestfires.csv"
download.file(URL, "forestfires.csv")

#Make dataframe from dataset
forest_fires <- read.csv("forestfires.csv")

#Display first few rows of dataframe
head(forest_fires)

#Provide statistical summary of dataframe
summary(forest_fires)

#Create new dataframe where all observations containing area == 0 is discarded.
#All columns except FWI variables and area are also discarded
forest_fires_clean <- filter(forest_fires, area != '0') %>% subset(select = -c(X, Y, month, day, temp, RH, wind, rain))

#Create statistical summary of new dataframe
summary(forest_fires_clean)

#Create a histogram of area data
hist(forest_fires_clean$area, main = "Histogram of area", xlab = "area", col = "red")

#Create Q-Q plot of area data
qqnorm(forest_fires_clean$area, main = "Sample vs. Theoretical Quantiles of Area Data", col = "red")
qqline(forest_fires_clean$area, col = "blue")

#Perform logarithmic transformation on area data
ln_area <- log(forest_fires_clean$area)

#Create histogram of log transformed area data
hist(ln_area, main = "Histogram of ln(area)", col = "blue")

#Create Q-Q plot of log transformed area data
qqnorm(ln_area, main = "Sample vs. Theoretical Quantiles of ln(area)", col = "red")
qqline(ln_area, col = "blue")

#Create new dataframe with log transformed area data appended and discard area column
ffc_ln_area <- cbind(forest_fires_clean, ln_area) %>% subset(select = -c(area))

#Perform multivariate linear regression on log(area) against all remaining variables
model <- lm(ln_area ~ FFMC + DMC + DC + ISI, data = ffc_ln_area)

#Plot log(area) against each variable
avPlots(model, col = "red")

#Generate residuals of model
res <- resid(model)

#Create a residuals vs. fitted values plot
plot(fitted(model), res, main = "Residuals vs. Fitted Values", xlab = "Fitted values", ylab = "Residuals", col = "red")
abline(0,0, col = "blue")

#Create Q-Q plot for residuals
qqnorm(res, main = "Sample vs. Theoretical Quantiles of Residuals", col = "red")
qqline(res, col = "blue")

#Create density plot of residuals
plot(density(res), main = "Density of Residuals", xlab = "Residuals", col = "blue")

#Create summary of model
summary(model)

#Wald test on all regression variables individually (individually insignificant)
wald.test(Sigma = vcov(model), b = coef(model), Terms = 1)
wald.test(Sigma = vcov(model), b = coef(model), Terms = 2)
wald.test(Sigma = vcov(model), b = coef(model), Terms = 3)
wald.test(Sigma = vcov(model), b = coef(model), Terms = 4)

#Wald test on all regression variables together (collectively significant)
wald.test(Sigma = vcov(model), b = coef(model), Terms = 1:4)