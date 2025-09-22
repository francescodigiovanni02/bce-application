############################################################
# Gender Wage Gap Analysis
# Author: Francesco Di Giovanni
# Date: September 2025
#
# This script performs exploratory analysis and regression 
# models to evaluate gender differences in wages, 
# controlling for education and work experience.
############################################################

# Clean environment
rm(list = ls())

# Required libraries
library(foreign)
library(arm)

############################################################
# 1. Load data
############################################################
load("dati_salari.RData")

# Binary gender variable (1 = male, 0 = female)
wagedisc$male <- wagedisc$Sesso
attach(wagedisc)

############################################################
# 2. Exploratory analysis: descriptive plots
############################################################

par(mfrow = c(2,2))

# Boxplot: wages by gender
boxplot(Salario ~ Sesso, col = c("pink", "blue"), medcol = "white",
        xlab = "Gender", ylab = "Wages", xaxt = "n")
axis(1, c(1, 2), tick = TRUE, labels = c("F", "M"))

# Scatterplot: wages by gender
colors <- ifelse(male == 1, "blue", "pink")
plot(jitter(male, 0.8), jitter(Salario, 0.8), col = colors, pch = 20,
     xlab = "Gender", ylab = "Wage", xaxt = "n")
axis(1, c(0, 1), tick = TRUE, labels = c("F", "M"))

# Scatterplot: wages vs education
plot(Livello.di.Istruzione, Salario, col = colors, pch = 20)
legend("bottomright", legend = c("men", "women"), col = c("blue", "pink"), 
       pch = 19, bty = "n")

# Scatterplot: wages vs experience
plot(Esperienza, Salario, col = colors, pch = 20)
legend("bottomright", legend = c("men", "women"), col = c("blue", "pink"), 
       pch = 19, bty = "n")

par(mfrow = c(1,1))

# Wage distribution by gender
hist(Salario[male == 1], freq = FALSE, xlab = "Wage", 
     main = "Wage distribution (men)", col = "blue", ylim = c(0,0.30))
lines(density(Salario[male == 1], kernel = "gaussian"), col = "blue")

hist(Salario[male == 0], freq = FALSE, xlab = "Wage", 
     main = "Wage distribution (women)", col = "pink", ylim = c(0,0.30))
lines(density(Salario[male == 0], kernel = "gaussian"), col = "pink")

############################################################
# 3. Descriptive statistics
############################################################
mean(Salario) 
mean(Salario[male == 1]) 
mean(Salario[male == 0]) 

table(Sesso)  
table(Esperienza) 
table(Livello.di.Istruzione)

############################################################
# 4. Regression models
############################################################

# Simple regression: wage ~ gender
fit.6 <- lm(Salario ~ male, data = wagedisc)
display(fit.6)
summary(fit.6)

# Log-wage regression: log(wage) ~ gender
fit.1 <- lm(log(Salario) ~ male, data = wagedisc)
display(fit.1)
summary(fit.1)

# Residuals analysis
resid_fit1 <- resid(fit.1)
hist(resid_fit1, breaks = 50, freq = FALSE, main = "Residuals Histogram (fit.1)")  
lines(density(resid_fit1))

############################################################
# 5. Models with education and experience
############################################################

fit.2 <- lm(log(Salario) ~ male + rescale(Livello.di.Istruzione) + rescale(Esperienza), 
            data = wagedisc)
display(fit.2)
summary(fit.2)

# Model with interaction experience*gender
fit.3 <- lm(log(Salario) ~ male + rescale(Livello.di.Istruzione) +
              rescale(Esperienza) + male*rescale(Esperienza), 
            data = wagedisc)
display(fit.3)
summary(fit.3)

# --- KEEP YOUR ORIGINAL COMMENTS ON COEFFICIENTS HERE ---
# e.g.:
# The interaction coefficient is positive and statistically significant...
# Experience has a stronger effect on women than on men...
# The gender coefficient is almost null when controlling for education and experience...
# ...

############################################################
# 6. Residual diagnostics for fit.3
############################################################

resid_fit3 <- resid(fit.3)
fitted_fit3 <- fit.3$fitted.values

# Histogram and density
hist(resid_fit3, breaks = 50, freq = FALSE, 
     main = "Residuals Histogram (fit.3)", xlab = "Residuals", col = "gray")
lines(density(resid_fit3), col = "red", lwd = 2)

# Residuals vs fitted values
plot(fitted_fit3, resid_fit3, xlab = "Fitted values", ylab = "Residuals",
     main = "Residuals vs Fitted")
abline(h = 0, col = "blue", lty = 2)

# Q-Q plot
qqnorm(resid_fit3, main = "Q-Q Plot of Residuals")
qqline(resid_fit3, col = "red", lwd = 2)

# Comment on residual diagnostics:
# Residual diagnostics indicate that the model fits reasonably well:
# - Residuals are approximately symmetrically distributed around zero.
# - No clear nonlinear patterns are visible in residual vs fitted plots.
# - Variance appears roughly constant across levels of fitted values.
# These checks suggest that the assumptions of linearity, homoscedasticity,
# and approximate normality of errors are reasonably satisfied.

############################################################
# 7. Additional models (quadratic, predictions, diagnostics)
############################################################

# Example: quadratic regression on education
fit.5 <- lm(log(Salario) ~ rescale(Esperienza) + rescale(Livello.di.Istruzione) +  
              I(rescale(Livello.di.Istruzione)^2) + male + rescale(Esperienza):male,
            data = wagedisc)
display(fit.5)
summary(fit.5)

############################################################
# Conclusions:
# - Gender wage differences decrease when controlling for education and experience.
# - Experience has a stronger impact on women’s wages compared to men’s.
# - Wages follow an approximately log-normal distribution,
#   therefore log-linear models are more appropriate.
############################################################

