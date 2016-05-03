#####  Chapter 7 Lab

library(ISLR)

dim(Wage)
colnames(Wage)

attach(Wage)
#detach(wage)

fit <- lm(wage ~ poly(age,4), data = Wage)
summary(fit)
coef(summary(fit))

fit2 <- lm(wage ~ poly(age, 4, raw = TRUE), data = Wage)
summary(fit2)

# same as fit2, except uses I() wrapper function
fit2a <- lm(wage ~ age+I(age^2) + I(age^3) + I(age^4), data = Wage)
summary(fit2a)

# Same as fit2 and fit2a
fit2b <- lm(wage ~ cbind(age, age^2, age^3, age^4), data = Wage)
summary(fit2b)

# Create a grid of values for age set age limits
agelims <- range(age)
age_grid <- seq(from = agelims[1], to = agelims[2])
predictions <- predict(fit, newdata = list(age = age_grid), se = TRUE)
se_bands <- cbind(predictions$fit + 2*predictions$se.fit, 
                  predictions$fit - 2*predictions$se.fit)

# mar & oma arguments = influence where the margins are
par(mfrow = c(1,1), mar = c(4.5,4.5,1,1), oma = c(0,0,4,0))
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Degree-4 Polynomial", outer = FALSE) # title that spans both plots
lines(age_grid, predictions$fit, lwd = 2, col = "red")
matlines(age_grid, se_bands, lwd = 3, col = "orange", lty = 3)
plot(education, wage, cex = 0.5, col = "cornflowerblue")


predictions_2 <- predict(fit2, newdata = list(age = age_grid), se = TRUE)
max(abs(predictions$fit - predictions_2$fit))


###  Which polynomial to use?  Let's use ANOVA to test the best one!
fit.1 <- lm(wage ~ age, data = Wage)
fit.2 <- lm(wage ~ poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ poly(age, 4), data = Wage)
fit.5 <- lm(wage ~ poly(age, 5), data = Wage)

# Run the ANOVA to see which one is best
anova(fit.1, fit.2, fit.3, fit.4, fit.5)

# appears that the cubic or quartic regression is the best
coef(summary(fit.1))
coef(summary(fit.2))
coef(summary(fit.3))
coef(summary(fit.4))
coef(summary(fit.5))


# Try with another predictor
fit.1 <- lm(wage ~ education + age, data = Wage)
fit.2 <- lm(wage ~ education + poly(age, 2), data = Wage)
fit.3 <- lm(wage ~ education + poly(age, 3), data = Wage)
fit.4 <- lm(wage ~ education + poly(age, 4), data = Wage)
anova(fit.1, fit.2, fit.3, fit.4)


# Polynomial Logistic Regression
poly_logistic <- glm(I(wage > 250) ~ poly(age, 4), data = Wage, family = binomial)
# when we do type = "link" we get predictions for the logit function
logistic_pred <- predict(poly_logistic, newdata = list(age = age_grid), se = TRUE, type = "link")

## Confidence Intervals are more involved in logistic regression
logit_ci <- exp(logistic_pred$fit)/(1 + exp(logistic_pred$fit))
se_bands_logit <- cbind(logistic_pred$fit + 2*logistic_pred$se.fit,
                        logistic_pred$fit - 2*logistic_pred$se.fit)
se_bands <- exp(se_bands_logit)/(1 + exp(se_bands_logit))


# Or do it directly with type = "response"!  DO THIS ONE
logistic_pred2 <- predict(poly_logistic, newdata = list(age = age_grid), se = TRUE, type = "response")

plot(age, I(wage > 250), xlim = agelims, type = "n", ylim = c(0, 0.2))
points(jitter(age), I((wage>250)/5), cex = 0.5, pch = "l", col = "darkgrey")    
title("Rug Plot", outer = TRUE) # title that spans both plots
lines(age_grid, logistic_pred2$fit, lwd = 2, col = "red")
matlines(age_grid, se_bands, lwd = 3, col = "cornflowerblue", lty = 3)



###  STEP FUNCTION
table(cut(age,4))
step_fit <- lm(wage ~ cut(age, 4), data = Wage)
step_fit_pred <- predict(step_fit, newdata = list(age = age_grid), se = TRUE, type = "response")
plot(age, wage, xlim = agelims, cex = 0.5, col = "darkgrey")
title("Step Function", outer = FALSE) # title that spans both plots
lines(age_grid, step_fit_pred$fit, lwd = 2, col = "red")
matlines(age_grid, se_bands, lwd = 3, col = "orange", lty = 3)




##  Splines 
library(splines)

# The bs() function generates the entire matrix of basis functions for splines with the specified set of knots.
# By default cubic splines are created
spline_fit <- lm(wage ~ bs(age, knots = c(25, 40, 60)), data = Wage)
spline_pred <- predict(spline_fit, newdata = list(age = age_grid), se = TRUE)
plot(age, wage, col = "gray")
lines(age_grid, spline_pred$fit, lwd = 2)
lines(age_grid, spline_pred$fit + 2* spline_pred$se, lty = "dashed")
lines(age_grid, spline_pred$fit - 2* spline_pred$se, lty = "dashed")


# Use df to produce a spline with knots at uniform quantiles of the data
dim(bs(age, knots = c(25, 40, 60)))
dim(bs(age, df = 6))
# the bs() function also has a degrees of freedom argument, df.
# So we can fit splines of any degree
attr(bs(age, df = 6), "knots")


# To fit a natural spline we will use the ns() function
nat_spline_fit <- lm(wage ~ ns(age, df = 4), data = Wage)
nat_spline_pred <- predict(nat_spline_fit, newdata = list(age = age_grid), se = TRUE)
lines(age_grid, nat_spline_pred$fit, col = "red", lwd = 2)


