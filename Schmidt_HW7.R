## ---- echo = FALSE, include = FALSE--------------------------------------
# Load ISLR Library
library(ISLR)
library(MASS) # Contains the Boston data
library(splines)
library(boot)

# Set the Seed for reproducibility
set.seed(232)
attach(Boston)

## ---- fig.width = 4.5, fig.height = 3.5, fig.align='center'--------------
cubic_fit <- lm(nox ~ poly(dis, 3), data = Boston)
coef(summary(cubic_fit))
dislims <- range(dis)
dis_grid <- seq(from = dislims[1], to = dislims[2])
cubic_pred <- predict(cubic_fit, newdata = list(dis = dis_grid), se = TRUE)
se_bands <- cbind(cubic_pred$fit + 2*cubic_pred$se.fit, 
                  cubic_pred$fit - 2*cubic_pred$se.fit)
par(mar = c(4.5,4.5,1,1), oma = c(0,0,2,0))
plot(dis, nox, xlim = dislims, col = "darkgrey", xlab = "dis", ylab = "nox")
title("Cubic Polynomial", outer = FALSE) # title that spans both plots
lines(dis_grid, cubic_pred$fit, lwd = 2, col = "red")
matlines(dis_grid, se_bands, lwd = 3, col = "orange", lty = 3)

## ---- eval = TRUE, echo = FALSE, fig.width = 8, fig.height = 5, fig.align='left'----
# Linear
fit.1 <- lm(nox ~ dis, data = Boston)
fit.1_pred <- predict(fit.1, newdata = list(dis = dis_grid), se = TRUE)
fit.1_se <- cbind(fit.1_pred$fit + 2*fit.1_pred$se.fit, fit.1_pred$fit - 2*fit.1_pred$se.fit)

# Quadratic
fit.2 <- lm(nox ~ poly(dis, 2), data = Boston)
fit.2_pred <- predict(fit.2, newdata = list(dis = dis_grid), se = TRUE)
fit.2_se <- cbind(fit.2_pred$fit + 2*fit.2_pred$se.fit, fit.2_pred$fit - 2*fit.2_pred$se.fit)

# Cubic
fit.3 <- lm(nox ~ poly(dis, 3), data = Boston)
fit.3_pred <- predict(fit.3, newdata = list(dis = dis_grid), se = TRUE)
fit.3_se <- cbind(fit.3_pred$fit + 2*fit.3_pred$se.fit, fit.3_pred$fit - 2*fit.3_pred$se.fit)

# Quartic
fit.4 <- lm(nox ~ poly(dis, 4), data = Boston)
fit.4_pred <- predict(fit.4, newdata = list(dis = dis_grid), se = TRUE)
fit.4_se <- cbind(fit.4_pred$fit + 2*fit.4_pred$se.fit, fit.4_pred$fit - 2*fit.4_pred$se.fit)

# 5 Degree
fit.5 <- lm(nox ~ poly(dis, 5), data = Boston)
fit.5_pred <- predict(fit.5, newdata = list(dis = dis_grid), se = TRUE)
fit.5_se <- cbind(fit.5_pred$fit + 2*fit.5_pred$se.fit, fit.5_pred$fit - 2*fit.5_pred$se.fit)

# 6 Degree
fit.6 <- lm(nox ~ poly(dis, 6), data = Boston)
fit.6_pred <- predict(fit.6, newdata = list(dis = dis_grid), se = TRUE)
fit.6_se <- cbind(fit.6_pred$fit + 2*fit.6_pred$se.fit, fit.6_pred$fit - 2*fit.6_pred$se.fit)

# 7 Degree
fit.7 <- lm(nox ~ poly(dis, 7), data = Boston)
fit.7_pred <- predict(fit.7, newdata = list(dis = dis_grid), se = TRUE)
fit.7_se <- cbind(fit.7_pred$fit + 2*fit.7_pred$se.fit, fit.7_pred$fit - 2*fit.7_pred$se.fit)


# 8 Degree
fit.8 <- lm(nox ~ poly(dis, 8), data = Boston)
fit.8_pred <- predict(fit.8, newdata = list(dis = dis_grid), se = TRUE)
fit.8_se <- cbind(fit.8_pred$fit + 2*fit.8_pred$se.fit, fit.8_pred$fit - 2*fit.8_pred$se.fit)

# 9 Degree
fit.9 <- lm(nox ~ poly(dis, 9), data = Boston)
fit.9_pred <- predict(fit.9, newdata = list(dis = dis_grid), se = TRUE)
fit.9_se <- cbind(fit.9_pred$fit + 2*fit.9_pred$se.fit, fit.9_pred$fit - 2*fit.9_pred$se.fit)

# 10 Degree
fit.10 <- lm(nox ~ poly(dis, 10), data = Boston)
fit.10_pred <- predict(fit.10, newdata = list(dis = dis_grid), se = TRUE)
fit.10_se <- cbind(fit.10_pred$fit + 2*fit.10_pred$se.fit, fit.10_pred$fit - 2*fit.10_pred$se.fit)

# 11 Degree
fit.11 <- lm(nox ~ poly(dis, 10), data = Boston)
fit.11_pred <- predict(fit.11, newdata = list(dis = dis_grid), se = TRUE)
fit.11_se <- cbind(fit.11_pred$fit + 2*fit.11_pred$se.fit, fit.11_pred$fit - 2*fit.11_pred$se.fit)

# Set up the plot
par(mfrow = c(2,3), mar = c(4.5,4.5,1,1), oma = c(0,0,4,0))

# First plot
plot(dis, nox, xlim = dislims, col = "darkgrey")
title("Linear", outer = FALSE) 
lines(dis_grid, fit.1_pred$fit, lwd = 2, col = "red")
matlines(dis_grid, fit.1_se, lwd = 3, col = "orange", lty = 3)

# Second plot
plot(dis, nox, xlim = dislims, col = "darkgrey")
title("Quadratic", outer = FALSE) 
lines(dis_grid, fit.2_pred$fit, lwd = 2, col = "red")
matlines(dis_grid, fit.2_se, lwd = 3, col = "orange", lty = 3)


# Third plot
#plot(dis, nox, xlim = dislims, col = "darkgrey")
#title("Cubic", outer = FALSE) 
#lines(dis_grid, fit.3_pred$fit, lwd = 2, col = "red")
#matlines(dis_grid, fit.3_se, lwd = 3, col = "orange", lty = 3)

# Fourth plot
#plot(dis, nox, xlim = dislims, col = "darkgrey")
#title("Quartic", outer = FALSE) 
#lines(dis_grid, fit.4_pred$fit, lwd = 2, col = "red")
#matlines(dis_grid, fit.4_se, lwd = 3, col = "orange", lty = 3)

# Fifth plot
plot(dis, nox, xlim = dislims, col = "darkgrey")
title("5 Degrees", outer = FALSE) 
lines(dis_grid, fit.5_pred$fit, lwd = 2, col = "red")
matlines(dis_grid, fit.5_se, lwd = 3, col = "orange", lty = 3)

# Sixth plot
#plot(dis, nox, xlim = dislims, col = "darkgrey")
#title("6 Degrees", outer = FALSE) 
#lines(dis_grid, fit.6_pred$fit, lwd = 2, col = "red")
#matlines(dis_grid, fit.6_se, lwd = 3, col = "orange", lty = 3)

# Seventh plot
#plot(dis, nox, xlim = dislims, col = "darkgrey")
#title("7 Degrees", outer = FALSE) 
#lines(dis_grid, fit.7_pred$fit, lwd = 2, col = "red")
#matlines(dis_grid, fit.7_se, lwd = 3, col = "orange", lty = 3)

# Eighth plot
plot(dis, nox, xlim = dislims, col = "darkgrey")
title("8 Degrees", outer = FALSE) 
lines(dis_grid, fit.8_pred$fit, lwd = 2, col = "red")
matlines(dis_grid, fit.8_se, lwd = 3, col = "orange", lty = 3)

# Ninth plot
#plot(dis, nox, xlim = dislims, col = "darkgrey")
#title("9 Degrees", outer = FALSE) 
#lines(dis_grid, fit.9_pred$fit, lwd = 2, col = "red")
#matlines(dis_grid, fit.9_se, lwd = 3, col = "orange", lty = 3)

# Tenth plot
#plot(dis, nox, xlim = dislims, col = "darkgrey")
#title("10 Degrees", outer = FALSE) 
#lines(dis_grid, fit.10_pred$fit, lwd = 2, col = "red")
#matlines(dis_grid, fit.10_se, lwd = 3, col = "orange", lty = 3)

# Eleventh plot
plot(dis, nox, xlim = dislims, col = "darkgrey")
title("11 Degrees", outer = FALSE) 
lines(dis_grid, fit.11_pred$fit, lwd = 2, col = "red")
matlines(dis_grid, fit.11_se, lwd = 3, col = "orange", lty = 3)

#  Plot the RSS 
RSS <- rep(NA, 11)
for (i in 1:11) {
    poly_fit <- lm(nox ~ poly(dis, i), data = Boston)
    RSS[i] <- sum(poly_fit$residuals^2)
}

plot(1:11, RSS, xlab = "Polynomial Degree", ylab = "Residual Sum of Squares", type = "l")
d.min <- which.min(RSS)
points(which.min(RSS), RSS[which.min(RSS)], col = "red", cex = 2, pch = 20)
title("RSS", outer = FALSE) 

# Name the Plot
title("Degree Polynomials and RSS", outer = TRUE) # title that spans both plots

## ---- fig.width = 4.15, fig.height = 3, fig.align='center'---------------
prediction_error <- rep(0, 10)
for (i in 1:10){ # Run all the polynomial models and store them
  # Use the glm function for poly models instead of lm so we can use cv.glm
   poly_fit <- glm(nox ~ poly(dis, i), data = Boston)
   prediction_error[i] <- cv.glm(Boston, poly_fit, K = 10)$delta[1]}
par(mfrow = c(1,1),  mar = c(4.5,4.5,1,1), oma = c(0,0,2,0)) # plot it!
plot(1:10, prediction_error, xlab = "Degree", ylab = "CV Error", type = "l")
d.min <- which.min(prediction_error)
points(which.min(prediction_error), prediction_error[which.min(prediction_error)], 
       col = "red", cex = 2, pch = 20)

## ---- fig.width = 4.25, fig.height = 3.15, fig.align='center'------------
spline_fit <- lm(nox ~ bs(dis, df = 4), data = Wage)
spline_pred <- predict(spline_fit, newdata = list(dis = dis_grid), se = TRUE)
par(mar = c(4.5,4.5,1,1), oma = c(0,0,2,0))
plot(dis, nox, col = "gray");title("Quartic", outer = FALSE)  # Plot the output
lines(dis_grid, spline_pred$fit, lwd = 2, col = "red")
lines(dis_grid, spline_pred$fit + 2* spline_pred$se, lwd = 3, col = "orange", lty = 3)
lines(dis_grid, spline_pred$fit - 2* spline_pred$se,lwd = 3, col = "orange", lty = 3)
attr(bs(dis, df = 4), "knots")

## ---- fig.width = 8, fig.height = 4.15, fig.align='center', message = FALSE, warning=FALSE----
# Code for *Question E*
RSS_reg_splines <- rep(NA, 18) 
for (i in 3:20) {
    reg_spline_fit <- lm(nox ~ bs(dis, df = i), data = Boston)
    RSS_reg_splines[i] <- sum(reg_spline_fit$residuals^2)}
par(mfrow = c(1,2), mar = c(4.5,4.5,1,1), oma = c(0,0,4,0))
RSS <- RSS_reg_splines[-c(1, 2)]
plot(1:18, RSS, xlab = "Degrees", ylab = "Test RSS", type = "l")
d.min <- which.min(RSS); title("Question E:  RSS", outer = FALSE)
points(which.min(RSS), RSS[which.min(RSS)], col = "red", cex = 2, pch = 20)

# Code for *Question F*
prediction_error <- rep(0, 20); set.seed(232)
for (i in 1:20){ # Run all the polynomial models and store them
  # Use the glm function for poly models instead of lm so we can use cv.glm
   reg_spline_fit <- glm(nox ~ bs(dis, df = i), data = Boston)
   prediction_error[i] <- cv.glm(Boston, reg_spline_fit, K = 10)$delta[1]}
plot(1:20, prediction_error, xlab = "Degree", ylab = "CV Error", type = "l")
d.min <- which.min(prediction_error); title("Question F:  CV Error", outer = FALSE)
points(which.min(prediction_error), prediction_error[which.min(prediction_error)], 
       col = "red", cex = 2, pch = 20)

