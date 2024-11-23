## Suppose you have any two points:
my_x <- c(1.5, 2.5)
my_y <- c(5, 2)

# Line goes through two points
## By definition of "slope":
my_line_slope <- (my_y[2] - my_y[1]) / (my_x[2]- my_x[1])
## Then just replace with one point to get intercept:
my_intercept <- my_y[1] - (my_line_slope * my_x[1])

calculate_resulting_y <- function(x) {
    my_line_slope * x + my_intercept
}

## Say then we have any value of x ("independent variable"):
my_interpolation_x_set <- seq(0, 5, 0.1)
## In a perfect World, we could then get any dependent value:
my_interpolation_y_set <- sapply(my_interpolation_x_set, calculate_resulting_y)

plot(my_interpolation_y_set ~ my_interpolation_x_set, col = "blue", type="l")
points(my_y ~ my_x, col = "red")

## Linear regression from scratch

## Some points that can be used:
my_regression_x_set <- seq(0, 5, 0.1) + rnorm(51,mean = 0, sd = 0.3)
my_regression_y_set <- my_interpolation_y_set + rnorm(51, mean = 0, sd = 0.3)

## SIMPLE linear regression. Using Least Squares distance.
## Key idea: Minimize differences.
## Find minimum for error function using differentiation.
## Final equations here instead of step by step explaining how to get there.

## Just follow the math:
mean_reg_x <- mean(my_regression_x_set)
mean_reg_y <- mean(my_regression_y_set)

dev_x <- (my_regression_x_set - mean_reg_x)
dev_y <- (my_regression_y_set - mean_reg_y)

beta_est <- sum(dev_x * dev_y) / sum(dev_x^2)
alpha_est <- mean_reg_y - (beta_est * mean_reg_x)

plot(my_regression_y_set ~ my_regression_x_set)
## alpha & beta can be confusing, I usually use Beta as multipliers of X...
abline(a = alpha_est, b = beta_est, col="blue")

## Importantly, squared error is differentiable and has a global minimum
## "easily". Related to "Gradient Descent"
err_x <- my_regression_y_set - (alpha_est + my_regression_x_set * beta_est)
err_y <- sapply(err_x, \(x) x^2)

plot(err_y ~ err_x)

## The issue with statistics without understanding data

## Direct from anscombe documentation
require(stats); require(graphics)
# summary(anscombe)
##-- now some "magic" to do the 4 regressions in a loop:
ff <- y ~ x
mods <- setNames(as.list(1:4), paste0("lm", 1:4))
for(i in 1:4) {
    ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
    mods[[i]] <- lmi <- lm(ff, data = anscombe)
    # print(anova(lmi))
}
## See how close they are (numerically!)
# sapply(mods, coef)
# lapply(mods, function(fm) coef(summary(fm)))
## Now, do what you should have done in the first place: PLOTS
op <- par(mfrow = c(2, 2), mar = 0.1+c(4,4,1,1), oma =  c(0, 0, 2, 0))
for(i in 1:4) {
    ff[2:3] <- lapply(paste0(c("y","x"), i), as.name)
    plot(ff, data = anscombe, col = "red", pch = 21, bg = "orange", cex = 1.2,
         xlim = c(3, 19), ylim = c(3, 13))
    abline(mods[[i]], col = "blue")
}
mtext("Anscombe's 4 Regression data sets", outer = TRUE, cex = 1.5)
par(op)

## And directly from Datasaurus Package documentation
library(datasauRus); library(ggplot2)

summary(datasaurus_dozen[datasaurus_dozen$dataset == "dino",])
summary(datasaurus_dozen[datasaurus_dozen$dataset == "bullseye",])

ggplot(datasaurus_dozen, aes(x = x, y = y, colour = dataset)) +
    geom_point() +
    theme_void() +
    theme(legend.position = "none") +
    facet_wrap(~dataset, ncol = 3)