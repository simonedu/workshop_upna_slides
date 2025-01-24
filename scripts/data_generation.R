n <- 15 # Number of observations
a <- 0 # Intercept
b <- 1.5 # Slope
sigma2 <- 25 # Residual variance
x <- 1:15 # Values of covariate 
set.seed(15)
eps <-rnorm(n, mean=0, sd=sqrt(sigma2))
y <- a + b*x + eps

model5 <- lm(y ~ x)

summary(model5)

# Normality of residuals

# The QQ plot of residuals can be 
# used to visually check the normality 
# assumption. The normal probability plot
# of residuals should
# approximately follow a straight line.

plot(model5, 2)

n <- 100 # Number of observations
a <- 0 # Intercept
b <- 1.5 # Slope
sigma2 <- 25 # Residual variance
x <- 1:100 # Values of covariate 
set.seed(15)
eps <-rnorm(n, mean=0, sd=sqrt(sigma2))
y <- a + b*x + eps

model5 <- lm(y ~ x)

summary(model5)

# Normality of residuals

# The QQ plot of residuals can be 
# used to visually check the normality 
# assumption. The normal probability plot
# of residuals should
# approximately follow a straight line.

plot(model5, 2)



# https://library.virginia.edu/data/articles/diagnostic-plots

# This is how you can check the assumption of equal variance (homoscedasticity). 
# It’s good if you see a horizontal line with equally (randomly) spread points.

plot(model5, 3)

library(ggfortify)
autoplot(lm(Petal.Width ~ Petal.Length, 
    data = iris), 
    label.size = 3)

par(mfrow = c(1, 2))
m <- lm(Petal.Width ~ Petal.Length, data = iris)

autoplot(m, which = 1:6, ncol = 3, label.size = 3)

autoplot(model5)


# https://www.geeksforgeeks.org/linear-regression-assumptions-and-diagnostics-using-r/?ref=ml_lbp
# Load libraries and create a hypothetical dataset
library(ggplot2)
set.seed(123)
# Create individual vectors for X and Y
X <- 1:100
Y <- 2 * X + rnorm(100, mean = 0, sd = 10)

# Combine them into a data frame

data <- data.frame(
    X = 1:100, 
    Y = 2 * X + rnorm(100, mean = 0, sd = 10))

# Check linearity by creating a scatterplot
ggplot(data, aes(x = X, y = Y)) + 
    geom_point() + 
    geom_smooth(method = "lm")

# Check linearity by creating a scatterplot
ggplot(data, aes(x = X, y = Y)) + 
    geom_point() + 
    geom_smooth(method = "lm", formula = y ~ x + I(x^2))









# Example: Checking homoscedasticity
# Create a plot of residuals vs. fitted values

fit <- lm(Y ~ X, data = data)
plot(fit)

#plotting the model
ggplot(data, aes(X, Y)) +
    geom_point() +
    geom_line(aes(X, predict(fit))) +
    ggtitle("Polynomial Regression")

fit_poly <- lm(Y ~ poly(X), data = data)
plot(fit_poly)

#plotting the model
ggplot(data, aes(X, Y)) +
    geom_point() +
    geom_line(aes(X, predict(fit_poly))) +
    ggtitle("Polynomial Regression")



# imports library
library(minpack.lm)
library(ggplot2)
# generate data
x <- c(0, 1, 2, 3, 4, 5)
y <- c(1, 2, 4, 8, 16, 32)
# fit the model 
start_values <- c(a=4, b=2)
fit <- nls(y ~ a * exp(b * x),
    start = start_values,
    algorithm = "port",
    control = nls.control(maxiter = 1000))
summary(fit)

# plotting
ggplot(data.frame(x, y), aes(x, y)) +
    geom_point() +
    geom_line(aes(x, predict(fit, newdata = data.frame(x)))) +
    ggtitle("Exponential Regression") +
    xlab("x") +
    ylab("y")

#data 
x <- 1:10
y <- x^2 + x + 2 + rnorm(10, 0, 10)
df <- data.frame(x, y)
#fitting the model
fit <- lm(y ~ poly(x, 2), data = df)
summary(fit)

library(ggplot2)
#plotting the model
ggplot(df, aes(x, y)) +
    geom_point() +
    geom_line(aes(x, predict(fit))) +
    ggtitle("Polynomial Regression")

##############

linear <- ggplot(mtcars, aes(x=hp, y=mpg))+
    geom_point() +
    stat_smooth(aes(y=mpg), method = "lm")

non_linear <- ggplot(mtcars, aes(x=hp, y=mpg))+
    geom_point() +
    stat_smooth(aes(y=mpg), method = "lm", 
        formula = y ~ x + I(x^2))  

###

linear <- ggplot(mtcars, aes(x=hp, y=mpg))+
    geom_point() +
    stat_smooth(aes(y=mpg), method = "lm") +
    theme_minimalism()

non_linear <- ggplot(mtcars, aes(x=hp, y=mpg))+
    geom_point() +
    stat_smooth(aes(y=mpg), method = "lm", 
        formula = y ~ x + I(x^2))  +
    theme_minimalism()


# Custom custom theme 

# Adapted from Hehman, E., & Xie, S. Y. (2021). Doing better data visualization.
# Advances in Methods and Practices in Psychological Science, 4(4).
# https://journals.sagepub.com/doi/pdf/10.1177/25152459211045334

theme_minimalism <- function() {
    theme_minimal() + # ggplot's minimal theme hides many unnecessary features of plot
        theme( # make modifications to the theme
            panel.grid.major.y = element_blank(), # hide major grid for y axis
            panel.grid.minor.y = element_blank(), # hide minor grid for y axis
            panel.grid.major.x = element_blank(), # hide major grid for x axis
            panel.grid.minor.x = element_blank(), # hide minor grid for x axis
            plot.background = element_blank(),
            panel.border = element_blank(),
            legend.title = element_blank(), # no legend title
            legend.text = element_text(size = 12, color = "#333333"),
            text = element_text(size = 12, family = "Times New Roman"), 
            # font aesthetics
            axis.text = element_text(size = 12, color = "#333333"),
            # axis.title = element_text(size = 1, color = "#333333")#,
            legend.position = "top",
            # legend.justification = "left",
            plot.title.position = "plot",
            #   plot.caption = element_text(hjust=0),
            plot.caption.position = "plot"
        )
}





###

library(palmerpenguins)
library(ggthemes)
ggplot(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
    geom_point()
#> Warning: Removed 2 rows containing missing values or values outside the scale range
#> (`geom_point()`).

linear <- ggplot(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
    geom_point() +
    stat_smooth(aes(y=body_mass_g), method = "lm")

ggplot(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
    geom_point() +
    stat_smooth(aes(y=body_mass_g), method = "lm", 
        formula = y ~ x + I(x^2))  

