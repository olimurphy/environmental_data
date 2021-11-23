install.packages("here")
require(here)
# Create a ricker function :)
ricker_fun = function(x, a, b)
{
  return(a * x * exp(-b * x))
}

#and plot it!

curve(
  ricker_fun(x,.5,.5),
  # from and to sets the range of x inputs, add = false makes a new plot
  from = 0, to = 10, add = FALSE,
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")
  

# exponential function :)

exp_fun = function(x, a, b)
{
  return(a * exp(-b * x))
}
#plot time
curve(
  exp_fun(x, 2.2, 1/15), add = FALSE,
  from = 0, to = 50,
  #ann says whether there should be axis labels if not specified w/xlab or ylab
  ann = FALSE, axes = TRUE, 
  ylab = "f(x)")

#simulated data on a line

#set seed
set.seed(42069)

#specify x range and # points
n_pts = 50
x_min = 2
x_max = 10

#generate values

x_sim = runif(n_pts, min = x_min, max = x_max)

# choose intercepts and slope, generate "predicted" values

param_intercept = 2.3
param_slope = .67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")

# now plot with normal error

error_mean = 0
error_sd = .25

y_observed =
  y_pred +
  rnorm(
    n = n_pts,
    mean = error_mean,
    sd = error_sd)

plot(x_sim, y_observed, 
     main = "Normally Distributed Errors\nConstant Variance", 
     xlab = "", ylab = "")

# and another, more sophisticated stochastic model
  #in this one, sd is .1 * x value, so as x goes up, so does the sd

error_mean = 0
error_sd = .1

y_observed_2 =
  y_pred +
  rnorm(
    n = n_pts,
    mean = error_mean,
    sd = error_sd * x_sim)
  
par(mfrow = c(1,2))
plot(x_sim, y_observed, 
     main = "Normally Distributed Errors\nConstant Variance",
     xlab = "", ylab = "")
plot(x_sim, y_observed_2, 
     main = "Normally Distributed Errors\nIncreasing Variance",
     xlab = "", ylab = "")

#Exponentially distributed errors

y_observed_3 =
  y_pred +
  rexp(x_sim, rate = 1.2)


plot(x_sim, y_observed_3, 
     main = "Exponentially Distributed Errors",
     xlab = "", ylab = "")

#consider the three plots...

par(mfrow = c(3, 1))
plot(x_sim, y_observed)
plot(x_sim, y_observed_2)
plot(x_sim, y_observed_3)

#they all look linear... how could we try to determine the stochastic model?

par(mfrow = c(3, 1))
hist(y_observed - y_pred, main = "sim data 1", xlab = "observed y=values")
hist(y_observed_2 - y_pred, main = "sim data 2", xlab = "observed y=values")
hist(y_observed_3 - y_pred, main = "sim data 3", xlab = "observed y=values")