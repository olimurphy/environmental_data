# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "p")
abline(h = 0)

#Penguins example vs. random data
require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

#let's check out a normal distribution... 
#we need number of observations, mean, and sd
mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

#now let's generate some random normal data using that info!
dat_1 = rnorm(n = 344, mean = 4202, sd = 802)
dat_2 = rnorm(n = 344, mean = 4202, sd = 802)
dat_3 = rnorm(n = 344, mean = 4202, sd = 802)
dat_4 = rnorm(n = 344, mean = 4202, sd = 802)

#aaaand histogram time
par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)

#Random Uniform Numbers using runif()
set.seed(12)
dat_unif = runif(n = 2700, min = 0, max = 4)
hist(dat_unif)

#set.seed tells R to generate the same set of pseudorandom #s



#measuring error

set.seed(123)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)
dat = data.frame(x = x, y_observed = rnorm(n_pts))
plot(y_observed ~ x, data = dat, pch = 8)

#moar
# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
line_point_slope = function(x, x1, y1, slope)
{
  get_y_intercept = 
    function(x1, y1, slope) 
      return(-(x1 * slope) + y1)
  
  linear = 
    function(x, yint, slope) 
      return(yint + x * slope)
  
  return(linear(x, get_y_intercept(x1, y1, slope), slope))
}

set.seed(999)
n_pts = 10
x_min = 1
x_max = 10
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))

plot(y_observed ~ x, data = dat, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y_observed ~ x, data = dat, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = TRUE)

dat$y_predicted = line_point_slope(dat$x, guess_x, guess_y, guess_slope)
dat$resids = (dat$y_observed - dat$y_predicted)

sum(dat$resids)

dat$abs_resids = abs(dat$resids)
sum(dat$abs_resids)



set.seed(666)
n_pts = 13
x_min = 1
x_max = 25
x = runif(n = n_pts, min = x_min, max = x_max)
dat1 = data.frame(x = x, y_observed = rnorm(n_pts))

set.seed(420)
n1_pts = 69
x = runif(n = n1_pts, min = x_min, max = x_max)
dat2 = data.frame(x = x, y_observed = rnorm(n1_pts))
#4 plots <3
png(here("Labs", "Lab_4", "Four_Beautiful_Plots.png"),
    width = 800, height = 600,
     units = "px")
par(mfrow = c(2,2))

hist(dat2$x, 
     main = "69-Point Histogram", xlab = "Random Numbers (Seed = 420)", 
     col =
       rgb(255, 209, 220, maxColor = 255))

plot(dat1$x, dat1$y_observed, 
     main = "Dat1 Scatterplot, 13 Points",
     xlab = "X", ylab = "rnorm(x)",
     pch = 13, col = "steelblue")

boxplot(dat2$y_observed, 
        main = "Dat2 rnorm(x) Boxplot",
        col = "yellow")

plot(dat2$x, dat2$y_observed,
     main = "Dat 2 Scatterplot, 69 Points",
     xlab = "X", ylab = "rnorm(x)",
     col =
       rgb(200, 162, 200, maxColor = 255),
     pch = 4)
dev.off()

