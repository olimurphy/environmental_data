# apply() function
# applies a function to either rows or datas of input data

#arguments:
  #X the 2-dimensional data, usually a data frame or a matrix
  #MARGIN whether to apply the function to rows (MARGIN = 1) or columns (MARGIN = 2)
  #FUN The function to apply to the rows or columns

#create simulated data
dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

#min and max per row with apply()
apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN = 1, FUN = max)

#mean for each column
apply(dat, MARGIN = 2, FUN = mean)

#moth data set :)

require(here)
moths = (read.csv(here("data", "moths.csv")))
head(moths)

#THE BOOTSTRAP

hist(moths$anst)

#ok calculate CI, start with SSE

sse_mean = function(x)
{
  sd(x, na.rm = TRUE) / sqrt(length(x) - sum(is.na(x)))
}

sse_anst = sse_mean(moths$anst)

# critical t-values for alpha = .05, 
  #multiply by SSE to get CI radius

qt(.025, df = 23)
qt(.975, df = 23) * sse_anst

#mean +- CI radius is CI
mean(moths$anst)

#parametric CI = 2.47 +- 1.53


#Bootstrap CI
# numeric() creates an vector of length m 
  #with all values initialized to zero
m = 10000
result = numeric(m)
head(result)

for(i in 1:m)
{
  result[i] = mean(sample(moths$anst, replace = TRUE))
}

#calculate mean and quantiles for bootstrap
mean(result)
quantile(result,c(.025,.975))

#bootstrap interval using boot()

install.packages("boot")
require(boot)

#arguments
  # boot(data, statistic, R)
    #data is data object to resample--vector, matrix, data frame
    #statistic is a function that returns the stat of interest
    # R is number of iterations

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

myboot =
    boot(
      data = moths$anst,
      statistic = boot_mean,
      R = 10000)
print(myboot)

str(myboot)

#Find bootstrap CI:
quantile(myboot$t, c(.025,.975))

#Rarefaction Curve

#set up the bootstrap

moth_dat = moths[,-1]
head(moth_dat)
moth_dat

#create empty results matrix
n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(nrow = m, ncol = n)

#running the bootstrap simulation

n = nrow(moth_dat)

m = 100

moth_result = matrix(
  nrow = m,
  ncol = n
)

#outer loop: runs once for each bootstrap iteration. index variable is i
for(i in 1:m)
{
  #inner loop simulates increasing sample intensity
  #sampling intensity ranges from 1 site to complete count of sites (24)
  #index variable is j
  for(j in 1:n)
  {
    #sample input data row indices with replacement
    rows_j = sample(n, size = j, replace = TRUE)
    #creates a new data matrix from resampled rows
    t1 = moth_dat[rows_j, ]
    #calculates the column sum of the new data matrix
    t2 = apply(t1, 2, sum)
    #counts number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)

#package the code

rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat)
  m = 100
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  for(i in 1:m)
  {
    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace = TRUE)
      t1 = moth_dat[rows_j, ]
      t2 = apply(t1, 2, sum)
      moth_result[i, j] = sum(t2>0)
    }
  }
  return(moth_result)
}
rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#second draft, replacing variables used elsewhere

rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_data)
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  for(i in 1:n_iterations)
  {
    for(j in 1:n)
    {
      rows_j = sample(n, size = j, replace = TRUE)
      t1 = input_dat[rows_j, ]
      t2 = apply(t1, 2, sum)
      results_out[i,j] = sum(t2>0)
    }
  }
  return(results_out)
}
rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

##rerun it in a clear environment
rm(list = ls())

# Re-read my data:
moths = read.csv(here("data", "moths.csv"))
moth_dat = moths[,-1]
 #HERE'S THE GOOD CODE <3
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  for(i in 1:n_iterations)
  {
    for(j in 1:n_input_rows)
    {
      rows_j = sample(n_input_rows, size = j, replace = TRUE)
      t1 = input_dat[rows_j, ]
      t2 = apply(t1, 2, sum)
      results_out[i,j] = sum(t2>0)
    }
  }
  return(results_out)
}
rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

#building the rarefaction curve

moths = read.csv(here("data", "moths.csv"))
rarefact = rarefaction_sampler(moths[,-1], 10000)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs = c(.025, .975))
rare = t(rbind(rare_mean, rare_quant))

#plotting the curve

matplot(
  rare,
  type = 'l',
  xlab = "Number of Sampling Plots",
  ylab = "Species Richness",
  main = "Rarefaction Curve"
  )
  
legend(
  "bottomright",
  legend = c("mean","2.5%", "97.5%"),
  lty = c(1,2,3), col = c(1,2,3), inset = c(.1,.1)
)
  
