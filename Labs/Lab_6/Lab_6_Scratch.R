sse_mean = function(x)
{
  sd(x, na.rm = TRUE) / sqrt(length(x) - sum(is.na(x)))
}
require(palmerpenguins)
sse_mean(penguins$bill_depth_mm)

#Penguin data and p-values

boxplot(flipper_length_mm ~ species, data = penguins)

#two species data

dat_pen = subset(penguins, species !="Gentoo")
boxplot(flipper_length_mm ~ species, data = dat_pen)

#but it still has the gentoo heading!! let's drop level

dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  par(mfrow = c(1,2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_length_mm ~ species, data = dat_pen)
}

##resampling with replacement

set.seed(123)

flipper_shuffled = sample(penguins$flipper_length_mm, replace = TRUE)
par(mfrow = c(1,2))
  boxplot(flipper_length_mm ~ species, data = penguins)
  boxplot(flipper_shuffled ~ penguins$species, xlab = "species")
  
#Classic T-Test
  
  t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
  
##suggests good evidence for difference in flipper length
  
  set.seed(1)
  flipper_shuffled = sample(dat_pen$flipper_length_mm)
  
  boxplot(flipper_shuffled ~ dat_pen$species)
  
  t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1  

t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)  
t_test

t_test$estimate
t_test$conf.int

diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)

# using aggregate()

agg_means = aggregate(
  flipper_length_mm ~ species,
  data = dat_pen,
  FUN = mean,
  na.rm = TRUE)
diff_observed = diff(agg_means[,2])

agg_means
diff_observed

# sample sizes

table(dat_pen$species)

n_1 = 68
n_2 = 152

dat_1 = sample(dat_pen$flipper_length_mm, n_1, replace = TRUE)
dat_2 = sample(dat_pen$flipper_length_mm, n_2, replace = TRUE)

diff_simulated =
  mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)

# resampling function

two_group_resample = function(x, n_1, n_2)
{
  mean(
    sample(x, n_1, replace = TRUE), na.rm = TRUE) - mean(
      sample(x, n_2, replace = TRUE), na.rm = TRUE)
}

set.seed(54321)
two_group_resample(dat_pen$flipper_length_mm, 68, 152)

#let's go it 200 times!!
dev.off()

n = 2000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences) >= diff_observed)

##retrieving named elements

str(t_test)


