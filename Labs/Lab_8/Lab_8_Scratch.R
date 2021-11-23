require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

#TWO BOOTS???
install.packages("simpleboot")

require(simpleboot)

adelie = subset(penguins, species == "Adelie")
chinstrap = subset(penguins, species == "Chinstrap")
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}
penboot = two.boot(adelie$flipper_length_mm, 
         chinstrap$flipper_length_mm, 
         boot_mean, 10000)
print(penboot)
str(penboot)

hist(penboot)

#tree data

require(here)
veg = read.csv(here("data", "vegdata.csv"))
boxplot(pine ~ treatment, dat = veg)

dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))

boxplot(pine ~ treatment, data = dat_tree)

table(subset(dat_tree, treatment == "clipped")$pine)
      
wilcox.test(pine ~ treatment, data = dat_tree)
#what is the p value?
  #.1005

require(boot)

#Bootstrap

tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 1000,
    na.rm = TRUE
    )
  )
boot.ci(tree_boot)

hist(tree_boot$t, main = "Bootstrap sampling distribution")

quantile(tree_boot$t, .025)

#Bird Data
dat_bird = read.csv(here("data", "bird.sub.csv"))
dat_habitat = read.csv(here("data", "hab.sub.csv"))

dat_all = merge(
  dat_bird,
  dat_habitat,
  by = c("basin", "sub"))

head(dat_all[, c("b.sidi", "s.sidi")])

#z standardization

#calculate sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, ra.rm = TRUE)
b_sidi_sd = sd(dat_all$b.sidi, na.rm = TRUE)

#use the subset-by-name symbol ($) to create a 
#new column of z-standardized values
dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd

mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

#model variables

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

#fit a simple linear regression

fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

dat_1 =
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))
#is this negative relationship real? let's test with a null hypothesis!

#monte carlo resampling

index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i =
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]
print(slope_resampled_i)

plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

#randomization loop
m = 10000
result = numeric(m)

for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i =
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  result[i] = coef(fit_resampled_i)[2]
}
print(result)

hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

quantile(result, c(.05))

t.test(result, mu = slope_observed, alternative = "greater")

sum(slope_observed >= result)
