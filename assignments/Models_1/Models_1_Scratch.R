require(here)
catrate = read.csv(here("data", "catrate.csv"))
summary(catrate)
hist(catrate$cat.rate, main = "Histogram of Catastrophe Rates", xlab = "Catastrophe Rate")

#check for normality

shapiro.test(catrate$cat.rate)
# null hypothesis for shapiro test is that data is normally dist.
  #p value of .04097 indicates strong evidence against null
  #therefor data likely not normal (duh)
install.packages("nortest")

#lets to a t test to see if the mean observed catastrophe rate (.54)
  #is sig different from the expected model rate .28

t.test(catrate$cat.rate, mu = (2/7))
#p value = .012, can likely reject null hypothesis

#now see about greater, not just equal to

t.test(catrate$cat.rate, mu = 2/7, alternative = "g")
#p value is now .00597, even smaller. 

t.test(catrate$cat.rate, mu = 2/7, alternative = "l")
#p value is now .994, inverse of the g value :)

#wilcox time <3

wilcox.test(catrate$cat.rate, mu = 2/7, alternative = "l")
wilcox.test(catrate$cat.rate, mu = 2/7, alternative = "g")
#very similar p values

require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

boxplot(
  flipper_length_mm ~ species, 
  data = penguin_dat,
  ylab = "Flipper Length (mm)"
)

dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")

shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)          

t.test(dat_adelie$flipper_length_mm, dat_chinstrap$flipper_length_mm)
t.test(flipper_length_mm ~ species, data = penguin_dat)
wilcox.test(flipper_length_mm ~ species, data = penguin_dat)
