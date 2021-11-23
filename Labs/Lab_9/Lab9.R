#Catastrophic Rate Data
require(here)
catrate = read.csv(here("data", "catrate.csv"))
head(catrate)

n_success = sum(catrate$success)
n_years = sum(catrate$years)
binom.test(n_success, n_years)

late_fill_rate = 2/7
normal_fill_rate = 1 - late_fill_rate

binom.test(
  n_success,
  n_years,
  p = normal_fill_rate
)

binom.test(
  n_success,
  n_years,
  p = normal_fill_rate,
  alternative ='less')

veg = read.csv(here("data", "vegdata.csv"))
head(veg)

boxplot(pine ~ treatment, data = veg)
 # f test compares variance. f statistic represents
## ratio between two variances
var.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control', 'clipped')
)

# f test assumes normality

shapiro.test(veg$pine[veg$treatment=="control"])

shapiro.test(veg$pine[veg$treatment == "clipped"])

#non parametric variance test

fligner.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control','clipped')
)

bartlett.test(pine ~ treatment, data = veg)

fligner.test(pine ~ treatment, data = veg)

t.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control', 'clipped')
)

wilcox.test(
  pine ~ treatment,
  data = veg,
  subset = treatment %in% c('control', 'clipped')
)

control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']

t.test(control, clipped, paired = TRUE)

wilcox.test(control, clipped, paired = TRUE)

#Correlation

disp = read.csv(here("data", "dispersal.csv"))
disp

plot(disp$disp.rate.ftb, disp$disp.rate.eb)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use = 'complete.obs'
)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

plot(
  ecdf(disp$disp.rate.ftb),
  verticals = TRUE
)

plot(
  ecdf(disp$disp.rate.eb),
  verticals = TRUE,
  lty = 3,
  add = TRUE
)

ks.test(disp$disp.rate.ftb, disp$disp.rate.eb)

prop.test(c(4,16),c(40,250))

owls = matrix(c(16,9,4,11), nrow = 2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
chisq.test(owls)

fisher.test(owls)

#bird habitat data

birds = read.csv(here("data","bird.sta.csv"))
hab = read.csv(here("data", "hab.sta.csv"))
birdhab = merge(birds, hab, by = c("basin", "sub", "sta"))

# create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

br_creeper_table
