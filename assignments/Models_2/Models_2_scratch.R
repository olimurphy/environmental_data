#recap of t tests

# 1-sample t-test

require(palmerpenguins)
t.test(subset(penguins, species == "Gentoo")$ flipper_length_mm)
t.test(subset(
  penguins, species == "Gentoo")$ flipper_length_mm,
  mu = 218     
  )
t.test(subset(
  penguins, species == "Gentoo")$ flipper_length_mm,
  mu = 218 ,
  alternative = "l"
)

#2-sample t test

t.test(flipper_length_mm ~ species, 
       data = subset(
         penguins, species != "Chinstrap"))

# 1-way ANOVA in R
## 1. Perform graphical and numerical data exploration
## 2. Fit a linear model using lm()
## 3. Examine the model coefficient table using summary()
## 4. Conduct the Analysis of Variance using anova()

par(mfrow = c(1,2))
hist(penguins$body_mass_g, breaks = 80, main = "histogram of body mass",
     xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = TRUE), main = "density plot of body mass")

#that shit's not normal!
dev.off()
boxplot(body_mass_g ~ species, data = penguins)

# testing normality across species using shapiro test/agg function

aggregate(
  body_mass_g ~ species, data = penguins,
  FUN = function(x) shapiro.test(x)$p.value
)

#fit a linear model

fit_species = lm(body_mass_g ~ species, data = penguins)

#anova it

anova(fit_species)
summary(fit_species)

# two tables: model coefficients and ANOVA

#Model coefficient table:
summary(fit_species)

#factor level p values is a significance test for whether the coefficient in 
#that row is different from 0
##note! doesn't tell us how they relate!

#Anova tables

##degrees of freedom
  # represents number of levels within cat variable
## sum of squares
  #says how much of total variability is explained by each predictor variable
  # residuals row shows variation the model couldn't explain
  # total sum of squares isn't printed; sum of column
##mean squares
  # sum sq adjested by DF for each predictor variable
## f statistic, p value
  # measure of how much adding that variable to the model improves fit
  # null hypothesis: adding predictor x does not improve model fit

# two-way additive anova

boxplot(body_mass_g ~ species + sex, data = penguins)

fit_additive = lm(body_mass_g ~ sex + species, data = penguins)

#two way interactive/factorial anova
  #factorial design means all combinations of variables appear as groups in the data
  ## so it looks at a, b, and ab

fit_interactive = lm(body_mass_g ~ sex * species, data = penguins)

summary(fit_interactive)

#simple linear regression: bills & body mass

lm(bill_length_mm ~ body_mass_g, data = penguins)
