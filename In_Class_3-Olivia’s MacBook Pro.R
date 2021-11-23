#Prepare Dataset
install.packages("palmerpenguins")
install.packages("here")
require("palmerpenguins")
require("here")
class(penguins)
head(penguins)
#set variable penguins
penguins = data.frame(penguins)
#data exploration--numerical exploration
mean(penguins$body_mass_g)
#NA returned... head shows missing data. 
mean(penguins$body_mass_g, na.rm = TRUE)
#na.rm tells R whether or not to remove null data rows
summary(penguins)
#graphical exploration: plots
#scatterplot
plot(penguins$bill_length_mm, penguins$bill_depth_mm)
#scatterplot matrix
plot(penguins)
#boxplot
boxplot(penguins$bill_depth_mm)
#conditional boxplot... sex is conditional variable
boxplot(bill_depth_mm ~ sex, data = penguins)
#or you can view side-by-side...
par(mfrow = c(1, 2))
boxplot(penguins$bill_depth_mm)
boxplot(bill_depth_mm ~ sex, data = penguins)
#coplots
coplot(body_mass_g ~ bill_depth_mm | sex, data = penguins)
coplot(body_mass_g ~ bill_depth_mm | species, data = penguins)
coplot(body_mass_g ~ bill_depth_mm | flipper_length_mm, data = penguins)
require(here)
png(filename = here("basic_histogram_titled.png"), width = 800, height = 600)
hist(penguins$body_mass_g, main = "Penguin Body Mass")
dev.off()