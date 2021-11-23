pairs(iris)
pairs(iris[, c("Petal.Width", "Sepal.Width", "Sepal.Length")])
#histogram
hist(dat_bird$CBCH)
#histogram with better breaks
hist(dat_bird$CBCH, xlab = "Number of Birds Counted", breaks = 0:7 - .5)
#pygmy own histogram
hist(dat_bird$SOSP, xlab = "Number of Birds Counted", breaks = 0:7 - .5, main = "Song Sparrow Counts\nOllie Murphy", 
     col =
       rgb(255, 209, 220, maxColor = 255))
