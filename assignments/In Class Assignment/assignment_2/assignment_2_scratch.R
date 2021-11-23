data(iris)
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
plot(x = iris$Sepal.Width, y = iris$Sepal.Length)
data_center_x = mean(iris$Sepal.Width)
data_center_y = mean(iris$Sepal.Length)
c(data_center_x, data_center_y)
points(x = data_center_x, y = data_center_y, col = "red")
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
plot(x = iris$Sepal.Width, y = iris$Sepal.Length, main = "Ollie's Cool Flower Plot, xlab = "Sepal Width", ylab = "Sepal Length")
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -6), 
  add = TRUE)
title = "Ollie's Cool Flower Plot"
(xlab = "Sepal Width")
(ylab = "Sepal Length")
}
plot(x = iris$Sepal.Width, y = iris$Sepal.Length, main = "Ollie's Cool Flower Plot", xlab = "Sepal Width", ylab = "Sepal Length")
points(x = data_center_x, y = data_center_y, col = "red")
curve(
  line_point_slope(
    x, 
    data_center_x, 
    data_center_y,
    -6), 
  add = TRUE)