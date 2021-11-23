require("here")
bird_dat = read.csv(here("assignments", "In_Class_Assignment", "likelihood", "bird.sta.csv"))
hab_dat = read.csv(here("assignments", "In_Class_Assignment", "likelihood", "hab.sta.csv"))
x_observed = c(2, 6)
print(x_observed)

dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)

dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)

wiwa_counts = c(2, 6)
dpois( x = wiwa_counts, lambda = 4.5)

prod(dpois(x = wiwa_counts, lambda = 4.5))

sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

dat_bird = read.csv(here("assignments", "In_Class_Assignment", "likelihood", "bird.sta.csv"))
dat_habitat = read.csv(here("assignments", "In_Class_Assignment", "likelihood", "hab.sta.csv"))
dat_all = merge(dat_bird, dat_habitat)

summary(dat_all$WIWA)

hist(dat_all$WIWA, breaks = 7)

wiwa_counts = c(2)
dpois(x = wiwa_counts, lambda = 4.5
      )
x_wiwa = c(0:10)
plot(x = x_wiwa, y = sum(log(dpois(wiwa_counts, x_wiwa))))

x_wiwa_2 = c(1.5, 1.75, 2, 2.25, 2.5, 2.75)
plot(x = x_wiwa_2, y = sum(log(dpois(2, x_wiwa_2))))

sum(log(dpois(2, 2)))

sum(log(dpois(2, 1.9)))
dev.off()
