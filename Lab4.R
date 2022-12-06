# Calculates the value of y for a linear function, given the coordinates
# of a known point (x1, y1) and the slope of the line.
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


# Generate a vector of x-values
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)


require(palmerpenguins)
hist(
  penguins$body_mass_g,
  main = "Histogram of Penguin Body Mass",
  xlab = "Body Mass (g)")

mean(penguins$body_mass_g, na.rm = TRUE)
sd(penguins$body_mass_g, na.rm = TRUE)
nrow(penguins)

# mean(penguins$body_mass_g, na.rm = FALSE)
# sd(penguins$body_mass_g, na.rm = FALSE)

n_samples = 344
pop_sd = 802
pop_mean = 4202


dat_1 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_2 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_3 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)
dat_4 = rnorm(n = n_samples, mean = pop_mean, sd = pop_sd)


par(mfrow = c(2, 2))

hist(dat_1)
hist(dat_2)
hist(dat_3)
hist(dat_4)


dat_unif = runif(n = 270, min = 0, max = 4)
hist(dat_unif)


set.seed(1)
dat_unif_1 = runif(n = 270, min = 0, max = 4)
set.seed(1)
dat_unif_2 = runif(n = 270, min = 0, max = 4)

par(mfrow = c(1, 2))
hist(dat_unif_1)
hist(dat_unif_2)




set.seed(123)
n = 17
slope = 0.7
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 10)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)




# par(mfrow = c(1, 1))
set.seed(1)
n_pts = 10
x_min = 1
x_max = 10

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)

# Y values are normally-distributed.
# I used the default parameters for mean and sd.
y_random = rnorm(n = n_pts)

dat_random = data.frame(x = x_random, y = y_random)
dat_random
plot(y ~ x, data = dat_random, pch = 8)

guess_x = 6
guess_y = 0
guess_slope = 0.1

plot(y ~ x, data = dat_random, pch = 8)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
y_predicted = line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

dat_random = cbind(dat_random, y_predicted)
resids = dat_random$y_predicted-dat_random$y
dat_random = cbind(dat_random, resids)
dat_random
sum(abs(dat_random$resids))
par(mfrow = c(1, 2))
plot(resids ~ y_predicted , data = dat_random, pch = 8)

hist(dat_random$resids)





my_mean =  10.4
my_sd = 2.4
norm_17  = rnorm(n = 17, mean = my_mean, sd = my_sd)
norm_17
norm_30 = rnorm(n = 30, mean = my_mean, sd = my_sd)
norm_300 = rnorm(n = 300, mean = my_mean, sd = my_sd)
norm_3000 = rnorm(n = 3000, mean = my_mean, sd = my_sd)

png("lab_04_hist_01.png",  width = 1500, height = 1600, 
    res = 180, units = "px")
par(mfrow = c(2, 2))
hist(norm_17,main = paste("number of points", 17))
hist(norm_30,main = paste("number of points", 30))
hist(norm_300,main = paste("number of points", 300))
hist(norm_3000,main = paste("number of points", 3000))
dev.off()



# Generate a vector of x-values
x = seq(-6, 6, length.out = 1000)
# dnorm: density function of the normal distribution
# rnorm: random sampling from the normal distribution
y = dnorm(x)

plot(x, y, main = "Standard Normal PDF", type = "l", xlim = c(-3, 3))
abline(h = 0)

# svg("norm_1.svg")
# pdf(file = here("norm_1.pdf"))
pdf("norm_1.pdf")
my_mean =  10.4
my_sd = 2.4
# Generate a vector of x-values
x = seq(0, 20, length.out = 1000)
y = dnorm(x,mean = my_mean, sd = my_sd,log = FALSE)
plot(x, y, main = paste("Normal PDF mean=10.4;sd=2.4"), type = "l", 
     xlim = c(0, 20),ylim = c(0, 0.2))
abline(h = 0)

dev.off()






png("Q10.png",  width = 1500, height = 1600, 
    res = 180, units = "px")
par(mfrow = c(2, 2))

set.seed(1)
n_pts = 100
x_min = 1
x_max = 30

# X values are uniformly distributed
x_random = runif(n = n_pts, min = x_min, max = x_max)
y_random = rnorm(n = n_pts)
dat_random = data.frame(x = x_random, y = y_random)

plot(y ~ x, data = dat_random,main = paste("scatterplots"),col="steelblue")

hist(dat_random$x,main = paste("hist"),col="steelblue")
boxplot(x,data=dat_random, main="boxplot",col="steelblue")
barplot(dat_random$x, main="barplot",col="steelblue")
dev.off()




png("Q14.png",  width = 1500, height = 1600, 
    res = 180, units = "px")

# par(mfrow = c(2, 2))

set.seed(1)


n = 17
slope = 0.8
intcp = 0.2

guess_x = 6
guess_y = 4
guess_slope = 0.72

x = runif(n = n, min = 1, max = 20)
y = rnorm(n = n, mean = slope * x + intcp)

plot(x, y, pch = 16)
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)
y_predicted = line_point_slope(dat_random$x, guess_x, guess_y, guess_slope)

dat_random = cbind(dat_random, y_predicted)
resids = dat_random$y_predicted-dat_random$y
dat_random = cbind(dat_random, resids)
dat_random
sum(abs(dat_random$resids))
par(mfrow = c(1, 2))
plot(resids ~ y_predicted , data = dat_random, main = paste("scatterplots of resids vs. y_predicted"),pch = 8)

hist(dat_random$resids)

dev.off()
