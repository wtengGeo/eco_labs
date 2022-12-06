
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

# Ricker Function
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}
exp_fun = function(x, a, b) 
{
  return(a * exp(-b * x))
}
# Note the use of the curve() function:
# The from and to arguments tells curve() what range of x-values it should include in the plot.
# The add = FALSE argument value creates a new plot.
curve(
  ricker_fun(x, 1, 1), 
  from = 0, to = 5, add = FALSE, 
  main = "Ricker function: a = 1, b = 1",
  ylab = "f(x)", xlab = "x")

curve(
  exp_fun(x, 2.2, 1/15), add = FALSE, from = 0, to = 50,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()





# Seed the RNG so we can reproduce our results
set.seed(1234567)

# Specify the x-range and number of points:
n_pts = 50
x_min = 2
x_max = 10

# Generate the x-values
x_sim = runif(n_pts, min = x_min, max = x_max)


param_intercept = 2.3
param_slope = 0.67
y_pred = param_intercept + x_sim * param_slope
plot(x_sim, y_pred, main = "Simulated Data\nNo Errors", xlab = "", ylab = "")









error_mean = 0
error_sd = 0.25

y_observed = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd)
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")


error_mean = 0
error_sd = 0.1

y_observed_2 = 
  y_pred + 
  rnorm(
    n = n_pts, 
    mean = error_mean, 
    sd = error_sd * x_sim)
y_observed_3 = 
  y_pred + 
  rexp(
    n = n_pts, 
    rate = 1.2)

par(mfrow = c(1, 3))
plot(x_sim, y_observed, main = "Normally Distributed Errors\n Constant Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_2, main = "Normally Distributed Errors\n Increasing Variance", xlab = "", ylab = "")
plot(x_sim, y_observed_3, main = "Exponentially Distributed Errors", xlab = "", ylab = "")


fit_1 = lm(y_observed ~ x_sim)
fit_2 = lm(y_observed_2 ~ x_sim)
fit_3 = lm(y_observed_3 ~ x_sim)

par(mfrow = c(1, 3))

plot(y_observed ~ x_sim); abline(fit_1)
plot(y_observed_2 ~ x_sim); abline(fit_2)
plot(y_observed_3 ~ x_sim); abline(fit_3)


hist(y_observed - x_sim)
hist(y_observed_2 - x_sim)
hist(y_observed_3 - x_sim)








# curve(
#   ricker_fun(x, 1, 1), 
#   from = 0, to = 5, add = FALSE, 
#   main = "Ricker function: a = 1, b = 1",
#   ylab = "f(x)", xlab = "x")



curve(
  exp_fun(x, 1.9,  0.1), 
  col = "black", lty = "solid",
  ylim=c(0,2),xlim=c(0,60),
  add = TRUE, from = 0, to = 60,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

curve(
  exp_fun(x, 1.9,  0.3), 
  col = "black", lty = "dotted",
  ylim=c(0,2),xlim=c(0,60),
  add = TRUE, from = 0, to = 60,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

curve(
  exp_fun(x, 1.2,  0.2), 
  col = "red", lty = "solid",
  ylim=c(0,2),xlim=c(0,60),
  add = TRUE, from = 0, to = 60,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()

curve(
  exp_fun(x, 1.2,  0.4), 
  col = "red", lty = "dotted",
  ylim=c(0,2),xlim=c(0,60),
  add = TRUE, from = 0, to = 60,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()





curve(
  ricker_fun(x, 25,  0.2), 
  col = "black", lty = "solid",
  ylim=c(0,100),xlim=c(0,10),
  add = TRUE, from = 0, to = 10,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()


curve(
  ricker_fun(x, 20,  0.2), 
  col = "black", lty = "dotted",
  ylim=c(0,100),xlim=c(0,10),
  add = TRUE, from = 0, to = 10,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()


curve(
  ricker_fun(x, 10,  0.2), 
  col = "black", lty = "dotted",
  ylim=c(0,100),xlim=c(0,10),
  add = TRUE, from = 0, to = 10,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()



curve(
  ricker_fun(x, 75,  0.3), 
  col = "red", lty = "solid",
  ylim=c(0,100),xlim=c(0,10),
  add = TRUE, from = 0, to = 10,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()


curve(
  ricker_fun(x, 50,  0.3), 
  col = "red", lty = "dotted",
  ylim=c(0,100),xlim=c(0,10),
  add = TRUE, from = 0, to = 10,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()


curve(
  ricker_fun(x, 40,  0.3), 
  col = "red", lty = "dotted",
  ylim=c(0,100),xlim=c(0,10),
  add = TRUE, from = 0, to = 10,
  ann = FALSE, axes = TRUE, ylab = "f(x)"); box()







par(mfrow = c(1, 1))
require(here)
dat_dispersal = read.csv(
  here("data", "dispersal.csv")
)
head(dispersal)

# plot(dispersal$disp.rate.ftb ~ dispersal$dist.class)
plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n linear model")
curve(line_point_slope(x, 800, 0.4, -0.0008), add = TRUE)



plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n ricker model")
curve(ricker_fun(x, 0.01, 1/250), add = TRUE)



plot(
  dat_dispersal$dist.class,
  dat_dispersal$disp.rate.ftb,
  xlim = c(0, 1500),
  xlab = "distance class", 
  ylab = "standardized dispersal rate", 
  main = "Marbled Salamander - first time breeders\n exponential model")
curve(exp_fun(x, 0.8, 0.005), add = TRUE)
# curve(exp_fun(x, 1500, 0.8, -0.00001), add = TRUE)

resids_linear = dat_dispersal$disp.rate.ftb - line_point_slope(dat_dispersal$dist.class, 800, 0.4, -0.0008)
resids_exp = dat_dispersal$disp.rate.ftb - exp_fun(dat_dispersal$dist.class, 0.8, 0.005)
resids_ricker = dat_dispersal$disp.rate.ftb - ricker_fun(dat_dispersal$dist.class, 0.01, 1/250)

data_resids = data.frame(resids_linear = resids_linear, resids_exp = resids_exp, 
                         resids_ricker = resids_ricker)

par(mfrow = c(1, 3))
hist(data_resids$resids_linear)
hist(data_resids$resids_exp)
hist(data_resids$resids_ricker)




# Create a sample of 50 numbers which are incremented by 1.
x <- seq(0,10,by = 1)

# Create the binomial distribution.
y <- dbinom(x,10,0.6)

# Plot the graph for this sample.
plot(x,y)
