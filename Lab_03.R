# install.packages("tmvnsim")
# install.packages("psych")
# # install.packages("psych",denpendencies = TRUE)
require(psych)
# ## Loading required package: psych
pairs.panels(iris)
names(iris)
pairs.panels(iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length")])

# Function to calculate the logistic parameter a given the slope and midpoint
get_logistic_param_a = function(slope, midpoint)
{
  b = slope / 4
  return (-midpoint * (slope / 4))
}

# Function to calculate the logistic parameter b given the slope
get_logistic_param_b = function(slope)
{
  return (slope / 4)
}


# Calculate the value of the logistic function at x, given the parameters a and b.
logistic = function(x, a, b)
{
  val = exp(a + b * x)
  return(val / (1 + val))
}

# Calculate the value of the logistic function at x, given a slope and midpoint.
logistic_midpoint_slope = function(x, midpoint, slope)
{
  b = get_logistic_param_b(slope)
  a = get_logistic_param_a(slope, midpoint)
  return(logistic(x, a, b))
}


require(here)
dat_bird = read.csv(
  here("data", "bird.sta.csv")
)
head(dat_bird)

dat_hab = read.csv(
  here("data", "hab.sta.csv")
)
head(dat_hab)


dat_all = merge(dat_bird,dat_hab)

plot(ba.tot ~ elev, data = dat_all)

# cewa = sample(dat_all$CEWA, 100)
# cewa_bool = cewa>1
cewa_present_absent = as.numeric(dat_all$CEWA>1)
cewa_present_absent
plot(x = dat_all$elev, y = cewa_present_absent)

plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400+100, slope = 0.1+0.1), add = TRUE)


plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.1), add = TRUE)


plot(x = dat_all$elev, y = cewa_present_absent)
curve(logistic_midpoint_slope(x, midpoint = 400, slope = -0.005), add = TRUE)




# Q1
pairs.panels(dat_all[, c("elev", "slope", "aspect","ba.tot")])


# Q2
AMCR_present_absent = as.numeric(dat_all$MOQU>1)
AMCR_present_absent
plot(x = dat_all$ba.tot, y = AMCR_present_absent,     
     main="presence/absence of Mountain Quail vs basal area",
     xlab="basal area ", ylab="presence/absence of Mountain Quail")
curve(logistic_midpoint_slope(x, midpoint = 400-200, slope = 0.1+0.1), add = TRUE)


HUVI_present_absent = as.numeric(dat_all$HUVI>1)
HUVI_present_absent
plot(x = dat_all$ba.tot, y = HUVI_present_absent,
     main="presence/absence of Hutton's Vireo vs basal area",
     xlab="basal area ", ylab="presence/absence of Hutton's Vireo",)

curve(logistic_midpoint_slope(x, midpoint = 60, slope = 0.1+0.1), add = TRUE)


total_gray_jays = sum(dat_all$GRJA)
total_gray_jays

GRJA_present_absent = as.numeric(dat_all$GRJA>1)
GRJA_present_absent
present_count = sum(GRJA_present_absent>0)
present_count






#  Lec 3
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




par(mfrow = c(2,3))
hist(dat_all$elev,breaks = 16)
hist(dat_all$aspect,breaks = 19)
hist(dat_all$slope,breaks = 10)


plot(x = dat_all$elev, y = dat_all$ba.tot,col='blue',cex=1)
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.05), add = TRUE)

plot(x = dat_all$aspect, y = dat_all$ba.tot,col='blue',cex=1)
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.2), add = TRUE)

plot(x = dat_all$slope, y = dat_all$ba.tot,col='blue',cex=1)
curve(line_point_slope(x, x1 = 3.5, y1 = 1.25, slope = 0.2), add = TRUE)

dat_all






