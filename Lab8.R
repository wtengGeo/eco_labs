


two.boot <- function(sample1, sample2, FUN, R, student = FALSE, M,
                     weights = NULL, ...) {
  func.name <- ifelse(is.character(FUN), FUN, deparse(substitute(FUN)))
  func <- match.fun(FUN)
  ind <- c(rep(1, length(sample1)), rep(2, length(sample2)))
  nobsgrp <- as.numeric(table(ind))
  extra <- list(...)
  
  if(func.name == "quantile") {
    if(is.na(match("probs", names(extra))))
      stop("'probs' argument must be specified")
    if(length(extra$probs) > 1)
      stop("can only bootstrap a single quantile")
  }
  boot.func <- function(x, idx) {
    d1 <- x[idx[ind == 1]]
    d2 <- x[idx[ind == 2]]
    fval <- func(d1, ...) - func(d2, ...)
    
    if(student) {
      b <- two.boot(d1, d2, FUN, R = M, student = FALSE,
                    M = NULL, weights = NULL, ...) 
      fval <- c(fval, var(b$t))
    }
    fval
  }
  if(!is.null(weights))
    weights <- unlist(weights)
  b <- boot(c(sample1, sample2), statistic = boot.func, R = R,
            weights = weights, strata = ind)
  b$student <- student
  structure(b, class = "simpleboot")
}



library(palmerpenguins)
penguin_dat = subset(penguins, species != "Gentoo")

t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")

# install.packages("simpleboot")
library(boot)
Adelie_dat = subset(penguin_dat, species == "Adelie")
Chinstrap_dat = subset(penguin_dat, species == "Chinstrap")
# https://cran.r-project.org/web/packages/simpleboot/simpleboot.pdf
pen_boot <- two.boot(Adelie_dat$flipper_length_mm, Chinstrap_dat$flipper_length_mm, mean, R = 10000)
str(pen_boot)
pen_boot$data
sd(pen_boot$data, na.rm = TRUE)
mean(pen_boot$data, na.rm = TRUE)
hist(pen_boot$data)
quantile(pen_boot$t, c(0.025, 0.975), na.rm = TRUE)
hist(pen_boot$t)  ## Histogram of the bootstrap replicates

sum(diff(na.omit(pen_boot$data))>=-4.5)
sum(diff(na.omit(pen_boot$data))<=-8)


pen_ecdf = ecdf(diff(na.omit(pen_boot$data)))
pen_ecdf(-0.5)







veg = read.csv(here::here("data", "vegdata.csv"))
boxplot(pine ~ treatment, dat = veg)

dat_tree = droplevels(subset(veg, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = dat_tree)
table(dat_tree$treatment)

wilcox.test(pine ~ treatment, dat = dat_tree)


# t.test(pine ~ treatment, dat = dat_tree, alternative = "less")
control_dat = subset(dat_tree, treatment == "control")
clipped_dat = subset(dat_tree, treatment == "clipped")
tree_boot<- two.boot(clipped_dat$pine,control_dat$pine, mean, R = 10000)
str(tree_boot)
boot.ci(tree_boot)
quantile(tree_boot$t, c(0.025, 0.975))
hist(tree_boot$t)

mean(diff(dat_tree$pine))






require(here)
dat_bird = read.csv(
  here("data", "bird.sub.csv")
)
dat_bird
dat_habitat = read.csv(
  here("data", "hab.sub.csv")
)
dat_habitat
# I already read my data into dat_bird and dat_habitat:
dat_all = merge(
  dat_bird, 
  dat_habitat,
  by = c("basin", "sub"))
dat_all
head(dat_all[, c("b.sidi", "s.sidi")])


# Calculate the sample mean and sd:
b_sidi_mean = mean(dat_all$b.sidi, na.rm = TRUE)
b_sidi_sd   = sd(dat_all$b.sidi, na.rm = TRUE)

# Use the subset-by-name symbol ($) to create a 
# new column of z-standardized values.
# dat_all
dat_all$b.sidi.standardized = (dat_all$b.sidi - b_sidi_mean)/b_sidi_sd
mean(dat_all$b.sidi.standardized)
sd(dat_all$b.sidi.standardized)

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

# The slope coefficient is labeled s.sidi since that is the predictor we specify in the model.
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)
slope_observed = coef(fit_1)[2]
slope_observed
plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)


dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

set.seed(123)
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)
plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices (MC resampled data)",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)


m = 10000 
result_mc = numeric(m) 
result_mc
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i) 
  
  result_mc[i] = coef(fit_resampled_i)[2]
}
hist(
  result_mc,
  main = "Wenxiu's Null Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
abline(v = quantile(result_mc, c(.05)), lty = 2, col = "red", lwd = 2)
slope_observed
#lower critical value
quantile(result_mc, c(.05))
# p-value for this lower one-side test
sum(result_mc<slope_observed)/m











set.seed(345)
index_1 = sample(nrow(dat_1), replace = TRUE)

dat_boot = dat_1[index_1, ]
head(dat_boot)

fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)

coef(fit_bs1)


m = 10000 
result_boot = numeric(m) 
result_boot 
for(i in 1:m)
{
  index_1 = sample(nrow(dat_1), replace = TRUE)
  
  dat_boot = dat_1[index_1, ]
  head(dat_boot)
  
  fit_bs1 = lm(b.sidi ~ s.sidi, data = dat_boot)
  
  # coef(fit_bs1)
  
  result_boot[i] = coef(fit_bs1)[2]
}



hist(
  result_boot,
  main = "Wenxiu's Alternative Distribution of Regression Slope",
  xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)
abline(v = 0, lty = 2, col = 1, lwd = 2)



plot(
  density(result_mc),
  main = "Wenxiu's Null Distribution Density Plot",
  xlab = "Slope Coefficient")
plot(
  density(result_boot),
  main = "Wenxiu's Null Distribution Density Plot",
  xlab = "Slope Coefficient")

lines(density(result_boot),col = "red")
lines(density(result_mc),col = "blue")
# legend(x = "topright",bty = "n")

legend("topright", legend=c("Null", "Alt."),
       col=c("red", "blue"),bty = "n",lty = 1, lwd = 1)
