

rm(list = ls())
sse_mean = function(x)
{
  x = x[is.na(x) == 0] 
  return(sd(x)/sqrt(length(x))) 
}

sse_mean(penguins$body_mass_g)
sse_mean(mtcars$mpg)




require(palmerpenguins)
sse_mean(penguins$bill_depth_mm)



boxplot(
  flipper_length_mm ~ species, data = penguins,
  ylab = "Flipper length (mm)")


dat_pen = subset(penguins, species != "Gentoo")
boxplot(
  flipper_length_mm ~ species, data = dat_pen,
  ylab = "Flipper length (mm)")



dat_pen = droplevels(subset(penguins, species != "Gentoo"))
{
  boxplot(
    flipper_length_mm ~ species, data = dat_pen,
    ylab = "Flipper length (mm)")
}



# Resampling with replacement  Monte Carlo Resampling

# for reproducibility
set.seed(123)

flipper_shuffled = sample(
  penguins$flipper_length_mm, replace = TRUE)

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_shuffled ~ penguins$species,
    ylab = "Flipper length (mm)",
    main = "MonteCarlo Resampled Data",
    xlab = "species")
}

#Bootstrap Resampling and Alternative Hypotheses

penguins2 = penguins[sample(1:nrow(penguins), replace = T), ]

{
  par(mfrow = c(1, 2))
  boxplot(
    flipper_length_mm ~ species, data = penguins,
    ylab = "Flipper length (mm)",
    main = "Original Data")
  boxplot(
    flipper_length_mm ~ species, data = penguins2,
    ylab = "Flipper length (mm)",
    main = "Bootstrap Data")
}


# Repeated MC Resampling

par(mfrow = c(4, 4), mar = c(1, 1, 1, 1))
for (i in 1:16)
{
  
  flipper_shuffled = sample(
    penguins$flipper_length_mm, replace = TRUE)
  
  boxplot(
    flipper_shuffled ~ penguins$species,
    ann = F, axes = F)
  box()
  
}

# T-tests - A Frequentist Approach
t.test(dat_pen$flipper_length_mm ~ dat_pen$species)


# resampling version of a t-test on penguin flipper length.
# Reset the random number generator state for reproduceablility
set.seed(1)
flipper_shuffled = sample(dat_pen$flipper_length_mm)
boxplot(flipper_shuffled ~ dat_pen$species)

t_test_1 = t.test(flipper_shuffled ~ dat_pen$species)
t_test_1



# Difference of means
# The t-test great for comparing the means of two groups.
# We can see the group means in the t-test output:
t_test = t.test(dat_pen$flipper_length_mm ~ dat_pen$species)
t_test
t_test$estimate
diff_observed = round(diff(t_test$estimate), digits = 3)
print(diff_observed, digits = 3)
# Difference of means- Using aggregate()
agg_means = aggregate(
  flipper_length_mm ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_observed = diff(agg_means[, 2])

agg_means
diff_observed


two_group_resample_diff = function(x, n_1, n_2)
{
  # table(dat_pen$species)
  # n_1 = 68
  # n_2 = 152
  x = x[is.na(x) == 0] 
  dat_1 = sample(x, n_1, replace = TRUE)
  dat_2 = sample(x, n_2, replace = TRUE)
  
  diff_simulated = 
    mean(dat_1, na.rm = TRUE) - mean(dat_2, na.rm = TRUE)
  return(diff_simulated)

}
set.seed(54321)
two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)


par(mfrow = c(1, 1))
n = 200000000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$flipper_length_mm, 68, 152)
  )
}
hist(mean_differences)
sum(abs(mean_differences)>5.8)

sum(abs(mean_differences) >= diff_observed)







# dat_pen = subset(penguins, species != "Gentoo")
boxplot(
  body_mass_g ~ species, data = dat_pen,
  ylab = "body_mass_g")

agg_means = aggregate(
  body_mass_g ~ species, 
  data = dat_pen, 
  FUN = "mean", 
  na.rm = TRUE)
diff_crit = diff(agg_means[, 2])
diff_crit
agg_means
t_test = t.test(dat_pen$body_mass_g ~ dat_pen$species)
t_test


table(dat_pen$species)
par(mfrow = c(1, 1))
n = 1000
mean_differences = c()
for (i in 1:n)
{
  mean_differences = c(
    mean_differences,
    two_group_resample_diff(dat_pen$body_mass_g, 68, 152)
  )
}
hist(mean_differences)

sum(abs(mean_differences)>diff_crit)

# sum(abs(mean_differences) >= diff_observed)
