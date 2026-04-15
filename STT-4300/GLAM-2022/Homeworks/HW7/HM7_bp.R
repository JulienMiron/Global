rm(list = ls())

# Loading libraries
library(MASS)
library(lme4) # library glmmML could also be used
library(gee)

# Loading data
data(bacteria)
help(bacteria)
head(bacteria)

# Some general data exploration
mosaicplot(trt ~ y, data = bacteria,
           xlab = "treatment",
           ylab = "presence of bacteria")
# Check proportion of kids with bacteria as a function of week
plot(y ~ week,
     data = bacteria,
     ylab = "Presence of bacteria",
     main = "All subjects")
# Only for kids with treatment
plot(y ~ week,
     data = bacteria[bacteria$trt != "placebo", ],
     ylab = "Presence of bacteria",
     main = paste( sum(bacteria$trt != "placebo"), "Subjects with treatment"))
# Only for kids with placebo
plot(y ~ week,
     data = bacteria[bacteria$trt == "placebo", ],
     ylab = "Presence of bacteria",
     main = paste( sum(bacteria$trt == "placebo"), "Subjects with placebo"))

# Something may have happened in week 2...

# Plot the evolution for few kids
for (id in unique(bacteria$ID)[c(8, 12, 18, 20, 33, 42)]) {
  index <- which(bacteria$ID == id)
  plot(as.numeric(y == "y") ~ week,
       data = bacteria[index, ],
       type = "b",
       main = paste(id, ":", unique(bacteria[index, "trt"])),
       ylab = "bacteria")
}


# Why not fit a GEE ?
# Make the response a 0 / 1 variable instead of n / y
bacteria$b <- as.numeric(bacteria$y) - 1
# Fit the gee
gee_fit <- gee(b ~ trt + week,
               id = ID,
               data = bacteria,
               family = binomial,
               corstr = "exchangeable",
               scale.fix = TRUE,
               scale.value = 1)
# scale.fix = TRUE and scale.value = 1 prevents the gee(.) function to fit 
# an overdispersion parameter (scale) by setting it to 1.

summary(gee_fit)
# Check the significance of coefficients
summary(gee_fit)$coefficients[, c(1, 5)]
# compare the robust Z to quantile of a standard normal
qnorm(0.975)

# Plot Pearsons residuals
plot(gee_fit$fitted.values, 
     gee_fit$residuals / sqrt(gee_fit$fitted.values*(1-gee_fit$fitted.values)),
     xlab = "Fitted values (probabilities)",
     ylab = "Pearson's residuals")
# Pearson residuals must be computed by hand since gee_fit$residuals are the 
# unstandardized residuals y_i - p_i.
# observation 33 has a very low residual value, maybe outlier.

# No AIC available for GEE ... but ....
- 2 * sum(log(gee_fit$fitted.values)) + 2 * 5

# Fit GLMM
# Let's use GLMM with lme4 package (Gaussian quadrature)
help(glmer)
fit_glmm <- glmer(y ~ trt + week + (1|ID),
                  family = binomial,
                  data = bacteria)
# random effect are specified by the ( | ). Here we have a 
# constant (1) random effect per ID. We could also do a (week|ID) random effect
# Meaning a random coefficient for week per ID.

summary(fit_glmm) # The fit is better than GEE.

plot(fit_glmm)

# Check the random effect estimates (just out of curiousity)
ranef(fit_glmm)

# Plot fitted value vs week with a different color for each treatment,
# and some jitter for clarity.
plot(bacteria$week + rnorm(120, 0, 0.1), fitted(fit_glmm),
     col = as.numeric(bacteria$trt),
     ylim = c(0, 1), pch = 19,
     xlab = "Week (jittered)", ylab = "Fitted value (probability)")
legend("bottomleft", col = 1:3, legend = levels(bacteria$trt), pch = 19, bty = "n")

# Try a different model 
fit_glmm2 <- glmer(y ~ trt + I(week > 2) + (1|ID),
                   family = binomial,
                   data = bacteria)

summary(fit_glmm2) # This is the best fit (AIC is lowest)

plot(fit_glmm2)
# check random effect (... why not...?)
ranef(fit_glmm)


# Fit a simple GLM
fit_glm <- glm(y ~ trt + week, data = bacteria, family = binomial)
summary(fit_glm)
plot(fit_glm)


fit_glm2 <- glm(y ~ trt + I(week > 2), data = bacteria, family = binomial)

# The AIC are bigger than for the GLMM
AIC(fit_glm2)
AIC(fit_glm)


######### Exercise 2

# Load data
seiz <- read.table('seizure.txt', header = T)
head(seiz)

#create new variable "post treatment" equal to 0 when time <= 8 and 1 otherwise
seiz$post <- rep(0,295)
seiz$post[seiz$Time>8] <- 1
seiz$post # dummy: 0 = baseline, 1 = post-treatment

# Fit GEE
fit_gee_seiz <- gee(formula = y ~ post * factor(group) + offset(log(rep(c(8, 2, 2, 2, 2), 59))),
                    id = Subject,
                    data = seiz,
                    family = poisson,
                    corstr = "exchangeable")
# the offset is to take into account the non constant length of time intervals. 

# Fit GLMM with a random intercept per subject.
fit_glmm_seiz <- glmer(y ~ post * factor(group) + offset(log(rep(c(8, 2, 2, 2, 2), 59))) + (1|Subject),
                       family = poisson,
                       data = seiz)
# Plot Pearson residuals (have to compute them by hand for the GEE model)
plot(fit_gee_seiz$fitted.values,  
     fit_gee_seiz$residuals / sqrt(fit_gee_seiz$scale * fit_gee_seiz$fitted.values),
     xlab = "Fitted value", ylab = "Pearson's residual")

plot(fit_glmm_seiz)
# Residuals of GLMM are more centred and less extreme : the model is better.