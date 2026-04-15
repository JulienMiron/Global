# install.packages("gee")
library(gee)

# ==== Exercise 1 ====

# Out of many features of an ongoing LEI, we would like to identify which ones 
# predict its success.

# ---
# Outcome  
# ---

# SUCCESS   : ( = 0 or  =1) trainee i successfully performs a complete LEI 
#             in less than 30 seconds during trial $j$

# ---
# Covariates : 
# ---

# NECKFLEX  : neck flexion
# EXTOA     : atlanto-occipital extension
# PROPLGSP  : whether the trainee inserts the scope properly
# PROPLIFT  : whether the lift is performed successfully 
# ASKASSIS  : whether there is appropriate request for help 
# HELP      : whether there is unsolicited intervention by the attending 
#             anesthesiologist
# COMPS     : whether there are complications
# TRHAND    : the trainee's handedness
# TRGEND    : and the trainee's gender.

# ---
# Grouping identificator : 
# ---

# TRAINEE   : identifies the 19 trainees, who performed between 18 to 33 trials 
#             each. (n_i 's)
# TRIALCAT  : the trial's chronological position. 
#             = 1 trials 1-5, 
#             = 2 trials 6-10, etc.

# N = 436 LEI performed during this longitudinal study; 
# the file contains 443 rows but some observations are missing


# Load data
lei <- read.table('LEI.dat', header = T)
# Check it
head(lei)

# Look for NA
sum(is.na(lei)) 
# Remove NA
lei <- na.omit(lei)

#### Some exploration of data:
# Plot sequence of success for each trainee
for (trainee in unique(lei$TRAINEE)) {
  # select observations for which TRAINEE = trainee 
  index <- lei$TRAINEE == trainee
  # Plot success/failures vs trial
  plot(lei$TRIAL[index], lei$SUCCESS[index], type = "b", 
       xlab = "Trial", ylab = "Success of procedure" ,
       main = paste("Trainee", trainee), pch = 19)
}

# contingency table: success per trainee
tab <- table(lei$TRAINEE, lei$SUCCESS)
tab 

# Plot success rate per trainee using the contingency table
plot(1:19, tab[,2] / (tab[,2] + tab[,1]), 
     pch = 19,
     xlab = "Trainee", ylab = "Success rate", 
     ylim = c(0, 1))

# Some mosaic plots to check possible effects of covariates
mosaicplot(lei$EXTOA ~ lei$SUCCESS, 
           xlab = "Atlanto-occipital extension", 
           ylab = "SUCCESS")

mosaicplot(lei$NECKFLEX ~ lei$SUCCESS, 
           xlab = "Neck flexion", 
           ylab = "SUCCESS")

mosaicplot(lei$ASKASSIS ~ lei$SUCCESS, 
           xlab = "Ask assistance", 
           ylab = "SUCCESS")


##### Fit GEE
# Fit a GEE with exchangeable correlation structure
fit_1 <- gee(SUCCESS ~ EXTOA + PROPLGSP + PROPLIFT + 
               ASKASSIS + HELP + COMPS + NECKFLEX + 
               TRHAND + TRGEND + factor(TRIALCAT),
             id = TRAINEE, 
             corstr = "exchangeable",
             family = binomial,
             data = lei)
summary(fit_1)$coef[, c(1, 2, 5)]

# Compute Pearson residuals by hand

# Model is binary thus V_i = p_i * (1 - p_i)
sigma_hat_i <- sqrt(fit_1$fitted * (1 - fit_1$fitted)) 

# Pearson's residuals
res1 <- (lei$SUCCESS - fit_1$fitted) / sigma_hat_i

plot(res1)

# Check outlier
which(res1 > 15) # outlier is observation 278
lei[278, ]       # 3rd trial of trainee 13

# Let's see the failure/success sequence of this trainee
index <- lei$TRAINEE == 13

# Use color to indicate some covariate values
plot(lei$TRIAL[index], lei$SUCCESS[index], type = "b", 
     xlab = "Trial", ylab = "Success of procedure" ,
     main = paste("Trainee", trainee), 
     col = 1 + lei$ASKASSIS[index], pch = 19, ylim = c(0, 1.3))
legend("topright", 
       col = c(1, 2), pch = 19,
       legend = c("didn't ask assistance", "ask assistance"),
       bty = "n")
# You can check that for other covariate and see that the 3rd trials 
# "beat the odds" since it has unfavorable covariates but still succeeded

# To select significant variables :
# Compute p-values associated with each coefficient using the fact that the 
# asymptotic distribution of the robust z is N(0, 1) under H_0: "coef = 0"
p_values <- 1 - pnorm(abs(summary(fit_1)$coef[, "Robust z"]))

# put stars for cosmetics
stars <- vector("character", length = 16)
stars[p_values < 0.1] <- "*"
stars[p_values < 0.05] <- "**"
stars[p_values < 0.01] <- "***"

# What a nice representation!
data.frame(summary(fit_1)$coef[, c(1, 5)], p_values, stars)

# Fit whith some selected significant covariates
fit_2 <-gee(SUCCESS ~ PROPLGSP + PROPLIFT + ASKASSIS  + HELP + 
              COMPS + factor(TRIALCAT),
            id = TRAINEE,
            data = lei, 
            family = binomial, 
            corstr = "exchangeable")
summary(fit_2)$coef

# Check the "working correlation matrix" estimated by the model
summary(fit_2)$working
# The non diagonal terms are close to 0 -> Maybe independent structure is enough


fit_3 <-gee(SUCCESS ~ PROPLGSP + PROPLIFT + ASKASSIS  + HELP + COMPS + factor(TRIALCAT),
            id = TRAINEE,
            data = lei, 
            family = binomial, 
            corstr = "independence")
# Estimates are indeed very close to GEE with exchangeable structure very close 
summary(fit_2)$coef
summary(fit_3)$coef

# Fitting GEE with independent structure is the same as fitting a GLM
fit_glm <-glm(SUCCESS ~ PROPLGSP + PROPLIFT + ASKASSIS  + HELP + COMPS + factor(TRIALCAT),
              data = lei, 
              family = binomial)

# Let's check it:
summary(fit_glm)$coef
summary(fit_3)$coef


# ==== Exercise 2 ====
# Load data
ovary <- read.table('Ovary.dat', header = T)
head(ovary)

# Plot number of follicule vs time for each mare
for (m in unique(ovary$Mare)) {
  plot(ovary$Time[ovary$Mare == m], 
       ovary$follicles[ovary$Mare == m], type = "b", main = m)
}

boxplot(ovary$follicles ~ ovary$Mare, xlab = "Mare", ylab = "Ovary")


# Fit a GEE
fit_1 <- gee(follicles ~ Time, 
             id = Mare, 
             corstr = "exchangeable",
             family = poisson,
             data = ovary) 
summary(fit_1)$coef
# Pearson residuals
plot(fit_1$fitted.values, fit_1$residuals / sqrt(fit_1$fitted.values))


# Maybe a fit with sin(Time) could be relevent since it's a cycle:
fit_2 <- gee(follicles ~ sin(Time), 
             id = Mare, 
             corstr = "exchangeable",
             family = poisson,
             data = ovary) 
summary(fit_2)
# sin(Time) is more significant than Time (look at the robust Z of the two models)
