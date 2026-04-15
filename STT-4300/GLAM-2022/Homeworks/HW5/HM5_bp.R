source("truncpoisson.R")
#install.packages("pscl")
library("pscl")
library(statmod)

doc <- read.table("docvisits.asc", header = T)
head(doc)

doc$w <- doc$dvisits > 0


# Fit binary part of the hurdle model
hurdle_bin <- glm(w ~ sex + age + agesq + income + levyplus + 
                 freepoor + freerepa + illness + actdays + hscore + 
                 chcond1 + chcond2, 
                 data = doc, family = binomial)
summary(hurdle_bin)


# Fit truncated Poisson part of the hurdle model
hurdle_truncp <- glm(dvisits ~ sex + age + agesq + income + levyplus + 
                        freepoor + freerepa + illness + actdays + hscore + 
                        chcond1 + chcond2, 
                     data = doc, 
                     family = truncpoisson, 
                     subset = (dvisits > 0))
summary(hurdle_truncp)


# Fit the whole hurdle model at once
hurdle <- hurdle(dvisits ~ sex + age + agesq + income + levyplus + 
                       freepoor + freerepa + illness + actdays + hscore + 
                       chcond1 + chcond2,
                     data = doc,
                     dist = "poisson", zero.dist = "binomial", link = "logit")
summary(hurdle)

# Compare AIC
AIC(hurdle)
AIC(hurdle_bin) + AIC(hurdle_truncp)

# Fit a simple Poisson model
poisson_glm <- glm(dvisits ~ sex + age + agesq + income + levyplus + 
                       freepoor + freerepa + illness + actdays + hscore + 
                       chcond1 + chcond2, 
                   data = doc, 
                   family = poisson)
AIC(poisson_glm)

# Fit a ZIP model
zip_model <- zeroinfl(dvisits ~ sex + age + agesq + income + levyplus + 
                      freepoor + freerepa + illness + actdays + hscore + 
                      chcond1 + chcond2,
                      dist = "poisson",
                      link = "logit",
                      data = doc)
AIC(zip_model)