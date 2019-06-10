setwd("~/Box Sync/Jobs/Data Science UVA")
library(tidyverse)

anes <- read_csv("anes_cleaned.csv")

######## ANALYSIS OF VOTING IN THE 2016 ELECTION ##################
logit <- glm(vote ~ age + marital + education + union + 
                   race + gender,
             data = anes, 
             family=binomial(link="logit"))
summary(logit)

####### LOGIT FROM SCRATCH #######################################

logit.ll <- function(parameters, formula, data, outcome){
      Xmat <- model.matrix(terms(formula), data=data)
      ystar <- Xmat %*% parameters
      ll <- -ystar*(1 - outcome) - log(1 + exp(-ystar))
      return(sum(ll))
}

#Try an example (change the parameter values, try to make the ll as high as possible)
logit.ll(parameters = c(3, 1, -2, -.5, -.4, 1, .7,
                        .4, -1, .3, 2, .5, -.1),
         formula = as.formula(~ age + marital + education + 
                                    union + race + gender),
         data = anes, 
         outcome = anes$vote)

# Using a hill-climbing algorithm
our.logit <- optim(par = c(3,rep(0, 12)), 
                   fn = logit.ll,
                   formula = as.formula(~ age + marital + education + 
                                              union + race + gender),
                   data = anes,
                   outcome = anes$vote,
                   method="BFGS", 
                   hessian = TRUE,
                   control=list(fnscale=-1, maxit=5000))

#Comparing our logit to the pre-programmed version
our.coefs <- our.logit$par
canned.coefs <- coef(logit)
coef.compare <- data.frame(our.coefs, canned.coefs)
coef.compare

our.SEs <- sqrt(diag(solve(-our.logit$hessian)))
canned.SEs <- sqrt(diag(vcov(logit)))
SE.compare <- data.frame(our.SEs, canned.SEs)
SE.compare

####### INTERPRETATION OF RESULTS ###############################
exp(coef(logit)) ## odds ratios

library(margins)
m <- margins(logit, type="response") ## marginal change in probability (average marginal effects)
summary(m)

cplot(logit, "age", what="prediction") ## predicted probability of Clinton vote by age
cplot(logit, "race", what="prediction") ## predicted probability of Clinton vote by race
