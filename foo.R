# the code as-is from the TB at ICSE 2021

# Clone from GitHub:
# https://github.com/torkar/icse_tutorial

library(rethinking) # model specification
library(foreign) # for loading data files

# set working directory
setwd("~/Development/icse_tutorial/")

# load data file from PROMISE (Sayyad & Menzies), contributed by Shepperd
f <- read.arff("data/desharnais.arff")

# remove columns we don't need
f <- f[-c(1:5,7:11)]

# convert Language (factor) to numeric because we hate factors
f$Language <- as.numeric(f$Language)

# check out the data frame
str(f)

##### Step 1 Likelihood
# ontological and epistemological assumptions?
var(f$Effort)
mean(f$Effort)

##### Step 2 Prior predictive check
# Show for grand mean

max(rlnorm(1e5, 0, 4))

##### Step 3 Sampling and diagnostics
# grand mean model (m0) and varying intercept model (m1)

m0 <- ulam(
  alist(
    Effort ~ dgampois(lambda, phi),
    log(lambda) <- alpha,
    alpha ~ normal(0,4),
    phi ~ exponential(1)
  ), data =f , cores = 4, chains = 4, cmdstan = TRUE, log_lik=TRUE, iter = 5e3
)

m1 <- ulam(
  alist(
    Effort ~ dgampois(lambda, phi),
    log(lambda) <- a_lang[Language],
    a_lang[Language] ~ normal(0,3),
    phi ~ exponential(1)
  ), data =f , cores = 4, chains = 4, cmdstan = TRUE, log_lik=TRUE, iter = 5e3
)



##### Step 4 Posterior predictive checks
# do a simple postcheck() on m1

postcheck(m0)

##### Step 5 Model comparison
# use LOO

compare(m0,m1, func=LOO)

##### Step 6 Compute stuff
# a) plot posterior values with uncertainty using precis()
# b) extract samples using extract.samples()
# c) compute distribution of effect sizes using... arithmetic...

