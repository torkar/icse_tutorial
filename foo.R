# This R file differs from the Rmd and html files.
# This is a improvement over the Rmd etc. used in a research course
# autumn 2022.

library(rethinking) # math-like model specification language
library(foreign) # for loading funky data files install.packages("foreign")

# load data file from PROMISE (Sayyad & Menzies), contributed by Shepperd
d <- read.arff("data/desharnais.arff")

# remove columns we don't need
d <- d[-c(1:5, 7:11)]

# convert Language (factor) to numeric because we hate factors
d$Language <- as.numeric(d$Language)

# check out the data frame
str(d)

##### Step 1 Likelihood?
# We have a count (i.e., Poisson), but what about assumptions?
var(d$Effort)
mean(d$Effort)
# so clearly we break assumptions and need to fall back on negative-binomial
# i.e., for Poisson(lambda), lambda measures both mean and variance.

###############################################################################
# Step 2 Simplified prior analysis
# a suitable prior for alpha, i.e., grand mean
# note we're using LogNormal since we'll use a link function
###############################################################################
# num. of max hours in a project / 1500 ~ num. FTEs/yr
max(rlnorm(1e6, 0, 2)) / 1500

# Let's plot the density distribution also
curve(dlnorm(x, meanlog = 0, sdlog = 2), from = 0, to = 3e4)

###############################################################################
# Step 3 Design models
#
# m_cp = complete pooling
# m_np = no pooling
# m_pp = partial pooling
#
##############################################################################
m_cp <- ulam(
  alist(
    Effort ~ dgampois(lambda, phi),
    log(lambda) <- alpha,
    alpha ~ normal(0, 2),
    phi ~ exponential(1)
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE, log_lik = TRUE, iter = 5e3
)

m_np <- ulam(
  alist(
    Effort ~ dgampois(lambda, phi),
    log(lambda) <- a + a_lang[Language],
    a ~ normal(0, 2),
    a_lang[Language] ~ normal(0, 1),
    phi ~ exponential(1)
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE, log_lik = TRUE, iter = 5e3
)

m_pp <- ulam(
  alist(
    Effort ~ dgampois(lambda, phi),
    log(lambda) <- a + a_lang[Language],
    a ~ normal(0, 3),
    a_lang[Language] ~ normal(mu_l, sigma_l),
    mu_l ~ normal(0, 1),
    sigma_l ~ exponential(1),
    phi ~ exponential(1)
  ), data = d, cores = 4, chains = 4, cmdstan = TRUE, log_lik = TRUE,
      iter = 5e3, control = list(adapt_delta = 0.95)
)

###############################################################################
# Step 4 Model comparison
#
##############################################################################
(ll <- compare(m_cp, m_np, m_pp))
plot(compare(m_cp, m_np, m_pp))
# In short, no model is really significantly better

###############################################################################
# Step 5 Inference
#
##############################################################################
# We're interested to look at the effect Language has on effort.
# Let's look at the two models where we have intercepts for each language
precis(m_np, depth = 2, pars = c("a_lang"))
precis(m_pp, depth = 2, pars = c("a_lang"))

# Plot and compare
par(mfrow = c(1, 2))
plot(precis(m_np, depth = 2, pars = c("a_lang")), main = "No pooling")
plot(precis(m_pp, depth = 2, pars = c("a_lang")), main = "Partial pooling")
par(mfrow = c(1, 1))

# Let's compare the estimates for Language 3 in both models and see how they
# differ on the outcome scale
post_np <- extract.samples(m_np) # contains all samples
post_pp <- extract.samples(m_pp)

exp(mean(post_pp$a) + mean(post_pp$a_lang[, 3])) -
    exp(mean(post_np$a) + mean(post_np$a_lang[, 3]))
# so they only differ in ~151 hours on relative effect scale

# But how much do they differ on absolute effect scale (i.e., prediction)
sim_np <- sim(m_np)
sim_pp <- sim(m_pp)
mean(sim_np - sim_pp)
# So m_np predicts >5h higher on the outcome scale xD

# Which Language is the best? We see that no language is 'significant',
# but if we have a gun pointing at us and we want to pick one anyways?
# What we need to remember is that lower values are better, i.e.,
# less number of hours spent in the project.

# Let's use m_np to since it "won" the relative comparison above.

# Compare all combinations of languages, but let's first look at
# diff between language 1 and 2

# create empty plot window
plot(NULL, xlim = c(-2, 4), ylim = c(0, 1), ylab = "", xlab = "")

# Add the two densities and text
dens(post_np$a_lang[, 1], add = TRUE, col = "blue")
text(2, 0.5, "Language 1", col = "blue")
abline(v = mean(post_np$a_lang[, 1]), col = "blue")

dens(post_np$a_lang[, 2], add = TRUE, col = "red")
text(0, 0.5, "Language 2", , col = "red")
abline(v = mean(post_np$a_lang[, 2]), , col = "red")
# So visually we see that Language 2 is "better", since it seems the mean
# is lower (the vertical lines). Can we quantify that probabilistically?
# Let's use the power of arithmetics :) Take the 1e5 samples and simply
# take L1 - L2

comp_12 <- post_np$a_lang[, 1] - post_np$a_lang[, 2]
dens(comp_12) # plot the difference first

# so slightly positive, i.e., Language 1 has slightly higher values,
# which implies that Language 2 is slightly better then!

# Let's look at what that means!
table(sign(comp_12))
# this is what I get (it can differ somewhat b/c we use stochastic algorithms):
#  -1    1
#2547 7453

# 25.47% of the time, Language *1* gets lower values than Language *2*.
# 74.53% of the time, Language *2* gets lower values than Language *1*.
# If forced to choose, we should pick Language 2, no matter if it's significant
# or not

# As an exercise for you, you can now compare L1 w/ L3 and L2 w/ L3 also!
# You will see that it's a clear cut case that L3 should be picked, even though
# it's not 'significant'!
#
# In the data set we also have many more independent variables, which we could
# use. We remove them at the beginning but we don't have to...
# https://www.kaggle.com/datasets/toniesteves/desharnais-dataset