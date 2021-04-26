
# Clone from GitHub:
# https://github.com/torkar/icse_tutorial

library(rethinking) # model specification
library(foreign) # for loading data files

# set working directory
setwd("~/Development/icse_tutorial/")

# load data file
f <- read.arff("data/desharnais.arff")

# remove columns we don't need
f <- f[-c(1:5,7:11)]

# convert Language (factor) to numeric because we hate factors
f$Language <- as.numeric(f$Language)

str(f)

# Step 1 Likelihood


# Step 2 Prior predictive check


# Step 3 Sampling and diagnostics


# Step 4 Posterior predictive checks


# Step 5 Model comparison


# Step 6 Compute stuff


