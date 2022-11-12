library(rethinking) # math-like model specification language
library(foreign) # for loading funky data files install.packages("foreign")

# load data file from PROMISE (Sayyad & Menzies), contributed by Shepperd
d <- as.data.frame(read.arff("data/desharnais.arff"))

# remove columns we don't need
d <- d[-c(1:5, 7:11)]

# convert Language (factor) to numeric because we hate factors
d$Language <- as.numeric(d$Language)
# make sure Effort is integer
d$Effort <- as.integer(d$Effort)

# check out the data frame
str(d)

##### Step 1 Likelihood
# ontological and epistemological assumptions?
var(d$Effort)
mean(d$Effort)

##### Step 2 Simplified prior analysis
# a suitable prior for alpha, i.e., grand mean
# note we're using LogNormal since we'll use a link function
max(rlnorm(1e6, 0, 2))
curve(dlnorm(x, meanlog = 0, sdlog = 2), from = 0, to = 1e4)

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
      iter = 5e3, control = list(adapt_delta = 0.9)
)

(ll <- compare(m_cp, m_np, m_pp))
