

ulam(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha + b_l * x,
    alpha ~ dnorm(50, 10),
    b_loc ~ dlnorm(-5, 2),
    sigma ~ dexp(1)
    )
)


