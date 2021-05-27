

ulam(
  alist(
    y ~ dnorm(mu, sigma),
    mu <- alpha + b_loc * x,
    alpha ~ dnorm(50, 10),
    b_loc ~ dlnorm(-5, 2),
    sigma ~ dexp(1)
    )
)


