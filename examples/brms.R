

brm(
    y ~ 1 + LOC,
    data = d,
    family = normal(),
    prior = c(
        set_prior(normal(50, 10), class = Intercept),
        set_prior(normal(-5, 2), class = b,
        set_prior(exponential(1), class = sd))
    )
)


