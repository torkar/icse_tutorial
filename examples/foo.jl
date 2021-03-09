

@model function linear_regression(x, y)
    # Set std dev prior.
    σ ~ Exponential(1)
    
    # Set intercept prior.
    α ~ Normal(50, 10)
    
    # Set the priors on our coefficient.
    βₗ ~ LogNormal(-5, 2)

    # Calculate mu.
    N = length(y)
    for n ∈ 1:N
        μ[n] ~ α + βₗ * x
    end
    
    y ~ Normal(μ, σ)
end


