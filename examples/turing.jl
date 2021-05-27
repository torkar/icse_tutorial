

@model function linear_regression(x, y)
    σ ~ Exponential(1)
    α ~ Normal(50, 10)
    βₗ ~ LogNormal(-5, 2)

    N = length(y)
    for n ∈ 1:N
        μ[n] ~ α + βₗ * x
    end
    
    y ~ Normal(μ, σ)
end


