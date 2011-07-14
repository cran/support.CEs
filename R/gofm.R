gofm <-
function(output) 
{
# Log likelihood value at the start
    LL0 <- output$loglik[1]

# Log likelihood value at convergence
    LLb <- output$loglik[2]

# Number of estimated coefficients
    K <- length(output$coefficients)

# Rho-squared
    rho2 <- 1 - (LLb / LL0)

# Rho-squared adjusted by K
    rho2a <- 1 - ((LLb - K) / LL0)

# Output
    return(list(RHO2 = rho2,
                AdjRHO2 = rho2a,
                K = K,
                LL0 = LL0,
                LLb = LLb))
}

