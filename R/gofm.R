gofm <-
function(output) 
{
    LL0 <- output$loglik[1]
    LLb <- output$loglik[2]
    K <- length(output$coefficients)
    rho2 <- 1 - (LLb / LL0)
    rho2a <- 1 - ((LLb - K) / LL0)

    out <- list(RHO2 = rho2,
                AdjRHO2 = rho2a,
                K = K,
                LL0 = LL0,
                LLb = LLb)
    class(out) <- "gofm"

    return(out)
}

