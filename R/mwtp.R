mwtp <-
function(output,
         monetary.variables,
         nonmonetary.variables = NULL,
         nreplications = 10000,
         percentile.points = c(0.5, 2.5, 5, 95, 97.5, 99.5),
         seed = NULL) 
{
# Initial setting
    if (is.null(seed) == FALSE) {
        set.seed(seed)
    }

# Replicate coefficients
    repb <- mvrnorm(nreplications, output$coefficients, output$var)

# Initialize variables
    if (is.null(nonmonetary.variables) == TRUE){
        repmwtps <- matrix(0, nrow = nreplications, 
                              ncol = (length(names(output$assign)) - 1))
        mwtps <- rep(0, (length(names(output$assign)) - 1))
    }
    else {
        repmwtps <- matrix(0, nrow = nreplications,
                              ncol = sum(sapply(nonmonetary.variables, length)))
        mwtps <- rep(0, sum(sapply(nonmonetary.variables, length)))
    }
    monetary.index <- NULL
    nonmonetary.variable.names <- NULL
    m <- 1
# Store indices corresponding to monetary variables
    for (i in  1:length(monetary.variables)) {
        j <- which(names(output$assign) == monetary.variables[i])
        monetary.index <- c(monetary.index, j)
    }
# Store names of nonmonetary variables
    if (is.null(nonmonetary.variables) == TRUE) {
        nonmonetary.variables <- list(names(output$assign)[-monetary.index])
    }

# Calculate mean MWTPs and simulate empirical distribution for each of MWTPs 
    if (is.list(nonmonetary.variables) == TRUE) {
        for (i in 1:length(nonmonetary.variables)) {
            for (j in 1:length(nonmonetary.variables[[i]])) {
                k <- which(names(output$assign) == nonmonetary.variables[[i]][j])
                mwtps[m] <- -output$coefficients[k] / output$coefficients[monetary.index[i]]
                repmwtps[, m] <- -repb[, k] / repb[, monetary.index[i]]
                m <- m + 1
            }
            nonmonetary.variable.names <- c(nonmonetary.variable.names,
                                            nonmonetary.variables[[i]])
        }
    }
    else {
        for (i in 1:length(nonmonetary.variables)) {
            j <- which(names(output$assign) == nonmonetary.variables[i])
            mwtps[i] <- -output$coefficients[j] / output$coefficients[monetary.index]
            repmwtps[, i] <- -repb[, j] / repb[, monetary.index]
        }
        nonmonetary.variable.names <- nonmonetary.variables
    }
    colnames(repmwtps) <- nonmonetary.variable.names

# Calculate confidence intervals
    confidence.intervals <- apply(repmwtps, 2,
                                  quantile, probs = (percentile.points / 100))

# Format output
    output <- list(mwtp.table = t(rbind(MWTP=mwtps, confidence.intervals)),
                   mwtps = repmwtps)
    class(output) <- "mwtp"
  
    return(output)
}

