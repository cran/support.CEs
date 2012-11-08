Lma.design <-
function(candidate.array = NULL,
         attribute.names,
         nalternatives,
         nblocks,
         row.renames = TRUE,
         seed = NULL) 
{
# Initial setting 1/2
    attribute.levels <- rep(sapply(attribute.names, length), nalternatives)
    if (nblocks >= 2) {
        attribute.levels <- c(attribute.levels, nblocks)
    }

# Search an array corresponding to the argument
    if (is.null(candidate.array) == TRUE) {
        OA <- oa.design(nlevels = attribute.levels, seed = seed)
    }
    else {
        OA <- candidate.array
    }

# Initial setting 2/2
    nattributes <- length(attribute.names)
    total.nattributes <- nattributes * nalternatives
    nquestions <- nrow(OA)
    nquestions_nblocks <- nquestions / nblocks
    alt <- vector("list", nalternatives)

    if (is.null(candidate.array) == TRUE) {
        if (nblocks == 1) {
            backupOA <- OA
            OA <- cbind(OA, DUMMY=rep(1, nquestions))
        }
    }

# Randomize order of questions
    if (is.null(seed) == FALSE) {
        set.seed(seed)
    }
    ALTS <- transform(OA, r = runif(nquestions))
    ALTS <- ALTS[order(ALTS[,(total.nattributes + 1)], ALTS$r), ]

# Store alternatives
    if (nattributes == 1) {
        for (i in 1:nalternatives){
            temp <- cbind(BLOCK = as.numeric(ALTS[, (total.nattributes + 1)]),
                          QES = rep(1:nquestions_nblocks, nblocks),
                          ALT = rep(i, nquestions),
                          ALTS[, i:(i + 1)])[, 1:4]
            colnames(temp)[4] <- names(attribute.names)
            levels(temp[, 4]) <- attribute.names[[1]]          
            alt[[i]] <- temp
        }
    }
    else {
        for (i in 1:nalternatives) {
            temp <- ALTS[, (1 + (i - 1) * nattributes):(i * nattributes)]
            colnames(temp) <- names(attribute.names)
            for (j in 1:nattributes){
                levels(temp[, j]) <- attribute.names[[j]]
            }
            alt[[i]] <- cbind(BLOCK = as.numeric(ALTS[, (total.nattributes + 1)]),
                              QES = rep(1:nquestions_nblocks, nblocks),
                              ALT = rep(i, nquestions),
                              temp)
        }
    }

# Format output
    if (row.renames == TRUE) {
        for (i in 1:nalternatives) {
            rownames(alt[[i]]) <- c(1:nquestions)
        }
    }
    if (is.null(candidate.array) == TRUE) {
        if (nblocks == 1){
            OA <- backupOA
        }
    }
    ALTS <- list(NULL)
    for (i in 1:nalternatives) {
        ALTS[i] <- list(data.frame(alt[[i]]))
    }
    names(ALTS) <- paste("alt.", 1:nalternatives, sep = "")
    desinf <- list(nalternatives = nalternatives,
                   nblocks = nblocks,
                   nquestions = nquestions_nblocks,
                   nattributes = nattributes
                   )
    my.choice.experiment.design <- list(alternatives = ALTS,
                                        candidate = OA,
                                        design.information = desinf)
    class(my.choice.experiment.design) <- "cedes"

    return(my.choice.experiment.design)
}

