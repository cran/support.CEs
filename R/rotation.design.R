rotation.design <-
function(candidate.array = NULL,
         attribute.names,
         nalternatives,
         nblocks,
         row.renames = TRUE,
         randomize = FALSE,
         seed = NULL) 
{
# Initial setting
    attribute.levels <- sapply(attribute.names, length)
    nattributes <- length(attribute.levels)

# Search an array corresponding to the argument
    if (is.null(candidate.array) == TRUE) {
        OA <- oa.design(nlevels = attribute.levels, seed = seed)
    }
    else {
        OA <- candidate.array
    }

    nquestions <- nrow(OA)

# Check number of blocks against number of rows of the design
    if ((nquestions%%nblocks) != 0) {
        cat("\n")
        cat("Your nblocks =", nblocks, "\n")
        cat("Number of rows of the design =", nrow(OA), "\n")
        stop("Number of blocks should be divisors of number of rows of the design.", call. = FALSE)
    }

# Rotation design
    ALTS <- vector("list", nalternatives)
    ALTS[[1]] <- matrix(as.numeric(as.matrix(OA)), nrow = nquestions, ncol = nattributes)
    for (i in 2:nalternatives) {
        ALTS[[i]] <- matrix(0, nrow = nquestions, ncol = ncol(OA))
    }
    for (i in 2:nalternatives) {
        ALTS[[i]] <- sweep(ALTS[[(i - 1)]], 2, apply(ALTS[[(i - 1)]], 2, max), "%%") + 1
    }

# Rename row
    if (row.renames != TRUE) { 
        OriginalRowNames <- row.names(OA)
        for (i in 1:nalternatives) {
            rownames(ALTS[[i]]) <- OriginalRowNames
        }
    }

# Mix-and-Match design
    if (is.null(seed) == FALSE) {
        set.seed(seed)
    }

    if (randomize == TRUE) {
        repeat {
            for (i in 1:nalternatives) {
                ALTS[[i]] <- cbind(ALTS[[i]], sample(1:nquestions, nquestions))
                ALTS[[i]] <- ALTS[[i]][order(ALTS[[i]][, (nattributes + 1)]), ]
                ALTS[[i]] <- ALTS[[i]][, 1:nattributes]
            }
            chk <- FALSE
            for (i in 1:(nalternatives - 1)) {
                for (j in (i + 1):nalternatives) {
                    for (k in 1:nquestions) {
                        chk <- chk + all(ALTS[[i]][k, ] == ALTS[[j]][k, ])
                    }
                }
            }
            if (sum(chk) == 0) {
                break
            }
        }
    }

# Randomize order of questions
    nquestions_nblocks <- nquestions / nblocks
    RndUnif <- runif(nquestions)
    for (i in 1:nalternatives) {
        ALTS[[i]] <- cbind(ALTS[[i]], RndUnif)
        ALTS[[i]] <- ALTS[[i]][order(ALTS[[i]][, (nattributes + 1)]), ]
        ALTS[[i]] <- ALTS[[i]][, 1:nattributes]
    }

# Add BLOCK, QES, and ALT variables to ALTS[[i]]
    for (i in 1:nalternatives) {
        ALTS[[i]] <- cbind(rep(1:nblocks, each = nquestions_nblocks),
                           rep(1:nquestions_nblocks, nblocks),
                           rep(i, nquestions),
                           ALTS[[i]])
    }

# Convert ALTS[[i]] into data.frame
    for (i in 1:nalternatives) {
        ALTS[[i]] <- data.frame(ALTS[[i]])
    }

# Treat attribute columns as factors
    for (i in 1:nalternatives) {
        for (j in 1:nattributes) {
            ALTS[[i]][, (j + 3)] <- factor(ALTS[[i]][, (j + 3)])
        }
    }

# Assign names to attributes/levels
    for (i in 1:nalternatives) {
        colnames(ALTS[[i]]) <- c("BLOCK", "QES", "ALT", names(attribute.names))
        for (j in 1:nattributes){
            levels(ALTS[[i]][, (j + 3)]) <- attribute.names[[j]]
        }
    }

# Format output
    names(ALTS) <- paste("alt.", 1:nalternatives, sep = "")

    return(c(ALTS, list(candidate = OA)))
}

