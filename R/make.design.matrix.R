make.design.matrix <-
function(choice.experiment.design,
         optout = TRUE,
         categorical.attributes = NULL,
         continuous.attributes = NULL,
         unlabeled = TRUE) 
{
# Assign design information
    nblocks <- choice.experiment.design$design.information$nblocks
    nquestions <- choice.experiment.design$design.information$nquestions
    nalternatives <- choice.experiment.design$design.information$nalternatives
    nattributes <- choice.experiment.design$design.information$nattributes

# Initial setting
    variable.names <- NULL
    conv.choice.experiment.design <- vector("list", nalternatives)

# Create attribute variables
    for (i in 1:nalternatives) {
    # Categorical attribute variables
        if (is.null(categorical.attributes) == FALSE) {
            for (j in 1:length(categorical.attributes)) {
                k <- which(names(choice.experiment.design[[1]][[i]]) == categorical.attributes[j])
                m <- nlevels(choice.experiment.design[[1]][[i]][, k])
                variable.names <- c(variable.names, 
                                    levels(choice.experiment.design[[1]][[i]][, k])[2:m])
                conv.choice.experiment.design[[i]] <- cbind(conv.choice.experiment.design[[i]],
                                 model.matrix(~ choice.experiment.design[[1]][[i]][, k] - 1)[, 2:m])
            }
        }
    # Continuous attribute variables
        if (is.null(continuous.attributes) == FALSE) {
            for (j in 1:length(continuous.attributes)) {
                k <- which(names(choice.experiment.design[[1]][[i]]) == continuous.attributes[j]) 
                variable.names <- c(variable.names, names(choice.experiment.design[[1]][[i]])[k])
                conv.choice.experiment.design[[i]] <- cbind(conv.choice.experiment.design[[i]],
                                  as.numeric(as.character(choice.experiment.design[[1]][[i]][, k])))
            }
        }
    }

# Create design matrix
    nvariable <- length(variable.names) / nalternatives
    # Unlabeled
    if (unlabeled == TRUE) {
        my.design <- conv.choice.experiment.design[[1]]
        for (i in 2:nalternatives) {
            my.design <- rbind(my.design, conv.choice.experiment.design[[i]])
        }
        colnames(my.design) <- variable.names[1: nvariable]
    }
    # Labled
    else {
        my.design <- diag.block(conv.choice.experiment.design)
        variable.names <- paste(variable.names, rep(1:nalternatives, each=nvariable), sep = "")
        colnames(my.design) <- variable.names
    }

# Create BLOCK, QES, and ALT variables
    BQS <- choice.experiment.design[[1]][[1]][, 1:3]
    for (i in 2:nalternatives) {
        BQS <- rbind(BQS, choice.experiment.design[[1]][[i]][, 1:3])
    }

# ASC for unlabeled design
    if (unlabeled == TRUE) {
    # With opt-out option
        if (optout == TRUE) {
            ASC <- rep(1, nalternatives * nquestions * nblocks)
        }
    # Without opt-out option
        else {
            ASC <- c(rep(1, (nalternatives - 1) * nquestions * nblocks),
                     rep(0, nquestions * nblocks))
        }
    }
# ASC for labeled design
    else {
    # With opt-out option
        if (optout == TRUE) {
            ASC <- kronecker(diag(1, nalternatives), rep(1, nquestions * nblocks))
            if (nalternatives >= 2) {
                colnames(ASC) <- paste("ASC", 1:nalternatives, sep = "")
            }
        }
    # Without opt-out option
        else {
            ASC <- rbind(kronecker(diag(1, (nalternatives - 1)),
                                   rep(1, nquestions * nblocks)), 
                         matrix(0, nrow = nquestions * nblocks, ncol = (nalternatives - 1)))
            if (nalternatives >= 3) {
                colnames(ASC) <- paste("ASC", 1:(nalternatives - 1), sep = "")
            }
        }
    }

# Add BLOCK, QES, ALT, and ASC variables to design matrix
    my.design <- data.frame(BQS, ASC, my.design) 

# Add rows corresponding to opt-out options to design matrix
    if (optout == TRUE) {
        optout.set <- as.data.frame(matrix(c(rep(c(1:nblocks), each = nquestions),
                                             rep(c(1:nquestions), nblocks),
                                             rep((nalternatives + 1), nquestions * nblocks),
                                             rep(0, nquestions * nblocks * (ncol(my.design) - 3))),
                                           nrow = (nquestions * nblocks),
                                           ncol = ncol(my.design)))
        names(optout.set) <- names(my.design)
        my.design <- rbind(my.design, optout.set)
    }

# Format output
    my.design <- my.design[order(my.design$BLOCK, my.design$QES, my.design$ALT), ]
    row.names(my.design) <- 1:nrow(my.design)

    return(my.design)
}

