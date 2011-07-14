make.dataset <-
function(respondent.dataset,
         design.matrix,
         choice.indicators,
         detail = FALSE) 
{
# Initial setting
    nquestions <- length(choice.indicators)
    nalternatives <- length(table(design.matrix$ALT))
    nrespondents <- length(respondent.dataset$ID)

# Convert respondent.dataset into "one-row-one-alternative" style data set
    my.respondent.dataset <- rbind(respondent.dataset, respondent.dataset)
    if (nquestions * nalternatives > 2) {
        for (i in 1:(nquestions * nalternatives - 2)) {
            my.respondent.dataset <- rbind(my.respondent.dataset, respondent.dataset)
        }
    }
    my.respondent.dataset <- my.respondent.dataset[order(my.respondent.dataset$ID), ]

# Add QES and ALT variables to respondent data set 
    my.respondent.dataset$QES <- rep(1:nquestions, each = nalternatives, times = nrespondents)
    my.respondent.dataset$ALT <- rep(1:nalternatives, times = nrespondents * nquestions)

# Convert choice.indicators into SELECT variable
    colmuns.choice.indicators <- rep(0, nquestions)
    for (i in 1:nquestions) {
        colmuns.choice.indicators[i] <- which(colnames(my.respondent.dataset) ==
                                              choice.indicators[i])
    }
    m <- c(1)
    for (i in 1:nrespondents) {
        for (j in colmuns.choice.indicators) {
            for (k in 1:nalternatives) {
                my.respondent.dataset$SELECT[m]<-my.respondent.dataset[m,j]
                m <- m + c(1)
            }
        }
    }

# Convert SELECT variable into RES variable
    my.respondent.dataset$RES <- my.respondent.dataset$SELECT == my.respondent.dataset$ALT

# Combine respondent data set and design matrix
    my.respondent.dataset$mt <- my.respondent.dataset$ALT +
                                my.respondent.dataset$QES * 100 +
                                my.respondent.dataset$BLOCK * 10000
    design.matrix$mt <- design.matrix$ALT +
                        design.matrix$QES * 100 +
                        design.matrix$BLOCK * 10000
    design.matrix <- design.matrix[4:ncol(design.matrix)]
    tempds <- merge(my.respondent.dataset, design.matrix, by = "mt")

# Add STR variable
    tempds$STR <- tempds$QES + tempds$ID * 100

# Format output
    dataset <- subset(tempds, select = -mt)
    if (detail == FALSE) {
        dataset <- subset(dataset, select = -colmuns.choice.indicators)
        dataset <- subset(dataset, select = -SELECT)
    }
    dataset <- dataset[order(dataset$STR), ]
    row.names(dataset) <- rep(1:(nquestions * nalternatives * nrespondents))

    return(dataset)
}

