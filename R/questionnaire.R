questionnaire <-
function(choice.experiment.design)
{
# Assign design information
  nblocks <- choice.experiment.design$design.information$nblocks
  nquestions <- choice.experiment.design$design.information$nquestions
  nalternatives <- choice.experiment.design$design.information$nalternatives
  nattributes <- choice.experiment.design$design.information$nattributes

# Integrate alternatives into data.frame
  my.design <- as.matrix(choice.experiment.design[[1]][[1]])
  for (i in 2:nalternatives) {
    my.design <- rbind(my.design,
                       as.matrix(choice.experiment.design[[1]][[i]]))
  }
  rownames(my.design) <- NULL 
  my.design <- data.frame(my.design)
  my.design$BLOCK <- as.numeric(as.character(my.design$BLOCK))
  my.design$QES <- as.numeric(as.character(my.design$QES))
  my.design$ALT <- as.numeric(as.character(my.design$ALT))

# Convert the choice experiment design into questions
  my.design <- my.design[order(my.design$BLOCK, my.design$QES, my.design$ALT),]
  alternative.names <- paste("alt.", 1:nalternatives, sep= "")
  for (i in 1:nblocks) {
    cat("Block", i, "\n", "\n")
    for (j in 1:nquestions) {
      cat("Question", j, "\n")
      temp <- t(subset(my.design, my.design$BLOCK == i & my.design$QES == j)[, 4:(3 + nattributes)])
      colnames(temp) <- alternative.names
      print(temp)
      cat("\n")
    }
  }
}

