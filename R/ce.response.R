ce.response <- function(
  design, 
  categorical.attributes = NULL, 
  continuous.attributes = NULL, 
  optout = FALSE, 
  b, 
  n, 
  detail = FALSE, 
  seed = NULL)
{
# Initial setting
  nR   <- n
  nALT <- design$design.information$nalternatives
  nB   <- design$design.information$nblocks
  nQ   <- design$design.information$nquestions

  if(optout == TRUE) {
    nALT <- nALT + 1
  }

  if(nR < nB) {
    stop("number of respondents must be larger than number of blocks")
  }
  if((nR%%nB) != 0) {
    stop("number of respondents must be divisible by than number of blocks")
  }

# Prepare a design matrix
  D <- make.design.matrix(
         choice.experiment.design = design,
         optout = optout,
         categorical.attributes = categorical.attributes,
         continuous.attributes = continuous.attributes,
         unlabeled = TRUE,
         common = NULL,
         binary = FALSE)

  fullD <- rep(x = 1, times = nR/nB) %x% data.matrix(D)
  colnames(fullD) <- colnames(D)
  rownames(fullD) <- NULL

# Synthesize responses to CE questions
  X <- data.frame(fullD)
  Xb <- sweep(x = X[, 4:ncol(X)], MARGIN = 2, STATS = b, FUN = "*")
  V <- rowSums(Xb)
  if(!is.null(seed)) {
    set.seed(seed)
  }
  e <- -log(-log(runif(n = length(V))))
  U <- V + e
  Umat <- matrix(data = U, ncol = nALT, byrow = TRUE)
  columns.max <- max.col(Umat)
  RC <- cbind(R = 1:nrow(Umat), C = columns.max)
  RESmat <- matrix(data = 0L, nrow = nrow(Umat), ncol = ncol(Umat))
  RESmat[RC] <- 1L
  RES <- as.vector(t(RESmat))

# Prepare and return the dataset
  ID <- rep(x = 1:nR, each = nrow(D)/nB)
  STR <- 100 * ID + X$QES
  dataset <- data.frame(ID, X, STR, RES)

  if(detail == TRUE) {
    return(dataset)
  }

  simple.dataset <- dataset[dataset$RES == 1, c("ID", "BLOCK", "QES", "ALT")]
  colnames(simple.dataset)[4] <- "q"
  rtn <- reshape(simple.dataset, v.names = "q", idvar = "ID", 
                 timevar = "QES", sep = "", direction = "wide")
  rownames(rtn) <- NULL

  return(data.frame(rtn))
}
