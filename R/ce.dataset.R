ce.dataset <- function(
  data,
  response,
  design,
  categorical.attributes = NULL,
  continuous.attributes = NULL,
  common = NULL,
  optout = TRUE,
  unlabeled = TRUE,
  binary = FALSE,
  detail = FALSE)
{
  dm  <- make.design.matrix(
           choice.experiment.design = design,
           optout = optout,
           categorical.attributes = categorical.attributes,
           continuous.attributes = continuous.attributes,
           unlabeled = unlabeled,
           common = common,
           binary = binary)

  rtn <- make.dataset(
           respondent.dataset = data,
           design.matrix = dm,
           choice.indicators = response,
           detail = detail)

  nquestions <- design$design.information$nquestions
  nblocks    <- design$design.information$nblocks
  nalternatives <- nrow(dm) / (nquestions * nblocks)
  nrespondents  <- nrow(data)
  independent.names  <- colnames(dm)[-c(1, 2, 3)]
  noncovariate.names <- c("ID", "BLOCK", "QES", "ALT", "RES", "STR",
                          independent.names)
  covariate.names    <- colnames(rtn)[!colnames(rtn) %in% noncovariate.names]
  
  attributes(rtn)$data <- data
  attributes(rtn)$response <- response
  attributes(rtn)$nalternatives <- nalternatives
  attributes(rtn)$nrespondents <- nrespondents
  attributes(rtn)$nquestions <- nquestions
  attributes(rtn)$nchoicesets <- nblocks * nquestions
  attributes(rtn)$design <- design
  attributes(rtn)$design.matrix <- dm
  attributes(rtn)$categorical.attributes <- categorical.attributes
  attributes(rtn)$continuous.attributes <- continuous.attributes
  attributes(rtn)$independents <- independent.names
  attributes(rtn)$covariates <- covariate.names
  attributes(rtn)$noncovariates <- noncovariate.names
  attributes(rtn)$common <- common
  attributes(rtn)$optout <- optout
  attributes(rtn)$unlabeled <- unlabeled
  attributes(rtn)$binary <- binary
  attributes(rtn)$detail <- detail

  class(rtn) <- c("ce.dataset", "data.frame")

  rtn
}

