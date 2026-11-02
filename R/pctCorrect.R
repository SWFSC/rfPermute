#' @title Percent Correctly Classified
#' @description For classification models, calculate the percent of 
#'   individuals correctly classified in a specified percent of trees in 
#'   the forest.
#' 
#' @param x a \code{rfPermte} or \code{randomForest} model object.
#' @param pct vector of minimum percent of trees voting for each class. Can be 
#'   \code{0:1} or \code{0:100}.
#' 
#' @return a matrix giving the percent of individuals correctly classified in 
#'   each class and overall for each threshold value specified in \code{pct}.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov} 
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#'
#' rf <- randomForest(factor(am) ~ ., mtcars, importance = TRUE)
#' pctCorrect(rf)
#' 
#' @export
#' 
pctCorrect <- function(x, pct = c(seq(0.8, 0.95, 0.05), 0.99)) {
  rf <- as.randomForest(x)
  if(rf$type == "regression") stop("'rf' must be of a classification model")
  
  pct.good <- FALSE
  if(is.numeric(pct)) {
    zero.one <- all(dplyr::between(pct, 0, 1))
    if(zero.one) pct.good <- TRUE
    zero.hundred <- all(dplyr::between(pct, 0, 100))
    if(zero.hundred & !zero.one) {
      pct <- pct / 100
      pct.good <- TRUE
    }
  }
  if(!pct.good) {
    stop("'pct' must be a numeric vector with values in the range of 0:1 or 0:100")
  }
  
  mat <- do.call(cbind, lapply(pct, function(p) {
    is.correct <- sapply(names(rf$y), function(id) {
      rf$votes[id, rf$y[id]] >= p & rf$y[id] == rf$predicted[id]
    })
    by.class <- tapply(is.correct, rf$y, mean)
    c(by.class, Overall = mean(is.correct))
  }))
  colnames(mat) <- paste("pct.correct_", pct, sep = "")
  mat <- cbind(data.frame(class = rownames(mat)), mat * 100)
  rownames(mat) <- NULL
  mat
}