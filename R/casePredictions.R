#' @title Case Predictions
#' @description Construct a data frame of case predictions for 
#'   training data along with vote distributions.
#' 
#' @param x a \code{rfPermte} or \code{randomForest} model object.
#'   
#' @return A data frame containing columns of original and predicted cases,
#'   whether they were correctly classified, and vote distributions among cases.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#' 
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' 
#' cp <- casePredictions(rf)
#' cp
#'
#' @export
#'
casePredictions <- function(x) {    
  rf <- as.randomForest(x)
  if(rf$type != "classification") stop("'rf' must be a classification model")

  id <- names(rf$y)
  if(is.null(id)) id <- as.character(1:length(rf$y))
  df <- cbind(
    data.frame(
      id = id,
      original = rf$y,
      predicted = rf$predicted,
      is.correct = rf$y == rf$predicted
    ),
    rf$votes
  )
  rownames(df) <- NULL
  df
}