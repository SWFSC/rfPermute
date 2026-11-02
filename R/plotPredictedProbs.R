#' @title Plot Predicted Probabilities
#' @description Plot histogram of assignment probabilities to predicted class. 
#'   This is used for determining if the model differentiates between correctly 
#'   and incorrectly classified samples in terms of how strongly they are 
#'   classified.
#' 
#' @param x a \code{rfPermute} or \code{randomForest} model object.
#' @param bins number of bins in histogram. Defaults to number of samples / 5.
#' @param plot display the plot?
#'   
#' @return the \code{ggplot2} object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#' 
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' plotPredictedProbs(rf, bins = 20)
#' 
#' @export
#'
plotPredictedProbs <- function(x, bins = 30, plot = TRUE) {
  rf <- as.randomForest(x)
  if(rf$type == "regression") stop("'rf' must be of a classification model")
  
  p <- rf$votes |> 
    as.data.frame() |> 
    cbind(
      class = as.character(rf$y),
      predicted = as.character(rf$predicted)
    ) |> 
    tidyr::gather("pred.class", "prob", -.data$class, -.data$predicted) |> 
    dplyr::filter(.data$predicted == .data$pred.class) |> 
    dplyr::mutate(correct = .data$class == .data$predicted) |> 
    ggplot2::ggplot(ggplot2::aes(.data$prob, fill = .data$class)) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::facet_wrap(~ predicted) +
    ggplot2::labs(x = "Assignment probability", y = "Frequency") +
    ggplot2::theme(legend.title = ggplot2::element_blank())
  
  if(plot) print(p)
  invisible(p)
}
