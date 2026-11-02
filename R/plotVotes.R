#' @title Plot Vote Distribution
#' @description For classification models, plot distribution of votes for each 
#'   sample in each class.
#' 
#' @param x a \code{rfPermute} or \code{randomForest} model object.
#' @param type either \code{area} for stacked continuous area plot or 
#'   \code{bar} for discrete stacked bar chart. The latter is prefered for small 
#'   numbers of cases. If not specified, a bar chart will be used if all 
#'   classes have <= 30 cases.
#' @param freq.sep.line put frequency of original group on second line in facet 
#'   label? If \code{FALSE}, labels are single line. If \code{NULL} frequencies 
#'   will not be included in labels.
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
#' plotVotes(rf)
#' 
#' @export
#'
plotVotes <- function(x, type = NULL, freq.sep.line = TRUE, plot = TRUE) {  
  rf <- as.randomForest(x)
  if(rf$type == "regression") stop("'rf' must be of a classification model")
  
  swfscMisc::plotAssignments(
    (rf$votes / rowSums(rf$votes)) * 100, 
    rf$y, 
    type = type, 
    ylab = "Percent of votes", 
    freq.sep.line = freq.sep.line,
    plot = plot
  )
}