#' @name summary
#' @title Diagnostics of \code{rfPermute} or \code{randomForest} models.
#' @description Combine plots of error traces and inbag rates.
#'
#' @param object a \code{rfPermute} or \code{randomForest} model object to 
#'   summarize.
#' @param ... arguments passed to \code{\link{plotInbag}}.
#'   
#' @return A combination of plots from \code{\link{plotTrace}} and 
#'   \code{\link{plotInbag}} as well as summary confusion matrices 
#'   (classification) or error rates (regression) from the model.
#'  
#' @seealso \code{\link{plotTrace}}, \code{\link{plotInbag}}  
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' # A regression model using the ozone example
#' data(airquality)
#' ozone.rp <- rfPermute(
#'   Ozone ~ ., data = airquality, na.action = na.omit,
#'   ntree = 100, nrep = 50, num.cores = 1
#' )
#'
#' summary(ozone.rp)
#'  
#' @export
#' 
summary.randomForest <- function(object, ...) {
  print(object, ...)
  print(plotTrace(object, plot = FALSE))
}

#' @rdname summary
#' @export
#' 
summary.rfPermute <- function(object, ...) {
  summary.randomForest(object)
}