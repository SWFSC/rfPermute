#' @title Balanced Sample Size
#' @description Create a vector of balanced (equal) sample sizes for use in 
#'   the \code{sampsize} argument of \code{\link{rfPermute}} or 
#'   \code{\link[randomForest]{randomForest}} for a classification model. The 
#'   values are derived from a percentage of the smallest class sample size.
#' 
#' @param y character, numeric, or factor vector containing classes of 
#'   response variable. Values will be treated as unique for computing class 
#'   frequencies.
#' @param pct percent of smallest class frequency for \code{sampsize} vector.
#' 
#' @return a named vector of sample sizes as long as the number of classes.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(mtcars)
#' 
#' # A balanced model with default half of smallest class size
#' sampsize_0.5 <- balancedSampsize(mtcars$am)
#' sampsize_0.5
#' 
#' rfPermute(factor(am) ~ ., mtcars, replace = FALSE, sampsize = sampsize_0.5)
#' 
#' # A balanced model with one quarter of smallest class size
#' sampsize_0.25 <- balancedSampsize(mtcars$am, pct = 0.25)
#' sampsize_0.25
#' 
#' rfPermute(factor(am) ~ ., mtcars, replace = FALSE, sampsize = sampsize_0.25)
#' 
#' 
#' @export
#' 
balancedSampsize <- function(y, pct = 0.5) {
  if(!dplyr::between(pct, 0, 1)) {
    stop("'pct' must be between 0 and 1")
  }
  freq <- table(y)
  if(any(freq < 2)) {
    too.small <- paste(names(freq[freq < 2]), collapse = ", ")
    stop("The following classes have < 2 cases:", too.small)
  }
  sampsize <- rep(ceiling(min(freq) * pct), length(freq))
  stats::setNames(sampsize, names(freq))
}