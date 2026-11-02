#' @title Class Priors
#' @description Compute the class classification priors and class-specific 
#'   model binomial p-values using these priors as null hypotheses.
#' 
#' @param x a \code{rfPermute} or \code{randomForest} model object.
#' @param sampsize the vector of sample sizes used to construct the model. If 
#'   provided, must have length equal to number of classes. If set to
#'   \code{NULL}, priors will be computed assuming empirical sample sizes.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov} 
#' 
#' @seealso \code{\link{balancedSampsize}}, \code{\link{confusionMatrix}}
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#'
#' # random sampling with replacement
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' confusionMatrix(rf)
#' classPriors(rf, NULL)
#' 
#' # balanced design
#' sampsize <- balancedSampsize(mtcars$am)
#' rf <- randomForest(factor(am) ~ ., mtcars, replace = FALSE, sampsize = sampsize)
#' confusionMatrix(rf)
#' classPriors(rf, sampsize)
#' 
#' @export
#' 
classPriors <- function(x, sampsize) {
  rf <- as.randomForest(x)
  if(rf$type != "classification") stop("'x' must be of a classification model")
  
  # Confusion matrix and counts
  cm <- .confMat(rf)
  class.n <- rowSums(cm)
  all.n <- c(class.n, Overall = sum(class.n))
  total.n <- sum(class.n)
  correct.n <- diag(cm)
  correct.n <- c(correct.n, Overall = sum(correct.n))
  
  # Expected error rates (priors)
  prior <- if(is.null(sampsize)) class.n / total.n else {
    if(length(sampsize) != nrow(cm)) {
      stop("length of 'sampsize' is not equal to number of classes in 'x'.")
    }
    if(is.null(names(sampsize))) names(sampsize) <- rownames(cm)
    sampsize / sum(sampsize)
  }
  prior <- prior[rownames(cm)]
  prior <- c(prior, Overall = sum(prior * class.n) / total.n)
  class_p.value <- 1 - stats::pbinom(correct.n, all.n, prior)
  
  cbind(
    prior = (prior * 100)[names(all.n)], 
    class_p.value = class_p.value[names(all.n)]
  )
}