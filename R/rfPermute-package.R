#' \code{rfPermute} package
#' 
#' Random Forest Predictor Importance Significance and Model Diagnostics.
#' 
#' @aliases rfPermute-package NULL
#' @keywords internal 
"_PACKAGE"
#' 
#' @importFrom randomForest randomForest
#' @importFrom rlang .data
#' @importFrom methods new
#' 
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "Welcome to rfPermute v", utils::packageVersion("rfPermute"), "\n",
    "See rfPermuteTutorial() for a guide to the package."
  )
}
