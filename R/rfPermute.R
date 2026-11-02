#' @name rfPermute
#' @title Estimate Permutation p-values for Random Forest Importance Metrics
#' @description Estimate significance of importance metrics for a Random Forest 
#'   model by permuting the response variable. Produces null distribution of 
#'   importance metrics for each predictor variable and p-value of observed.
#'
#' @param x,y,formula,data,subset,na.action,\dots See \code{\link[randomForest]{randomForest}} 
#'   for definitions. In \code{as.randomForest} this is either a 
#'   \code{randomForest} or \code{rfPermute} object to be converted to a
#'   \code{randomForest} object.
#' @param num.rep Number of permutation replicates to run to construct 
#'   null distribution and calculate p-values (default = 100).
#' @param num.cores Number of CPUs to distribute permutation results over. 
#'   Defaults to \code{NULL} which uses one fewer than the number of cores 
#'   reported by \code{\link[parallel]{detectCores}}.
#' @param object an \code{rfPermute} model to be used for prediction. See
#'   \code{\link[randomForest]{predict.randomForest}}
#'
#' @details All other parameters are as defined in \code{randomForest.formula}. 
#'   A Random Forest model is first created as normal to calculate the observed 
#'   values of variable importance. The response variable is then permuted 
#'   \code{num.rep} times, with a new Random Forest model built for each 
#'   permutation step. 
#'
#' @return An \code{rfPermute} object.
#'
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#'
#' @examples
#' # A regression model predicting ozone levels
#' data(airquality)
#' ozone.rp <- rfPermute(Ozone ~ ., data = airquality, na.action = na.omit, ntree = 100, num.rep = 50)
#' ozone.rp
#'   
#' # Plot the scaled importance distributions 
#' # Significant (p <= 0.05) predictors are in red
#' plotImportance(ozone.rp, scale = TRUE)
#' 
#' # Plot the importance null distributions and observed values for two of the predictors
#' plotNull(ozone.rp, preds = c("Solar.R", "Month"))
#'
#'
#' # A classification model classifying cars to manual or automatic transmission 
#' data(mtcars)
#' 
#' am.rp <- rfPermute(factor(am) ~ ., mtcars, ntree = 100, num.rep = 50)
#' summary(am.rp)
#' 
#' 
#' plotImportance(am.rp, scale = TRUE, sig.only = TRUE)
#' 
#' 
#'
#' @export
#' 
rfPermute <- function(x, ...) UseMethod("rfPermute")


#' @rdname rfPermute
#' @export
#' 
rfPermute.default <- function(x, y = NULL, ..., num.rep = 100, num.cores = 1) {  
  if(!is.numeric(num.rep)) stop("'num.rep' must be a number.")
  num.rep <- round(num.rep, 0)
  if(num.rep < 1) stop("'num.rep' must a positive value.")
  
  run.start <- Sys.time()
  
  # parse function call
  orig.call <- match.call()
  orig.call$num.rep <- orig.call$num.cores <- NULL
  orig.call[[1]] <- as.name("randomForest")
  rf.call <- orig.call
  rf.call$x <- x
  rf.call$y <- y
  
  # partial match call names for importance and set to TRUE
  imp.match <- pmatch(names(rf.call), "importance")
  if(all(is.na(imp.match))) {
    rf.call$importance <- TRUE
  } else {
    is.imp <- names(rf.call)[which(!is.na(imp.match))]
    rf.call[[is.imp]] <- TRUE
  }
  
  rf.call[-1] <- lapply(as.list(rf.call[-1]), eval, envir = parent.frame())
  rf <- eval(rf.call)
  rf$call <- orig.call
  
  # Permute 'y' in rf.call 'num.rep' times and runs randomForest 
  # Setup number of cores
  if(is.null(num.cores)) num.cores <- parallel::detectCores() - 1
  if(is.na(num.cores)) num.cores <- 1
  num.cores <- max(1, num.cores)
  num.cores <- min(parallel::detectCores() - 1, num.cores)
  
  # Create list of permuted y values
  ran.y <- lapply(1:num.rep, function(i) sample(rf.call$y))
  call.x <- x
  
  # Set arguments in randomForest call not needed in permutations
  rf.call[c('proximity', 'do.trace', 'keep.forest', 'keep.inbag')]  <- FALSE
  
  # Get importance scores for permutations
  #  a list of 3-dimensional arrays of importance scores
  max.cores <- parallel::detectCores() - 1
  if(is.na(max.cores)) max.cores <- 1
  if(max.cores < 1) max.cores <- 1
  if(is.null(num.cores)) num.cores <- max.cores
  if(num.cores > max.cores) num.cores <- max.cores
  num.cores <- min(num.cores, max.cores)
  
  null.dist.list <- if(num.cores == 1) {   
    lapply(ran.y, .permFunc, call.x = call.x, perm.rf.call = rf.call)
  } else if(.Platform$OS.type == "windows") {
    cl <- parallel::makePSOCKcluster(num.cores)
    tryCatch({
      if(is.null(cl)) NULL else {
        parallel::clusterEvalQ(cl, require(randomForest))
        parallel::clusterExport(
          cl = cl, 
          varlist = c("call.x", "rf.call"), 
          envir = environment()
        )
        null.dist <- parallel::parLapplyLB(
          cl, ran.y, .permFunc, call.x = call.x, perm.rf.call = rf.call
        )
        null.dist
      }
    }, finally = parallel::stopCluster(cl))
  } else {
    parallel::mclapply(
      ran.y, .permFunc, call.x = call.x, perm.rf.call = rf.call, 
      mc.cores = num.cores
    )
  }
  
  # create and load null distribution arrays for each scaled and unscaled importances
  null.dist <- list(unscaled = .makeImpArray(rf$importance, num.rep, NULL))
  null.dist$scaled <- null.dist$unscaled
  for(i in 1:num.rep) {
    null.dist$unscaled[, , i] <- null.dist.list[[i]][, , "unscaled"]
    null.dist$scaled[, , i] <- null.dist.list[[i]][, , "scaled"]
  }
  
  # calculate p-value of observed importance metrics
  pval <- .calcImpPval(rf, null.dist)

  result <- list(
    rf = rf, 
    null.dist = null.dist, 
    pval = pval, 
    num.rep = num.rep, 
    start = run.start,
    end = Sys.time()
  )
  class(result) <- "rfPermute"
  result
}


#' @rdname rfPermute
#' @importFrom stats na.fail
#' @export
#' 
rfPermute.formula <- function(formula, data = NULL, ..., subset, 
                              na.action = na.fail, 
                              num.rep = 100, num.cores = 1) {
  if (!inherits(formula, "formula")) stop("method is only for formula objects")
  m <- match.call(expand.dots = FALSE)
  if (any(c("xtest", "ytest") %in% names(m))) {
    stop("xtest/ytest not supported through the formula interface")
  }
  
  # extract formula terms
  names(m)[2] <- "formula"
  if (is.matrix(eval(m$data, parent.frame()))) m$data <- as.data.frame(data)
  m$... <- m$num.rep <- m$num.cores <- NULL
  m$na.action <- na.action
  m[[1]] <- as.name("model.frame")
  m <- eval(m, parent.frame())
  y <- stats::model.response(m)
  Terms <- attr(m, "terms")
  attr(Terms, "intercept") <- 0
  m <- stats::model.frame(
    stats::terms(stats::reformulate(attributes(Terms)$term.labels)), 
    data.frame(m)
  )
  for (i in seq(along = ncol(m))) {
    if (is.ordered(m[[i]])) m[[i]] <- as.numeric(m[[i]])
  }
  
  # run rfPermute.default
  rf.call <- match.call()
  rf.call[[1]] <- as.name("rfPermute")
  names(rf.call)[2:3] <- c("x", "y")
  rf.call$x <- m
  rf.call$y <- y
  rf.call$subset <- rf.call$na.action <- NULL
  rf.call[-1] <- lapply(rf.call[-1], eval, envir = parent.frame())
  rp <- eval(rf.call)
  
  # reconstitute original randomForest call
  rf.call <- match.call()
  rf.call[[1]] <- as.name("randomForest")
  rp$rf$call <- rf.call
  rp$rf$terms <- Terms
  if (!is.null(attr(m, "na.action"))) rp$rf$na.action <- attr(m, "na.action")
  class(rp$rf) <- c("randomForest.formula", "randomForest")
  
  rp
}


#' @rdname rfPermute
#' @export
#' 
as.randomForest <- function(x) {
  if(inherits(x, "randomForest")) return(x)
  if(inherits(x, "rfPermute")) return(x$rf)
  stop("'x' is not a randomForest or rfPermute object.")
}


#' @rdname rfPermute
#' @export
#' 
print.rfPermute <- function(x, ...) {
  op <- options(digits = 3)
  rf <- x$rf
  cat("An rfPermute model\n\n")
  cat("               Type of random forest:", rf$type, "\n")
  cat("                     Number of trees:", rf$ntree, "\n")
  cat("No. of variables tried at each split:", rf$mtry, "\n")
  cat("       No. of permutation replicates:", x$num.rep, "\n")
  cat("                          Start time:", format(x$start), "\n")
  cat("                            End time:", format(x$end), "\n")
  cat("                            Run time:", format(difftime(x$end, x$start), digits = 3), "\n")

  if(rf$type == "regression") {
    if (!is.null(rf$mse)) {
      cat("\n")
      cat("          Mean of squared residuals:", rf$mse[length(rf$mse)], "\n")
      pct.var <- round(100 * rf$rsq[length(rf$rsq)], digits = 2)
      cat("                    % Var explained:", pct.var, "\n")
      if (!is.null(rf$test$mse)) {
        test.mse <- round(rf$test$mse[length(rf$test$mse)], digits = 2)
        cat("                       Test set MSE:", test.mse, "\n")
        pct.var.exp <- round(100 * rf$test$rsq[length(rf$test$rsq)], digits = 2)
        cat("                    % Var explained:", pct.var.exp, "\n")
      }
    }
    if (!is.null(x$rf$coefs)) {
      cat("\n")
      cat("  Bias correction applied:\n")
      cat("  Intercept:", rf$coefs[1], "\n")
      cat("      Slope:", rf$coefs[2], "\n")
    }
  } else {
    cat("\n")
    print(confusionMatrix(x))
    cat("\n")
  }
  options(op)
}


#' @rdname rfPermute
#' @importFrom stats predict
#' @export
#' 
predict.rfPermute <- function(object, ...) predict(as.randomForest(object), ...)


#' @rdname rfPermute
#' @export
#' 
rfPermuteTutorial <- function() {
  utils::browseURL(system.file("rfPermute_Tutorial.html", package = "rfPermute"))
}