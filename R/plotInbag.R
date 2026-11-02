#' @title Plot Inbag distribution
#' @description Plot distribution of the fraction of trees that samples were
#'   inbag in the Random Forest model.
#' 
#' @param x a \code{rfPermute} or \code{randomForest} model object..
#' @param bins number of bins in histogram.
#' @param replace was sampling done with or without replacement?
#' @param sampsize sizes of samples drawn. Either a single value or vector of 
#'   sample sizes as long as the number of classes.
#' @param plot display the plot?
#'   
#' @note Red vertical lines on the plot denote the expected inbag rate(s). 
#'   These rates are based on the values of \code{replace} and 
#'   \code{sampsize} supplied. If not specified, they are set to the 
#'   \code{\link[randomForest]{randomForest}} defaults. If this is not the 
#'   same as the arguments used to run the model, there will be a mismatch in 
#'   the location of these indicator lines and the inbag frequency distribution.   
#'   
#' @return the \code{ggplot2} object is invisibly returned. 
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#' 
#' rf <- randomForest(factor(am) ~ ., data = mtcars, ntree = 10)
#' plotInbag(rf)
#' 
#' rf <- randomForest(factor(am) ~ ., data = mtcars, ntree = 1000)
#' plotInbag(rf)
#' 
#' rf <- randomForest(factor(am) ~ ., data = mtcars, ntree = 10000)
#' plotInbag(rf)
#' 
#' rf <- randomForest(factor(am) ~ ., data = mtcars, ntree = 10000, sampsize = c(5, 5))
#' plotInbag(rf, sampsize = c(5, 5))
#' 
#' @export
#'
plotInbag <- function(x, bins = 10, replace = TRUE, sampsize = NULL, 
                      plot = TRUE) {
  rf <- as.randomForest(x)
  
  if(!is.null(sampsize)) {
    length.good <- length(sampsize) == 1 | 
      (rf$type == "classification" & length(sampsize) == length(unique(rf$y)))
    if(!length.good) {
      stop("sampsize' must be of length one or the number of classes in 'x'")
    }
  }
  
  # number of individuals
  k <- length(rf$y)
  # number of individuals per class
  n <- table(rf$y)
  # check sampsize
  if(is.null(sampsize)) sampsize <- if(replace) k else ceiling(0.632 * k)
  
  # compute expected percent
  exp.pct <- if(replace) {
    if(length(sampsize) == 1) {
      pct <- 1 - (1 - (1 / sampsize)) ^ sampsize
      if(is.nan(pct)) pct <- 1 - (1 / exp(1))
      pct * 100
    } else {
      as.vector((1 - ((n - 1) / n) ^ sampsize)) * 100
    }
  } else if(length(sampsize) == 1) {
    (sampsize / k) * 100
  } else {
    as.vector(sampsize / n) * 100
  }
  
  # convert expected percent to data frame
  exp.pct <- if(rf$type == "classification") {
    data.frame(group = levels(rf$y), exp.pct = exp.pct) 
  } else {
    data.frame(group = 1, exp.pct = exp.pct)
  }
  
  # observed percent inbag
  obs.pct <- data.frame(
    group = rf$y,
    pct = ((rf$ntree - rf$oob.times) / rf$ntree) * 100
  )
  
  # create histograms
  p <- obs.pct |> 
    ggplot2::ggplot(ggplot2::aes(x = .data$pct)) +
    ggplot2::geom_histogram(bins = bins) +
    ggplot2::labs(
      x = "Percent of trees where sample was inbag",
      y = "Frequency"
    ) + 
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = exp.pct), 
      data = exp.pct, 
      color = "red"
    )
  if(length(sampsize) > 1) p <- p + ggplot2::facet_wrap(~.data$group, scales = 'free')
  
  # plot histograms
  if(plot) print(p)
  
  # summarize inbag rates
  smry.vals <- c('mean', 'median', 'mode', 'min', 'max', 'sd', 'ci.lower', 'ci.upper')
  inbag.smry <- suppressWarnings(if(length(sampsize) == 1) {
    c(
      expected = exp.pct$exp.pct[1],
      swfscMisc::distSmry(obs.pct$pct, method = 'venter')[smry.vals] 
    ) 
  } else {
    smry <- do.call(
      rbind,
      tapply(
        obs.pct$pct, 
        obs.pct$group, 
        swfscMisc::distSmry, 
        method = 'venter'
      )
    ) |> 
      t()
    rbind(
      expected = tibble::deframe(exp.pct), smry[smry.vals, ])
  })
  
  # show summary
  message('Percent inbag summary:')
  print(inbag.smry)
  
  invisible(list(plot = p, smry = inbag.smry))
}