#' @title Plot Random Forest Importance Null Distributions
#' @description Plot the Random Forest null distributions importance metrics, 
#' observed values, and p-values for each predictor variable from the 
#' object produced by a call to \code{\link{rfPermute}}.
#' 
#' @param x An object produced by a call to \code{\link{rfPermute}}.
#' @param preds a character vector of predictors to plot. If \code{NULL}, then 
#'   all predictors are plotted.
#' @param imp.type A character vector giving the importance metric(s) to plot.
#' @param scale Plot importance measures scaled (divided by) standard errors?
#' @param plot.type type of plot to produce: \code{"density"} for smoothed density 
#'   plot, or \code{"hist"} for histogram.
#' @param plot display the plot?
#' 
#' @details The function will generate an plot for each predictor, with facetted 
#'   importance metrics. The vertical red line shows the observed importance 
#'   score and the _p_-value is given in the facet label. 
#'   
#' @return A named list of the \code{ggplot} figures produced is invisibly returned.
#'   
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' # A regression model using the ozone example
#' data(airquality)
#' ozone.rp <- rfPermute(
#'   Ozone ~ ., data = airquality, ntree = 100, 
#'   na.action = na.omit, nrep = 50, num.cores = 1
#' )
#'   
#' # Plot the null distributions and observed values.
#' plotNull(ozone.rp) 
#' 
#' @export
#' 
plotNull <- function(x, preds = NULL, imp.type = NULL, scale = TRUE, 
                     plot.type = c("density", "hist"), plot = TRUE) {
  if(!inherits(x, "rfPermute")) stop("'x' is not an rfPermute model")
  imp <- randomForest::importance(as.randomForest(x))
  avail.types <- colnames(imp)
  
  if(is.null(imp.type)) imp.type <- avail.types
  if(!is.character(imp.type)) stop("'imp.type' is not a character vector")
  imp.type <- unique(imp.type)
  not.found <- imp.type[!(imp.type %in% avail.types)]
  if(length(not.found) > 0) {
    not.found <- paste(not.found, collapse = ", ")
    stop("imp.type:", not.found, "is not in 'x'")
  }
  
  sc <- if(scale) "scaled" else "unscaled"
  
  if(is.null(preds)) preds <- rownames(imp)
  not.found <- setdiff(preds, rownames(imp))
  if(length(not.found) > 0) {
    not.found <- paste(not.found, collapse = ", ")
    stop("The following predictors could not be found:", not.found)
  }
  
  plot.type <- match.arg(plot.type)
  g <- sapply(preds, function(pr) {
    pval <- x$pval[pr, imp.type, sc, drop = FALSE]
    labels <- stats::setNames(
      paste0(imp.type, "\n(p = ", sprintf("%0.3f", pval), ")"),
      imp.type
    )
    
    df <- sapply(imp.type, function(i) x$null.dist[[sc]][pr, i, ]) |> 
      as.data.frame() |> 
      tidyr::gather("imp.type", "importance") |> 
      dplyr::mutate(imp.type = factor(labels[imp.type], levels = labels))
    obs <- imp[pr, imp.type, drop = FALSE] |> 
      as.data.frame() |> 
      tibble::rownames_to_column("predictor") |> 
      tidyr::gather("imp.type", "importance", -.data$predictor) |> 
      dplyr::mutate(imp.type = factor(labels[imp.type], levels = labels))
    
    p <- ggplot2::ggplot(df, ggplot2::aes(x = .data$importance)) 
    p <- if(plot.type == "hist") {
        p + ggplot2::geom_histogram() + ggplot2::ylab("Count")
      } else {
        p + ggplot2::geom_density() + ggplot2::ylab("Density")
      } 
    p <- p + 
      ggplot2::xlab("Importance") + 
      ggplot2::ggtitle(pr) +
      ggplot2::geom_vline(
        ggplot2::aes(xintercept = .data$importance), 
        color = "red", data = obs
      ) +
      ggplot2::facet_wrap(~ imp.type, scales = "free")
  }, simplify = FALSE, USE.NAMES = TRUE)
  
  if(plot) for(p in g) print(p)
  invisible(g)
}
