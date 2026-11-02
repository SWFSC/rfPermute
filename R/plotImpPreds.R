#' @title Plot Important Predictor Distribution
#' @description For classification models, plot distribution of predictor 
#'   variables on classes sorted by order of importance in model.
#' 
#' @param x a \code{rfPermute} or \code{randomForest} model object.
#' @param df data.frame with predictors in \code{rf} model.
#' @param class.col response column name in \code{df}.
#' @param imp.type character string representing importance type to use for 
#'   sorting predictors.
#' @param max.vars number of variables to plot (from most important to least).
#' @param scale For permutation based importance measures, should they be divided 
#'   their "standard errors"?
#' @param size,point.alpha,violin.alpha controls size of points and alpha 
#'   values (transparency) for points and violin plots.
#' @param plot display the plot?
#'   
#' @return the \code{ggplot2} object is invisibly returned.
#' 
#' @note If the model in \code{x} is from \code{randomForest} and was run 
#'   with \code{importance = TRUE}, then 'MeanDecreaseAccuracy' is used as 
#'   the default importance measure for sorting. Otherwise, 'MeanDecreaseGini' 
#'   is used.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#' 
#' df <- mtcars
#' df$am <- factor(df$am)
#' 
#' rf <- randomForest(am ~ ., df, importance = TRUE)
#' plotImpPreds(rf, df, "am")
#' 
#' @export
#'
plotImpPreds <- function(x, df, class.col, imp.type = NULL, max.vars = 16, 
                         scale = TRUE, size = 1, point.alpha = 0.2,
                         violin.alpha = 0.5, plot = TRUE) {
  rf <- as.randomForest(x)
  if(rf$type != "classification") stop("'x' must be a classification model")
  
  if(!class.col %in% colnames(df)) {
    stop("'class.col' = \"", class.col, "\" cannot be found in 'df'")
  }
  
  imp.mat <- randomForest::importance(rf, scale = scale)
  imp.type <- if(is.null(imp.type)) {
    if("MeanDecreaseAccuracy" %in% colnames(imp.mat)) {
      "MeanDecreaseAccuracy" 
    } else {
      "MeanDecreaseGini"
    }
  } else {
    imp.type <- imp.type[1]
    if(!imp.type %in% colnames(imp.mat)) {
      stop("'x' does not contain imp.type:", imp.type)
    }
  }
  
  imp.vars <- rownames(imp.mat)[order(imp.mat[, imp.type], decreasing = TRUE)]
  imp.vars <- imp.vars[1:min(c(length(imp.vars), max.vars))]
  
  if(!all(imp.vars %in% colnames(df))) {
    stop("some predictors in 'x' can't be found in 'df'")
  }
  df$.class. <- factor(df[[class.col]])
  df <- df[, c(".class.", imp.vars)]
  
  p <- df |>
    tidyr::pivot_longer(-.data$.class., names_to = "var") |> 
    dplyr::mutate(var = factor(.data$var, levels = imp.vars)) |> 
    ggplot2::ggplot(ggplot2::aes(x = .data$.class., y = .data$value)) +
    ggplot2::geom_violin(alpha = violin.alpha) +
    ggplot2::geom_jitter(size = size, alpha = point.alpha, width = 0.25, height = 0) +
    ggplot2::facet_wrap(~ var, scales = "free_y") +
    ggplot2::labs(x = class.col, y = "Value") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))  
  
  if(plot) print(p)
  invisible(p)
}
