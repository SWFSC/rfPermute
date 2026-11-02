#' @title Receiver Operator Curve
#' @description Create a Receiver Operator Curve (ROC) for each class in a 
#'   Random Forest model and (optionally) plot them.
#' 
#' @param x a \code{rfPermute} or \code{randomForest} model object.
#' @param plot logical? Plot the curve?
#' 
#' @return \describe{ a list returned invisbly containing:
#'  \item{\code{roc}}{a list containing ROC results from \code{\link[pROC]{roc}} for each class}
#'  \item{\code{smry}}{a data fame summarizing the threshold and AUC for each class}
#'  \item{\code{plot}}{the \code{ggplot} plot of the ROC curves}
#'}
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov} 
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#'
#' # random sampling with replacement
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' 
#' roc <- rfROC(rf)
#' 
#' roc$smry
#' 
#' @export
#'
rfROC <- function(x, plot = TRUE) {
  rf <- as.randomForest(x)
  if(rf$type != "classification") stop("'x' must be of a classification model")
  
  roc.list <- sapply(levels(rf$y), function(cls) {
    class.roc <- pROC::roc(
      as.numeric(rf$y == cls),
      rf$votes[, cls], 
      direction = "<",
      quiet = TRUE
    )
    
    df <- data.frame(
      sensitivity = class.roc$sensitivities,
      specificity = class.roc$specificities,
      threshold = class.roc$thresholds
    ) |> 
      dplyr::mutate(
        class = cls,
        threshold = ifelse(.data$threshold == -Inf, 0, .data$threshold),
        threshold = ifelse(.data$threshold == Inf, 1, .data$threshold)
      ) |> 
      dplyr::arrange(dplyr::desc(.data$specificity), .data$sensitivity, .data$threshold)
    
    list(roc = class.roc, df = df)
  }, simplify = FALSE)
  
  smry <- do.call(
    rbind,
    lapply(roc.list, function(r) {
      pROC::coords(
        roc = r$roc, 
        x = "best", 
        ret = c("threshold", "sensitivity", "specificity")
      ) |> 
        dplyr::mutate(
          auc = as.vector(pROC::auc(r$roc)),
          class = unique(r$df$class)
        ) |> 
        dplyr::select(.data$class, dplyr::everything())
    })
  ) |> 
    tibble::remove_rownames()
  
  p <- do.call(
    rbind,
    lapply(roc.list, function(x) x$df)
  ) |> 
    ggplot2::ggplot(ggplot2::aes(.data$specificity, .data$sensitivity)) +
    ggplot2::annotate(
      'segment', 
      x = 1, 
      y = 0, 
      xend = 0, 
      yend = 1, 
      linetype = 'dashed', 
      color = 'gray'
    ) +
    ggplot2::geom_line(ggplot2::aes(color = .data$threshold), linewidth = 2) +
    ggplot2::geom_text(data = smry, label = 'T') +
    ggplot2::labs(x = 'Specificity', y = 'Sensitivity') +
    ggplot2::scale_color_distiller(
      name = 'Threshold',
      palette = 'Spectral',
      limits = c(0, 1),
      rescaler  = ~scales::rescale_mid(., mid = 0.5),
      direction = -1
    ) + 
    ggplot2::scale_x_reverse() +
    ggplot2::facet_wrap('class') +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.key.height = ggplot2::unit(1, "null")) 
  if(plot) plot(p)
  
  print(smry)
  
  invisible(list(
    roc = sapply(roc.list, function(x) x$roc, simplify = FALSE),
    smry = smry,
    p = p
  ))
}