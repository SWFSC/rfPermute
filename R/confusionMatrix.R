#' @title Confusion Matrix
#' @description Generate a confusion matrix for Random Forest classification 
#'   models with error rates translated into percent correctly classified, 
#'   and columns for confidence intervals added.
#' 
#' @param x a \code{rfPermute} or \code{randomForest} model object.
#' @param conf.level confidence level for the \code{\link{binom.test}} 
#'   confidence interval
#' @param threshold threshold to test observed classification 
#'   probability against. Should be a number between 0 and 1. 
#'   If not \code{NULL}, the output matrix will have extra 
#'   columns giving the one-tailed probability that the true correct 
#'   classification is >= \code{threshold}. 
#' @param title a title for the plot.
#' @param plot display the plot?
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov} 
#' 
#' @seealso \code{\link{classPriors}}
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#'
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' confusionMatrix(rf)
#' 
#' confusionMatrix(rf, conf.level = 0.75)
#' 
#' confusionMatrix(rf, threshold = 0.7)
#' confusionMatrix(rf, threshold = 0.8)
#' confusionMatrix(rf, threshold = 0.95)
#' 
#' @export
#' 
confusionMatrix <- function(x, conf.level = 0.95, threshold = NULL) {
  rf <- as.randomForest(x)
  if(rf$type != "classification") stop("'x' must be of a classification model")
  
  # Confusion matrix and counts
  cm <- .confMat(rf)
  class.n <- rowSums(cm)
  all.n <- c(class.n, Overall = sum(class.n))
  total.n <- sum(class.n)
  correct.n <- diag(cm)
  correct.n <- c(correct.n, Overall = sum(correct.n))
  
  # Confidence intervals
  ci <- t(sapply(
    mapply(
      stats::binom.test, 
      x = correct.n, 
      n = all.n, 
      p = correct.n / all.n, 
      conf.level = conf.level,
      SIMPLIFY = FALSE
    ),
    function(x) x$conf.int * 100
  ))
  colnames(ci) <- paste(c("LCI", "UCI"), conf.level, sep = "_")
  
  # Probability threshold
  prob.gt <- NULL
  if(!is.null(threshold)) {
    prob.gt <- lapply(threshold, function(p) {
      stats::pbinom(correct.n, total.n, p)
    })
    prob.gt <- do.call(cbind, prob.gt)
    colnames(prob.gt) <- paste0("Pr.gt_", threshold)
  }
  
  cm <- rbind(cm, Overall = rep(NA, ncol(cm)))
  pct.correct <- (correct.n / all.n) * 100
  cbind(
    n = all.n,
    cm, 
    pct.correct = pct.correct[rownames(cm)], 
    ci[rownames(cm), , drop = FALSE],
    prob.gt
  )
}


#' @rdname confusionMatrix
#' @export
#' 
plotConfMat <- function(x, title = NULL, plot = TRUE) {
  rf <- as.randomForest(x)
  if(rf$type == "regression") stop("'rf' must be of a classification model")
  
  conf <- .confMat(rf)
  pct.correct <- (100 * sum(diag(conf)) / sum(conf)) |> 
    round(0) |> 
    paste0("% correct")
  title <- if(is.null(title)) {
    pct.correct 
  } else {
    paste0(title, " (", pct.correct, ")")
  }
  freq <- rowSums(conf)
  rownames(conf) <- paste0(names(freq), " (", freq, ")")
  
  p <- conf |> 
    prop.table(1) |> 
    as.data.frame() |> 
    tibble::rownames_to_column("observed") |> 
    tidyr::gather("predicted", "prop", -.data$observed) |> 
    dplyr::mutate(
      observed = factor(.data$observed),
      observed = stats::reorder(.data$observed, dplyr::desc(.data$observed)),
      predicted = factor(.data$predicted)
    ) |> 
    ggplot2::ggplot(ggplot2::aes(x = .data$predicted, y = .data$observed)) +
    ggplot2::geom_raster(ggplot2::aes(fill = .data$prop)) +
    ggplot2::scale_fill_viridis_c(
      option = "magma", 
      direction = -1, 
      limits = c(0, 1)
    ) +
    ggplot2::scale_x_discrete(position = "top") +
    ggplot2::labs(x = "Predicted", y = "True", title = title) +
    ggplot2::guides(fill = ggplot2::guide_colorbar(title = "Proportion")) +
    ggplot2::theme(
      axis.text.x.top = ggplot2::element_text(angle = 45, hjust = 0),
      panel.background = ggplot2::element_blank(),
      legend.key.height = ggplot2::unit(1, 'null')
    )
  
  if(plot) print(p)
  invisible(p)
}
