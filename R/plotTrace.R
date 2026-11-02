#' @title Plot Trace
#' @description Plot trace of cumulative OOB (classification) or MSE 
#'   (regression) error rate by number of trees.
#' 
#' @param x a \code{rfPermute} or \code{randomForest} model object.
#' @param pct.correct display y-axis as percent correctly classified 
#'   (\code{TRUE}) or OOB error rate (\code{FALSE}).
#' @param plot display the plot?
#'   
#' @return the \code{ggplot2} object is invisibly returned.
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' library(randomForest)
#' data(mtcars)
#' 
#' rf <- randomForest(factor(am) ~ ., mtcars)
#' plotTrace(rf)
#' 
#' @export
#'
plotTrace <- function(x, pct.correct = TRUE, plot = TRUE) {
  rf <- as.randomForest(x)
  p <- if(utils::hasName(rf, "err.rate")) {
    class.cols <- c("black", scales::hue_pal()(nlevels(rf$y)))
    class.lines <- c(1, rep(2, nlevels(rf$y)))
    names(class.cols) <- names(class.lines) <- c("OOB", levels(rf$y))
    
    df <- as.data.frame(rf$err.rate)
    df |> 
      dplyr::mutate(trees = 1:dplyr::n()) |> 
      tidyr::gather("class", "error", -.data$trees) |> 
      dplyr::mutate(
        class = factor(.data$class, levels = colnames(df)),
        error = if(pct.correct) (1 - .data$error) * 100 else .data$error * 100
      ) |> 
      ggplot2::ggplot(
        ggplot2::aes(x = .data$trees, y = .data$error, color = .data$class)
      ) +
      ggplot2::geom_line(ggplot2::aes(linetype = .data$class)) +
      ggplot2::scale_color_manual(values = class.cols) +
      ggplot2::scale_linetype_manual(values = class.lines) +
      ggplot2::labs(
        x = "Trees", 
        y = ifelse(pct.correct, "Percent Correct", "OOB Error")
      ) +
      ggplot2::theme(legend.title = ggplot2::element_blank())
  } else if(utils::hasName(rf, "mse")) {
    data.frame(trees = 1:length(rf$mse), error = rf$mse) |> 
      ggplot2::ggplot(ggplot2::aes(x = .data$trees, y = .data$error)) +
      ggplot2::geom_line() +
      ggplot2::labs(x = "Trees", y = "Mean Squared Error")
  } else NULL
  
  if(is.null(p)) {
    stop(
      "The randomForest model does not have an 'err.rate' matrix or 'mse' vector. ",
      "Is it the result of a 'randomForest::combine(...)' operation?"
    )
  }
  if(plot) print(p)
  invisible(p)
}
