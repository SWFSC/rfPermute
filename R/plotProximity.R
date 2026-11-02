#' @title Plot Random Forest Proximity Scores
#' @description Create a plot of Random Forest proximity scores using 
#'   multi-dimensional scaling.
#' 
#' @param x a \code{rfPermute} or \code{randomForest} model object.
#' @param dim.x,dim.y numeric values giving x and y dimensions to plot from 
#'   multidimensional scaling of proximity scores.
#' @param class.cols vector of colors to use for each class.
#' @param legend.type type of legend to use to label classes.
#' @param legend.loc character keyword specifying location of legend. 
#'   Can be \code{"bottom", "top", "left", "right"}.
#' @param point.size size of central points. Set to \code{NULL} for no points.
#' @param circle.size size of circles around points indicating classification.
#'   Set to NULL for no circles.
#' @param circle.border width of circle border.
#' @param group.type type of grouping to display.
#'  Ignored for regression models.
#' @param group.alpha value giving alpha transparency level for group shading. 
#'   Setting to \code{0} produces no shading. 
#' @param ellipse.level the confidence level at which to draw the ellipse.
#' @param n.contour.grid number of grid points for contour lines.
#' @param label.size size of label if \code{legend.type = `label`}.
#' @param label.alpha transparency of label background.
#' @param plot logical determining whether or not to show plot.
#' 
#' @details Produces a scatter plot of proximity scores for \code{dim.x} and 
#'   \code{dim.y} dimensions from a multidimensional scale (MDS) conversion of 
#'   proximity scores from a \code{randomForest} object. For classification 
#'   models, points are colored according to original (inner) 
#'   and predicted (outer) class.
#'   
#' @return a list with: 
#'  \item{prox.mds}{the MDS scores of the selected dimensions} 
#'  \item{g}{a \code{ggplot2} object}
#' 
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' library(randomForest)
#' data(symb.metab)
#'
#' rf <- randomForest(type ~ ., symb.metab, proximity = TRUE)
#' 
#' # With confidence ellipses
#' plotProximity(rf)
#' 
#' # With convex hulls
#' plotProximity(rf, group.type = "hull")
#' 
#' # With contours
#' plotProximity(rf, group.type = "contour")
#' 
#' # Remove the points and just show ellipses
#' plotProximity(rf, point.size = NULL, circle.size = NULL, group.alpha = 0.5)
#' 
#' # Labels instead of a legend
#' plotProximity(rf, legend.type = "label", point.size = NULL, circle.size = NULL, group.alpha = 0.5)
#' 
#' @export
#' 
plotProximity <- function(x, dim.x = 1, dim.y = 2, class.cols = NULL,
                          legend.type = c("legend", "label", "none"),
                          legend.loc = c("top", "bottom", "left", "right"),
                          point.size = 2, circle.size = 8, circle.border = 1, 
                          group.type = c("ellipse", "hull", "contour", "none"),
                          group.alpha = 0.3, ellipse.level = 0.95, 
                          n.contour.grid = 100, label.size = 4, 
                          label.alpha = 0.7, plot = TRUE) {
  
  rf <- as.randomForest(x)
  if(is.null(rf$proximity)) {
    stop("'rf' has no 'proximity' element. rerun with 'proximity = TRUE'")
  }
  
  prox.mds <- stats::cmdscale(1 - rf$proximity, k = max(c(dim.x, dim.y)))
  prox.mds <- prox.mds[, c(dim.x, dim.y)]
  mds.df <- data.frame(prox.mds, class = rf$y, predicted = rf$predicted)
  colnames(mds.df)[1:2] <- c("x", "y")
  
  g <- ggplot2::ggplot(mds.df, ggplot2::aes(.data$x, .data$y, color = .data$class)) 
  
  # Origin axes
  g <- g + 
    ggplot2::geom_hline(yintercept = 0, color = "lightgrey") +
    ggplot2::geom_vline(xintercept = 0, color = "lightgrey")
  
  legend.type <- match.arg(legend.type)
  
  # Group designators
  if(rf$type != "regression") {
    group.type <- match.arg(group.type)
    switch(
      group.type, 
      ellipse = {
        g <- g + ggplot2::stat_ellipse(
          ggplot2::aes(fill = .data$class), 
          geom = "polygon",
          alpha = group.alpha,
          level = ellipse.level,
          show.legend = legend.type == "legend"
        )
      },
      hull = {
        for(cl in unique(mds.df$class)) {
          cl.df <- mds.df[mds.df$class == cl, ]
          i <- grDevices::chull(cl.df$x, cl.df$y)
          g <- g + ggplot2::geom_polygon(
            ggplot2::aes(fill = .data$class), 
            data = cl.df[c(i, i[1]), ], 
            alpha = group.alpha,
            show.legend = legend.type == "legend"
          ) 
        }
      },
      contour = {
        g <- g + ggplot2::geom_density_2d(
          alpha = group.alpha,
          n = n.contour.grid,
          show.legend = legend.type == "legend"
        )
      }
    )
  }
  
  # Points
  if(!is.null(point.size)) {
    g <- g + ggplot2::geom_point(
      size = point.size, 
      show.legend = legend.type == "legend"
    ) 
  }
  
  # Predicted circles
  if(!is.null(circle.size)) {
    g <- g + ggplot2::geom_point(
      ggplot2::aes(color = .data$predicted), 
      shape = 21, 
      size = circle.size,
      stroke = circle.border,
      show.legend = FALSE
    )
  }
  
  # Class colors
  if(is.null(class.cols)) {
    n <- length(rf$classes)
    hues = seq(15, 375, length = n + 1)
    class.cols <- grDevices::hcl(h = hues, l = 65, c = 100)[1:n]
  }
  if(is.null(names(class.cols))) names(class.cols) <- rf$classes
  
  # Class labels
  if(legend.type == "label") {
    g <- g + ggplot2::geom_label(
      ggplot2::aes(label = .data$class), 
      data = mds.df |> 
        dplyr::group_by(.data$class) |> 
        dplyr::summarize(
          x = mean(.data$x),
          y = mean(.data$y)
        ) |> 
        dplyr::ungroup(),
      fill = "white",
      alpha = label.alpha,
      size = label.size,
      show.legend = FALSE
    )
  }
  
  # Plot decoration
  g <- g + 
    ggplot2::scale_color_manual(values = class.cols) +
    ggplot2::scale_fill_manual(values = class.cols) +
    ggplot2::labs(
      x = paste("Dimension", dim.x), 
      y = paste("Dimension", dim.y)
    ) +
    ggplot2::theme(
      legend.position = ifelse(
        legend.type == "legend",
        match.arg(legend.loc),
        "none"
      ),
      legend.title = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(color = NA, fill = NA),
      panel.background = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank()
    )
  
  if(plot) print(g)
  invisible(list(prox.mds = prox.mds, g = g))
}
