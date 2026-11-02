#' @name importance
#' @title Extract rfPermute Importance Scores and p-values.
#' @description The \code{importance} function extracts a matrix of 
#'   the observed importance scores and p-values from the object 
#'   produced by a call to \code{rfPermute}. \code{plotImportance} produces 
#'   a visualization of importance scores as either a barchart or heatmap.
#' 
#' @param x for \code{importance}, an object produced by a call to 
#'   \code{rfPermute}. For \code{plotImportance}, either a \code{rfPermute} 
#'   or \code{randomForest} model object. If the latter, it must have been 
#'   run with \code{importance = TRUE}.
#' @param scale For permutation based measures, should the measures be divided 
#'   their "standard errors"?
#' @param sort.by character vector giving the importance metric(s) or p-values 
#'   to sort by. If \code{NULL}, defaults to \code{"MeanDecreaseAccuracy"} for 
#'   classification models and \code{"\%IncMSE"} for regression models.
#' @param decreasing logical. Should the sort order be increasing or decreasing?
#' @param plot.type plot importances as a \code{bar} chart or \code{heatmap}?
#' @param imp.type character vector listing which importance measures to plot.
#'   Can be class names (for classification models) or names of overall 
#'   importance measures (e.g., "MeanDecreaseAccuracy").
#' @param scale for permutation based measures, should the measures be divided 
#'   their "standard errors"?
#' @param sig.only Plot only the significant (<= \code{alpha}) predictors?
#' @param alpha a number specifying the critical alpha for identifying 
#'   predictors with importance scores significantly different from random. 
#'   This parameter is only relevant if \code{rf} is a \code{\link{rfPermute}}
#'   object with p-values. Importance measures with p-values less than or 
#'   equal to \code{alpha} will be denoted in barcharts in red and in the
#'   heatmap by a white diamond. If set to \code{NULL}, significance is not 
#'   denoted.
#' @param n plot \code{n} most important predictors.
#' @param ranks plot ranks instead of actual importance scores?
#' @param xlab,ylab labels for the x and y axes.
#' @param main main title for plot.
#' @param size a value specifying the size of the significance diamond in the 
#'   heatmap if the p-value <= \code{alpha}.
#' @param plot display the plot?
#' @param ... arguments to be passed to and from other methods.
#'   
#' @author Eric Archer \email{eric.archer@@noaa.gov}
#' 
#' @examples
#' data(mtcars)
#' 
#' # A classification model classifying cars to manual or automatic transmission 
#' am.rp <- rfPermute(factor(am) ~ ., mtcars, ntree = 100, nrep = 50)
#'   
#' imp.scaled <- importance(am.rp, scale = TRUE)
#' imp.scaled
#' 
#' # plot scaled importance scores
#' plotImportance(am.rp, scale = TRUE)
#' 
#' # plot unscaled and only significant scores
#' plotImportance(am.rp, scale = FALSE, sig.only = TRUE)
#' 
#' @importFrom randomForest importance
#' @method importance rfPermute
#' @export importance
#' @export
#' 
importance.rfPermute <- function(x, scale = TRUE, sort.by = NULL, 
                                 decreasing = TRUE, ...) {  
  if((!is.character(sort.by) & !is.vector(sort.by)) & !is.null(sort.by)) {
    stop("'sort.by' is not a character vector")
  }
  
  imp <- if(scale) {
    .scaleImp(x$rf$importance, x$rf$importanceSD)
  } else {
    x$rf$importance
  }
  
  pval <- x$pval[, , if(scale) "scaled" else "unscaled"]
  colnames(pval) <- paste(colnames(pval), ".pval", sep = "")
  pred <- rownames(imp)
  vals <- do.call(cbind, lapply(1:ncol(imp), function(i) {
    cbind(imp[pred, i, drop = FALSE], pval[pred, i, drop = FALSE])
  }))
  
  if(is.null(sort.by)) {
    sort.by <- ifelse(
      x$rf$type == "regression", "%IncMSE", "MeanDecreaseAccuracy"
    )
  }
  not.found <- sort.by[!(sort.by %in% colnames(vals))]
  if(length(not.found) > 0) {
    not.found <- paste(not.found, collapse = ", ")
    stop(paste("sort.by: ", not.found, " not found", sep = ""))
  }
  
  order.list <- lapply(sort.by, function(i) vals[, i, drop = FALSE])
  order.list <- c(order.list, decreasing = decreasing)
  vals[do.call(order, order.list), , drop = FALSE]
}


#' @rdname importance
#' @export
#' 
plotImportance <- function(x, plot.type = c("bar", "heatmap"), imp.type = NULL,
                           scale = TRUE, sig.only = FALSE, alpha = 0.05,
                           n = NULL, ranks = TRUE, xlab = NULL, ylab = NULL, 
                           main = NULL, size = 3, plot = TRUE) { 
  
  has.imp <- (inherits(x, "randomForest") & !is.null(x$importance)) | 
    inherits(x, "rfPermute")
  if(!has.imp) {
    stop("'x' is not a randomForest object with variable importance scores or a rfPermute object.")
  }
  
  rf <- as.randomForest(x)
  imp.mat <- importance(rf, scale = scale)
  if(is.null(imp.type)) imp.type <- colnames(imp.mat)
  not.found <- setdiff(imp.type, colnames(imp.mat))
  if(length(not.found) > 0) {
    not.found <- paste(not.found, collapse = ", ")
    stop(paste(
      "the following columns in 'type' can't be found in 'x':", 
      not.found
    ))
  }
  if(is.null(n)) n <- nrow(imp.mat)
  if(!is.numeric(n) | n < 1) stop("'n' must be a number >= 1")
  n <- min(n, nrow(imp.mat))
  
  imp.type <- colnames(imp.mat)[match(imp.type, colnames(imp.mat))]
  sc <- if(scale) "scaled" else "unscaled"
  if(is.null(alpha)) alpha <- -Inf
  
  plot.type <- match.arg(plot.type)
  imp.plot <- if(plot.type == "bar") {
    imp.list <- lapply(imp.type, function(i) {
      imp.df <- data.frame(
        imp = imp.mat[, i],
        pval = if(inherits(x, "rfPermute")) {
          x$pval[rownames(imp.mat), i, sc]
        } else Inf
      ) |> 
        tibble::rownames_to_column("pred") |> 
        dplyr::mutate(
          is.sig = .data$pval <= alpha,
          pred = stats::reorder(.data$pred, .data$imp)
        ) |> 
        dplyr::arrange(-.data$imp)
      if(sig.only) imp.df <- imp.df[imp.df$is.sig, ]
      n <- min(n, nrow(imp.df))
      imp.df <- imp.df[1:n, ]
      imp.df$is.sig <- as.character(imp.df$is.sig)
      
      ggplot2::ggplot(
        imp.df, 
        ggplot2::aes(x = .data$pred, y = .data$imp, fill = .data$is.sig) 
      ) + 
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::coord_flip() + 
        ggplot2::ggtitle(i) + 
        ggplot2::scale_fill_manual(
          values = c("FALSE" = "black", "TRUE" = "red")
        ) +
        ggplot2::theme(
          legend.position = "none",
          axis.title = ggplot2::element_blank()
        )
    })
    imp.list$top <- main
    imp.list$bottom <- "Importance"
    imp.list
  } else {
    preds <- rownames(imp.mat)
    imp.val <- if(rf$type == "regression") "%IncMSE" else "MeanDecreaseAccuracy"
    preds <- preds[order(imp.mat[, imp.val])]
    preds <- rev(rev(preds)[1:n])
    imp.df <- data.frame(imp.mat[preds, imp.type, drop = FALSE], check.names = FALSE)
    if(ranks) for(i in imp.type) imp.df[[i]] <- rank(-imp.df[[i]])
    imp.df <- imp.df |> 
      tibble::rownames_to_column("pred") |> 
      tidyr::pivot_longer(-.data$pred, names_to = "type") |> 
      dplyr::mutate(
        type = factor(.data$type, levels = imp.type),
        pred = factor(.data$pred, levels = preds)
      )
    
    # create plot
    g <- ggplot2::ggplot(imp.df, ggplot2::aes(x = .data$type, y = .data$pred)) +
      ggplot2::geom_raster(ggplot2::aes(fill = .data$value)) + 
      ggplot2::theme(panel.background = ggplot2::element_blank())
    g <- g + if(ranks) {
      ggplot2::scale_fill_gradient2(
        "Rank", low = "#a50026", mid = "#ffffbf", high = "#313695",
        midpoint = mean(range(imp.df$value)), 
        guide = ggplot2::guide_colorbar(reverse = TRUE)
      )
    } else {
      ggplot2::scale_fill_gradient2(
        "MeanDecreaseAccuracy", low = "#313695", mid = "#ffffbf", high = "#a50026",
        midpoint = mean(range(imp.df$value))
      )
    }
    g <- g + if(is.null(xlab)) {
      ggplot2::theme(axis.title.x = ggplot2::element_blank())
    } else {
      ggplot2::xlab(xlab)
    }
    g <- g + if(is.null(ylab)) {
      ggplot2::theme(axis.title.y = ggplot2::element_blank()) 
    } else {
      ggplot2::ylab(ylab)
    }
    if(!is.null(main)) g <- g + ggplot2::ggtitle(main)
    
    if(inherits(x, "rfPermute") & !is.null(alpha)) {
      sc <- ifelse(scale, "scaled", "unscaled")
      sig <- sapply(1:nrow(imp.df), function(i) {
        pred <- as.character(imp.df$pred[i])
        cl <- as.character(imp.df$type[i])
        x$pval[pred, cl, sc] <= alpha
      })
      if(any(sig)) {
        sig.df <- imp.df[sig, ]
        g <- g + ggplot2::geom_point(
          data = imp.df[sig, ], size = size, shape = 23, 
          fill = "white", color = "black"
        )
      }
    }
    g
  }
  
  if(plot) {
    if(plot.type == "bar") {
      suppressWarnings(do.call(gridExtra::grid.arrange, imp.plot))
    } else {
      print(imp.plot)
    }
  }
  invisible(imp.plot)
}
