# Scale importance measures
# imp: matrix of predictors x importance type
# impSD: vector of standard deviation of importances for predictors
.scaleImp <- function(imp, impSD) {
  imp[, -ncol(imp)] <- imp[, -ncol(imp), drop = FALSE] / 
    ifelse(impSD < .Machine$double.eps, 1, impSD)
  return(imp)
}

# Make a 3-dimensional array of importance values
# x: initial 2-dimensional array of predictors x importance type
# z.dim: dimensionality of z axis
# z.dimnames: dimnames of z axis
.makeImpArray <- function(x, z.dim, z.dimnames) {
  array(
    x, dim = c(nrow(x), ncol(x), z.dim), 
    dimnames = list(rownames(x), colnames(x), z.dimnames)
  )
}

# Calculate importance p-values from null distribution and observed importances
# x: a list with randomForest elements and $null.dist 
.calcImpPval <- function(rf, null.dist) {
  
  calcPval <- function(obs, null) {
    t(sapply(1:nrow(null), function(i) {
      sapply(1:ncol(null), function(j) swfscMisc::pVal(obs[i, j], null[i, j, ]))
    }))
  }
  imp <- rf$importance
  impSD <- rf$importanceSD
  rm(rf)
  arr <- .makeImpArray(calcPval(imp, null.dist$unscaled), 2, NULL)
  arr[, , 2] <- calcPval(.scaleImp(imp, impSD), null.dist$scaled)
  dimnames(arr) <- list(rownames(imp), colnames(imp), c("unscaled", "scaled"))
  return(arr)
}

# Simple confusion matrix
.confMat <- function(rf) {
  if(!inherits(rf, "randomForest")) {
    stop("'rf' must be a randomForest object or inherit from one")
  }
  cbind(table(rf$y, rf$predicted))
}

.permFunc <- function(y, call.x, perm.rf.call) {
  x <- call.x # required to avoid clash with clusterApply formal argument x (called from parLapply)
  perm.rf.call$y <- y
  perm.rf <- eval(perm.rf.call)
  imp <- perm.rf$importance
  impSD <- perm.rf$importanceSD
  rm(perm.rf)
  imp.arr <- .makeImpArray(imp, 2, c("unscaled", "scaled"))
  imp.arr[, , 2] <- .scaleImp(imp, impSD)
  return(imp.arr)
}