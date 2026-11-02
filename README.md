[![CRAN version](http://www.r-pkg.org/badges/version/rfPermute?color=red)](https://cran.r-project.org/package=rfPermute)
[![CRAN last day downloads](http://cranlogs.r-pkg.org/badges/last-day/rfPermute?color=red)](https://cran.r-project.org/package=rfPermute)
[![CRAN last week downloads](http://cranlogs.r-pkg.org/badges/last-week/rfPermute?color=red)](https://cran.r-project.org/package=rfPermute)
[![CRAN last month downloads](http://cranlogs.r-pkg.org/badges/rfPermute?color=red)](https://cran.r-project.org/package=rfPermute)
[![CRAN total downloads](http://cranlogs.r-pkg.org/badges/grand-total/rfPermute?color=red)](https://cran.r-project.org/package=rfPermute)  
[![R-CMD-check](https://github.com/EricArcher/rfPermute/workflows/R-CMD-check/badge.svg)](https://github.com/EricArcher/rfPermute/actions)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/EricArcher/rfPermute?branch=master&svg=true)](https://ci.appveyor.com/project/EricArcher/rfPermute)

# rfPermute [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.15085006.svg)](https://doi.org/10.5281/zenodo.15085006)

## Description
`rfPermute` estimates the significance of importance metrics for a Random Forest model by permuting the response variable. It will produce null distributions of importance metrics for each predictor variable and _p_-values of observed importances. The package also includes several summary and visualization functions for `randomForest` and `rfPermute` results. See `rfPermuteTutorial()` in the package for a guide on running, summarizing, and diagnosing `rfPermute` and `randomForest` models.

## Contact
* submit suggestions and bug-reports: <https://github.com/ericarcher/rfPermute/issues>
* send a pull request: <https://github.com/ericarcher/rfPermute/>
* e-mail: <eric.archer@noaa.gov>

## Installation
To install the stable version from CRAN:
```r
install.packages('rfPermute')
```

To install the latest version from GitHub:
```r
# make sure you have devtools installed
if (!require('devtools')) install.packages('devtools')

# install from GitHub
devtools::install_github('EricArcher/rfPermute')
```

## Current Functions

### Variable importance p-value estimation, summary, and visualization
* `rfPermute` Estimate Permutation p-values for Random Forest Importance Metrics  
* `importance` Extract rfPermute Importance Scores and p-values  
* `plotNull` Plot Random Forest Importance Null Distributions  
* `plotImpPreds` Distribution of Important Variables  

### Random Forest model summary
* `summary` Summarize rfPermute and randomForest models
* `confusionMatrix` Confusion Matrix  
* `casePredictions` Return predictions and votes for training cases 
* `pctCorrect` Percent Correctly Classified  

### Random Forest model visualization and diagnostics
* `plotInbag` Distribution of sample inbag rates 
* `plotPredictedProbs` Distribution of prediction assignment probabilities  
* `plotProximity` Plot Random Forest Proximity Scores   
* `plotTrace` Trace of cumulative error rates in forest  
* `plotVotes` Vote Distribution    
* `rfROC` Receiver Operator Curve
 
### Miscellaneous functions
* `combineRP` Combine rfPermute models  
* `balancedSampsize` Balanced Sample Size
* `cleanRFdata` Clean Random Forest Input Data  

## Changelog

### version 2.5.6 (on CRAN)

* switched aes_string() and aes_() for aes() in ggplot plots
* added rfROC()
* fixed sampsize bug in plotInbag() when replace = TRUE
* changed plotInbag() to plot one panel per class for classification models when sampsize is specified by class
* updated null distribution `randomForest()` calls to explicitly set `proximity`, `do.trace`, `keep.forest`, and `keep.inbag` arguments to `FALSE` to save memory space in each replicate.

### version 2.5.5

* move of package to SWFSC/rfPermute as main GitHub repository

### version 2.5.4

* fixed print.rfPermute output for regression models.  

### version 2.5.2

* fixed bug in plotImportance heatmap to now properly choose top rather than bottom `n` predictors.
* update package documentation for CRAN

### version 2.5.1

* added `pct.correct` argument to `plotTrace()`. Default is now to have y-axis as 1 - OOB error rate.

### version 2.5

__NOTE__: v2.5 is a large redevelopment of the package. The structure of rfPermute model objects has changed make them incompatible with previous versions. Also, the name and functionality of several functions has changed to make them more consistent with one another.
A tutorial (under construction) is available within the package as `rfPermuteTutorial()`.

### version 2.2 (on CRAN)

* moved value of OOB expected error rate to end of output vector in `exptdErrRate`
* changed default of `threshold` argument in `classConfInt` and `confusionMatrix` to `NULL`
* added new grouping and labelling options to proximityPlot()
* added binomial test for priors in `exptdErrRate` and `confusionMatrix`

### version 2.1.81

* Fixed bug in `pctCorrect`
* Added `casePredictions`
* Updated parallel code

### version 2.1.7

* Fixed bug in parallel processing code.  

### version 2.1.6

* Added `plotConfMat`, `plotOOBtimes`, `plotRFtrace`, and `plotInbag`, and `plotImpVarDist` visualizations.  
* Changed `confusionMatrix` so it will work when `randomForest` model doesn't have a `$confusion` element, like when model is result of `combine`-ing multiple models.   
* Improved efficiency and stability of parallel processing code. Changed default value of `num.cores` to `NULL`.  

### version 2.1.5

* Added `type` argument to `plotVotes` to choose between area and bar charts.
* Changed `plot.rfPermute` to `plotNull` to avoid clashes and maintain functionality of `randomForest::plot.randomForest`.
* Changed name of `proximity.plot` to `proximityPlot`,  `exptd.err.rate` to `exptdErrRate`, and `clean.rf.data` to `cleanRFdata` to make camelCase naming scheme more consistent in package.
* Changed `plotNull` from base graphics to *ggplot2*.
* Added `symb.metab` data set.

### version 2.1.1

* Added `n` argument to `impHeatmap`.
* Added functions: `classConfInt`, `confusionMatrix`, `plotVotes`, `pctCorrect`.

### version 2.0.1

* Fixed bug in `plot.rfPermute` that was reporting the p-value incorrectly at the top of the figure.
* Fixed multi-threading in `rfPermute` so it works on Windows too.
* Added `impHeatmap` function.
* Switched `proximity.plot` to use `ggplot2` graphics.

### version 2.0

* Fixed bug with calculation of p-values not respecting importance measure scaling (division by standard deviations). New format of output of `rfPemute` has separate `$null.dist` and `$pval` elements, each with results for unscaled and scaled importance mesures. See `?rfPermute` for more information.
* `rp.importance` and `plot.rfPermute` now take a `scale` argument to specify whether or not importance values should be scaled by standard deviations.
* If `nrep = 0` for `rfPermute`, a `randomForest` object is returned.

### version 1.9.3

* Fixed import declarations to avoid `grid` name clashes.
* Fixed logic error in `clean.rf.data` where fixed predictors were not removed.
* Fixed error in use of `main` argument in `plot.rp.importance`.

### version 1.9.2

* Added this NEWS.md
* Added README.md
* Added `num.cores` argument to `rfPermute` to take advantage of multi-threading 

### version 1.9.1

* Added internal keyword to `calc.imp.pval` to keep it from indexing
* Updated imports to match new CRAN policies
