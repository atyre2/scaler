---
output: github_document
---

<!-- README.md is generated from README.Rmd. Please edit that file -->



## Introduction and Rationale

Centering and rescaling covariates is a common task prior to building
almost any sort of statistical model. Although function `scale()` will scale
scale and center numeric matrices, it always returns a `matrix`. Most model
fitting functions take data.frames. And although `scale()` will take a
data.frame as an input, it fails with an error if there is even one column
that is a categorical variable. The functions in this package provide typesafe
scaling and/or centering while ignoring
non-numeric columns. Like `scale()`, the constants used are stored as
attributes in the result, enabling automatic undoing of these operations.
This is handy for making plots after fitting the models.

My primary reason for putting this package together is to learn the process of 
making an R package. But secondarily, the scaling and unscaling of covariates
is a common source of errors among students in NRES 803 Ecological Statistics. 
I hope that these functions will reduce those errors and make everyone's lives
easier!

I aim for these functions to be fast and to play well with pipes. 

## Installation

At the moment, the only way to install this package is from the github
repository:


```r
install.packages("devtools")
devtools::install_github("atyre2/scaler")
```

