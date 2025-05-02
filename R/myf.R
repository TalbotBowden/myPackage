#' Compute Piecewise Regression Values
#'
#' This function calculates values for a piecewise linear regression model with a breakpoint at x = 18.
#'
#' @param x Numeric vector of predictor values.
#' @param coef Numeric vector of coefficients from a linear model.
#'
#' @return Numeric vector of fitted values.
#'
#' @examples
#' coef <- c(5, 1.5, -0.2)
#' myf(20, coef)
#'
#' @export
myf <- function(x, coef) {
  coef[1] + coef[2] * (x) + coef[3] * (x - 18) * (x > 18)
}
