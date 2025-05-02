#' Maximum Likelihood Estimation (Univariate)
#'
#' Computes and plots the log-likelihood for a univariate parameter
#' using repeated sampling from the same distribution.
#'
#' @param lfun A log-likelihood function of the form \code{function(x, param)}.
#' @param x A vector of data values.
#' @param param A vector of parameter values to evaluate.
#' @param ... Additional plotting arguments.
#'
#' @return A list containing:
#' \describe{
#'   \item{i}{Index of the maximum likelihood estimate.}
#'   \item{parami}{The MLE parameter value.}
#'   \item{yi}{The log-likelihood at the MLE.}
#'   \item{slope}{The approximate slope around the MLE.}
#' }
#'
#' @examples
#' logbin <- function(x, param) log(dbinom(x, prob = param, size = 10))
#' mymaxlik(logbin, x = c(9, 9, 1, 9, 9, 9), param = seq(0, 1, length = 1000))
#'
#' @importFrom graphics abline axis points segments
#' @importFrom stats quantile
#' @export
mymaxlik <- function(lfun, x, param, ...) {
  np <- length(param)
  z <- outer(x, param, lfun)
  y <- apply(z, 2, sum)
  plot(param, y, col = "Blue", type = "l", lwd = 2, ...)
  i <- max(which(y == max(y)))
  abline(v = param[i], lwd = 2, col = "Red")
  points(param[i], y[i], pch = 19, cex = 1.5, col = "Black")
  axis(3, param[i], round(param[i], 2))
  slope <- if (i - 3 >= 1 & i + 2 <= np) {
    (y[(i - 2):(i + 2)] - y[(i - 3):(i + 1)]) / (param[(i - 2):(i + 2)] - param[(i - 3):(i + 1)])
  } else {
    "NA"
  }
  return(list(i = i, parami = param[i], yi = y[i], slope = slope))
}
