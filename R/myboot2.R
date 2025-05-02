#' Graphical Bootstrap Confidence Interval
#'
#' @description
#' This function generates a bootstrap confidence interval for a statistic (e.g., mean, median)
#' and overlays the results on a histogram of bootstrap sample statistics. It also returns the
#' confidence interval invisibly.
#'
#'
#' @param iter Integer. Number of bootstrap samples to generate. Default is 10000.
#' @param x Numeric vector. The sample data to resample from.
#' @param fun Character or function. The summary statistic to apply to each bootstrap sample (e.g., "mean", "sd"). Default is "mean".
#' @param alpha Numeric. Significance level (e.g., 0.05 for a 95% confidence interval). Default is 0.05.
#' @param cx Numeric. Size of text annotations in the plot. Default is 1.5.
#' @param ... Additional arguments passed to the histogram plotting function \code{\link{hist}}.
#'
#' @return A list with the following components:
#' \describe{
#'   \item{ci}{Confidence interval (numeric vector of length 2)}
#'   \item{fun}{The statistic function used}
#'   \item{x}{The original sample}
#' }
#'
#' @examples
#' set.seed(123)
#' x <- rnorm(20, mean = 5, sd = 2)
#' myboot2(x = x, fun = "mean")
#'
#'
#' @importFrom graphics abline axis points segments
#' @importFrom stats quantile
#' @export
myboot2 <- function(iter = 10000, x, fun = "mean", alpha = 0.05, cx = 1.5, ...) {
  n <- length(x)
  y <- sample(x, n * iter, replace = TRUE)
  rs.mat <- matrix(y, nrow = n, ncol = iter, byrow = TRUE)
  xstat <- apply(rs.mat, 2, fun)
  ci <- quantile(xstat, c(alpha / 2, 1 - alpha / 2))
  para <- hist(xstat, freq = FALSE, las = 1,
               main = paste("Histogram of Bootstrap sample statistics\nalpha =",
                            alpha, " iter = ", iter, sep = ""),
               ...)
  mat <- matrix(x, nrow = length(x), ncol = 1)
  pte <- apply(mat, 2, fun)
  abline(v = pte, lwd = 3, col = "black")
  segments(ci[1], 0, ci[2], 0, lwd = 4)
  text(ci[1], 0, paste("(", round(ci[1], 2)), col = "red", cex = cx)
  text(ci[2], 0, paste(round(ci[2], 2), ")"), col = "red", cex = cx)
  text(pte, max(para$density) / 2, round(pte, 2), cex = cx)
  invisible(list(ci = ci, fun = fun, x = x))
}
