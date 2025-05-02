#' Calculate optimal number of tickets to sell for overbooking
#'
#' Computes the number of tickets to sell using both a binomial model (discrete)
#' and a normal approximation (continuous) such that the probability of overbooking
#' does not exceed a specified threshold.
#'
#' @param N Integer. Number of seats on the plane.
#' @param gamma Numeric. Maximum acceptable probability of overbooking.
#' @param p Numeric. Probability that a passenger shows up.
#'
#' @return A named list containing \code{nd}, \code{nc}, \code{N}, \code{p}, and \code{gamma}.
#' @examples
#' ntickets(N = 400, gamma = 0.02, p = 0.95)
#' @export
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot curve hist lines polygon text
#' @importFrom stats density dnorm dunif pnorm qbinom runif

ntickets <- function(N, gamma, p) {
  # --- Discrete: Binomial ---
  n_range <- (N - 10):(N + 30)
  q_vals <- qbinom(1 - gamma, n_range, p)
  nd_index <- which(q_vals >= N)[1]
  nd <- n_range[nd_index]

  # --- Continuous: Normal Approximation ---
  cdf_vals <- pnorm(N + 0.5, mean = n_range * p, sd = sqrt(n_range * p * (1 - p)))
  f_continuous <- 1 - gamma - cdf_vals
  nc_index <- which((n_range >= N) & (f_continuous >= 0))[1]
  nc <- n_range[nc_index]

  result <- list(nd = nd, nc = nc, N = N, p = p, gamma = gamma)
  print(result)
  return(invisible(result))
}
