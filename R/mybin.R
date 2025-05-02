#' Binomial Simulation Function
#'
#' This function simulates a binomial experiment with a given number of trials,
#' probability of success, and number of iterations. It returns a bar plot of the
#' distribution of successes.
#'
#' @param iter Number of iterations (default: 100)
#' @param n Number of trials (default: 10)
#' @param p Probability of success (default: 0.5)
#' @return A bar plot of the binomial distribution and a table of proportions.
#' @export
#' @importFrom graphics barplot
#' @importFrom grDevices rainbow
#' @examples
#' mybin(iter = 1000, n = 10, p = 0.3)
mybin <- function(iter = 100, n = 10, p = 0.5) {
  # Create a matrix to hold the samples
  sam.mat <- matrix(NA, nrow = n, ncol = iter, byrow = TRUE)


  # Vector to store success counts
  succ <- numeric(iter)

  for (i in 1:iter) {
    # Generate samples
    sam.mat[, i] <- sample(c(1, 0), n, replace = TRUE, prob = c(p, 1 - p))
    # Count number of successes
    succ[i] <- sum(sam.mat[, i])
  }

  # Create frequency table
  succ.tab <- table(factor(succ, levels = 0:n))

  # Generate bar plot
  barplot(succ.tab / iter, col = rainbow(n + 1),
          main = "Binomial Simulation", xlab = "Number of Successes")

  return(succ.tab / iter)
}
