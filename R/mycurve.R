#' Draw a Normal Curve and Shade Area
#'
#' Plots a normal distribution with mean mu and sd sigma, shading the area
#' from -infinity to x=a, and returning the probability P(X <= a).
#'
#' @param a The x-value up to which the area is shaded.
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @return A list with mu, sigma, and the computed probability.
#' @importFrom graphics abline axis points segments
#' @importFrom stats quantile
#' @export
myncurve <- function(a, mu, sigma) {
  x <- seq(mu - 3 * sigma, mu + 3 * sigma, length = 1000)
  y <- dnorm(x, mean = mu, sd = sigma)

  plot(x, y, type="l", col="black", lwd=2,
       xlab="x", ylab="Density",
       main=paste("Normal Curve with mu =", mu, "and sigma =", sigma))

  x_shade <- seq(mu - 3 * sigma, a, length=500)
  y_shade <- dnorm(x_shade, mean=mu, sd=sigma)
  polygon(c(x_shade, a, mu - 3*sigma), c(y_shade, 0, 0),
          col="pink", border=NA)

  prob <- pnorm(a, mean=mu, sd=sigma)
  text(a, max(y)*0.2, paste("P(X <=", a, ") =", round(prob,4)), pos=4)

  return(list(mu=mu, sigma=sigma, probability=round(prob,4)))
}
