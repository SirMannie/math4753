#' Plot a Normal Distribution Curve and Return Parameters
#'
#' @param mu Mean of the normal distribution.
#' @param sigma Standard deviation of the normal distribution.
#' @return A list containing the mean (`mu`), standard deviation (`sigma`), and calculated probability.
#' @importFrom graphics curve
#' @importFrom stats dnorm
#' @examples
#' \dontrun{myncurve(mu = 0, sigma = 1)}
#' @export
myncurve = function(mu, sigma) {
  x <- seq(mu - 3 * sigma, mu + 3 * sigma, length.out = 100)  # Define a sequence for x
  y <- dnorm(x, mean = mu, sd = sigma)  # Calculate the density values for the normal distribution
  plot(x, y, type = 'l', main = "Normal Distribution", xlab = "X", ylab = "Density")  # Plot the curve
  list(mu = mu, sigma = sigma)  # Return the parameters
}






