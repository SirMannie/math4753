#' @title calculates the number of tickets to be sold
#'
#' @param N number of seats in the flight
#' @param gamma probability that the plane will truly be overbooked
#' @param p probability of a "show"
#'
#' @return the number of tickets to be sold
#' @export
#'
#' @examples
#' \dontrun{functionCall = ntickets(N = 400, gamma = 0.02, p = 0.95)}
#' @importFrom stats pbinom pnorm
#' @importFrom graphics plot abline

ntickets <- function(N, gamma, p) {

  # N = number of seats in the plane
  # gamma = the probability of the plane being overbooked
  # p = probability of the show

  # Define the range of ticket numbers to consider
  x = c(N:(N + 20))

  # Calculate the objective function for discrete case using binomial distribution
  y = (1 - gamma) - pbinom(N, x, p)

  # Find the number of tickets to sell for minimizing the objective
  numTickets1 = which.min(abs(y))
  numTickets1 = x[numTickets1]

  # Plot the discrete case
  plot(x, y, type = 'b', pch = 21, bg = 'blue', main = paste("Objective vs n to find optimal tickets sold\n", "(", numTickets1, ") Gamma = ", gamma, " N = ", N, " \nDiscrete (Binomial)"), ylab = "Objective", xlab = "n")
  abline (h = 0, v = numTickets1, lwd = 2, col = "red")


  # For the continuous approximation, convert axes
  convertXaxis = seq(N, N + 20, length = 10000)
  convertYaxis = (1 - gamma) - pnorm(N + 0.5, convertXaxis * p, sqrt(convertXaxis * p * 1- p))

  # Find the number of tickets to sell for minimizing the objective in continuous case
  numTickets2 = which.min(abs(convertYaxis))
  numTickets2 = convertXaxis[numTickets2]

  # Plot the continuous case
  plot(convertXaxis, convertYaxis, type = 'l', main = paste("Objective vs n to find optimal tickets sold\n", "(", numTickets2, ") Gamma = ", gamma, " N = ", N, "\nContinous (Normal)" ), ylab = "Objective", xlab = "n")
  abline (h = 0, v = numTickets1, lwd = 2, col = "blue")

  # Return a list of variables for review
  variableList = list(nDiscrete = numTickets1, nContinous = numTickets2, numSeats = N, Prob.OfShow = p, Gamma = gamma)

  print(variableList)
}

