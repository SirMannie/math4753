#'@importFrom stats pbinom
#'@title probability for binomial distribution function
#'
#'@param q quantile
#'@param size number of trials
#'@param prob probability of success
#'
#'@return a pbinom value
#'@export
#'
#'@examples
#'\dontrun{pbinomf(q = 8, size = 15, prob = 0.4)}

pbinomf <- function(q, size, prob) {

  pbinom(q, size, prob)

}

