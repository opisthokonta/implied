#' Calculate the probabilities using Shin's formula, for a given value of z.
#'
#' @param zz ?
#' @param io Inverted odds.
#' @return Probabilities using Shin's formula
#' @keywords internal

shin_func <- function(zz, io) {
  bb <- sum(io)
  (sqrt(zz^2 + 4 * (1 - zz) * ((io ^ 2) / bb)) - zz) / (2 * (1 - zz))
}

#' The condition that the sum of the probabilites must sum to 1. Used with
#' uniroot.
#'
#' @param zz ?
#' @param io Inverted odds.
#' @return `0` if the probabilities sum to 1, `1` otherwise.
#' @keywords internal

shin_solvefor <- function(zz, io) {
  tmp <- shin_func(zz, io)
  1 - sum(tmp) # 0 when the condition is satisfied.
}

#' Calculate the probabilities using the odds ratio method, for a given value
#' of the odds ratio cc.
#'
#' @param zz ?
#' @param io Inverted odds.
#' @return Probabilities using the odds ratio method.
#' @keywords internal

or_func <- function(cc, io) {
  io / (cc + io - (cc * io))
}

#' The condition that the sum of the probabilites must sum to 1. This function
#' calulates the true probability, given bookmaker probabilites xx, and the
#' odds ratio cc.
#'
#' @param cc ?
#' @param io Inverted odds.
#' @return True probabilities.
#' @keywords internal

or_solvefor <- function(cc, io) {
  tmp <- or_func(cc, io)
  sum(tmp) - 1
}

#' Power function
#'
#' @param nn Numeric vector.
#' @param io ?.
#' @return Numeric.
#' @keywords internal

pwr_func <- function(nn, io) io ^ (1 / nn)


#' The condition that the sum of the probabilites must sum to 1. This function
#' calulates the true probability, given bookmaker probabilites xx, and the
#' inverse exponent.
#'
#' @param nn Numeric vector.
#' @param io ?.
#' @return True probabilities.
#' @keywords internal

pwr_solvefor <- function(nn, io) sum(pwr_func(nn, io)) - 1
