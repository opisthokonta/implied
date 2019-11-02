#' Handles the calculations in implied_probabilities
#'
#' @param odds Odds in a preprocessed format.
#' @param method The method, already preprocessed from `implied_probabilities`.
#' @return An `out` list for use `implied_probablities`.

implied_handler <- function(odds, method = "method") {

  # Prepare the list that will be returned.
  out <- vector(mode = "list", length = 2)
  names(out) <- c("probabilities", "margin")

  # Some useful quantities
  n_odds <- nrow(odds)
  n_outcomes <- ncol(odds)

  # Inverted odds and margins
  inverted_odds <- 1 / odds
  inverted_odds_sum <- rowSums(inverted_odds)
  out$margin <- inverted_odds_sum - 1

  if (method == "basic") {
    out$probabilities <- inverted_odds / inverted_odds_sum

  } else if (method == "shin") {

    zvalues <- numeric(n_odds) # The proportion of insider trading.
    probs <- matrix(nrow = n_odds, ncol = n_outcomes)

    for (ii in 1:n_odds) {

      res <- stats::uniroot(f = shin_solvefor, interval = c(0, 0.4),
                            io = inverted_odds[ii, ])
      zvalues[ii] <- res$root
      probs[ii, ] <- shin_func(zz = res$root, io = inverted_odds[ii, ])

    }

    out$probabilities <- probs
    out$zvalues <- zvalues

  } else if (method == "wpo") {
    # Margin Weights Proportional to the Odds.
    # Method from the Wisdom of the Crowds pdf.
    fair_odds <- (n_outcomes * odds) / (n_outcomes - (out$margin * odds))
    out$probabilities <- 1 / fair_odds
    out$specific_margins <- (out$margin * fair_odds) / n_outcomes

  } else if (method == "or") {

    odds_ratios <- numeric(n_odds)
    probs <- matrix(nrow = n_odds, ncol = n_outcomes)

    for (ii in 1:n_odds) {
      res <- stats::uniroot(f = or_solvefor, interval = c(0.05, 5),
                            io = inverted_odds[ii, ])
      odds_ratios[ii] <- res$root
      probs[ii, ] <- or_func(cc = res$root, io = inverted_odds[ii, ])
    }

    out$probabilities <- probs
    out$odds_ratios <- odds_ratios

  } else if (method == "power") {

    probs <- matrix(nrow = n_odds, ncol = n_outcomes)
    exponents <- numeric(n_odds)

    for (ii in 1:n_odds) {
      res <- stats::uniroot(f = pwr_solvefor, interval = c(0.001, 1),
                            io  = inverted_odds[ii, ])
      exponents[ii] <- res$root
      probs[ii, ] <- pwr_func(nn = res$root, io = inverted_odds[ii, ])
    }

    out$probabilities <- probs
    out$exponents <- exponents

  } else if (method == "additive") {

    probs <- matrix(nrow = n_odds, ncol = n_outcomes)

    for (ii in 1:n_odds) {
      probs[ii, ] <- inverted_odds[ii, ] - ((inverted_odds_sum[ii] - 1)
                                            / n_outcomes)
    }

    out$probabilities <- probs

  }

  out

}



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
