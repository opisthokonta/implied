

#' Implied probabilities from bookmaker odds.
#'
#' This function calculate the implied probabilties from bookmaker odds in decimal format, using five
#' different methods to account for overround in the odds.
#'
#' The method 'basic' is the simplest method, and computes the implied probabilities by
#' dividing the inverted odds by the sum of the inverted odds.
#'
#' The method 'shin' uses the method by Shin (1991). This model assumes that there is a fraction of
#' insider trading, and that the bookmakers tries to maximize their profits. In addition to providing
#' implied probabiltiies, the method also gives an estimate of the proportion if inside trade. The method
#' implemented here is based on the description in Štrumbelj (2014).
#'
#' The methods 'wpo', 'or' and 'power' are form the Wisdom of the Crowds document (the updated version) by
#' Joseph Buchdahl. The method 'or' is origianlly by Cheung (2015), and the method 'power' is there referred
#' to as the logarithmic method.
#'
#'
#' @param odds A matrix or numeric of bookmaker odds. The odds must be in the decimal format.
#' @param method A string giving the method to use. Valid methods are 'basic', 'shin',
#' 'wpo', 'or' and 'power'.
#' @param normalize Logical. Some of the methods will give small rounding errors. If TRUE (default)
#' a final normalization is applied to make absoultely sure the
#' probabilities sum to 1.
#'
#'
#' @return A named list. The first component is named 'probabilities' and contain a matrix of
#' implied probabilities. The second in the bookmaker margins (aka the overround). The third
#' depends on the method used to compute the probabilities:
#'  * zvalues (method = 'shin'): The estimated amount of insider trade.
#'  * specific_margins (method = 'wpo'): Matrix of the margins applied to each outcome.
#'  * odds_ratios (method = 'or'): Numeric with the odds ratio that is used to convert true
#'  probabilities to bookamker probabilties.
#'  * exponents (method = 'power'): The (inverse) exponents that is used to convert true
#'  probabilities to bookamker probabilties.
#'
#'  @section References
#'  * Hyun Song Shin (1991) Optimal betting odds against insider traders.
#'  Erik Štrumbelj (2014) On determining probability forecasts from betting odds.
#'  * Joseph Buchdahl - USING THE WISDOM OF THE CROWD TO FIND VALUE IN A FOOTBALL MATCH BETTING MARKET (http://www.football-data.co.uk/wisdom_of_crowd_bets)
#'  * Keith Cheung (2015) Fixed-odds betting and traditional odds (www.sportstradingnetwork.com/article/fixed-odds-betting-traditional-odds/)
#'
#' @export
implied_probabilities <- function(odds, method='basic', normalize=TRUE){

  stopifnot(length(method) == 1,
            tolower(method) %in% c('basic', 'shin', 'wpo', 'or', 'power'),
            all(odds >= 1))

  if (!is.matrix(odds)){
    odds <- matrix(odds, nrow=1,
                   dimnames = list(NULL, names(odds)))
  }

  # Prepare the list that will be returned.
  out <- vector(mode='list', length=2)
  names(out) <- c('probabilities', 'margin')

  # Some useful quantities
  n_odds <- nrow(odds)
  n_outcomes <- ncol(odds)

  # Inverted odds and margins
  inverted_odds <- 1 / odds
  inverted_odds_sum <- rowSums(inverted_odds)
  out$margin <- inverted_odds_sum - 1

  if (method == 'basic'){
    out$probabilities <- inverted_odds / inverted_odds_sum

  } else if (method == 'shin'){

    zvalues <- numeric(n_odds) # The proportion of insider trading.
    probs <- matrix(nrow=n_odds, ncol=n_outcomes)

    for (ii in 1:n_odds){
      res <- stats::uniroot(f=shin_solvefor, interval = c(0,0.4), io=inverted_odds[ii,])
      zvalues[ii] <- res$root
      probs[ii,] <- shin_func(zz=res$root, io = inverted_odds[ii,])
    }

    out$probabilities <- probs
    out$zvalues <- zvalues

  } else if (method == 'wpo'){
    # Margin Weights Proportional to the Odds.
    # Method from the Wisdom of the Crowds pdf.
    fair_odds <- (n_outcomes * odds) / (n_outcomes - (out$margin * odds))
    out$probabilities <- 1 / fair_odds
    out$specific_margins = (out$margin * fair_odds) / n_outcomes
  } else if (method == 'or'){

    odds_ratios <- numeric(n_odds)
    probs <- matrix(nrow=n_odds, ncol=n_outcomes)

    for (ii in 1:n_odds){
      res <- stats::uniroot(f=or_solvefor, interval = c(0.05, 5), io=inverted_odds[ii,])
      odds_ratios[ii] <- res$root
      probs[ii,] <- or_func(cc=res$root, io = inverted_odds[ii,])
    }

    out$probabilities <- probs
    out$odds_ratios <- odds_ratios

  } else if (method == 'power'){

    probs <- matrix(nrow=n_odds, ncol=n_outcomes)
    exponents <- numeric(n_odds)

    for (ii in 1:n_odds){
      res <- stats::uniroot(f=pwr_solvefor, interval = c(0.001, 1), io=inverted_odds[ii,])
      exponents[ii] <- res$root
      probs[ii,] <- pwr_func(nn=res$root, io = inverted_odds[ii,])
    }

    out$probabilities <- probs
    out$exponents <- exponents

  }

  ## do a final normalization to make sure the probabilites
  ## sum to 1 without rounding errors.
  if (normalize){
    out$probabilities <- out$probabilities / rowSums(out$probabilities)
  }

  # Make sure the matrix of implied probabilities has column names.
  if (!is.null(colnames(odds))){
    colnames(out$probabilities) <- colnames(odds)
  }

  return(out)
}


#########################################################
# Internal functions used to transform probabilities
# and be used with uniroot.
#########################################################

# Calculate the probabilities usin Shin's formula, for a given value of z.
# io = inverted odds.
shin_func <- function(zz, io){
  bb <- sum(io)
  (sqrt(zz^2 + 4*(1 - zz) * (((io)^2)/bb)) - zz) / (2*(1-zz))
}

# the condition that the sum of the probabilites must sum to 1.
# Used with uniroot.
shin_solvefor <- function(zz, io){
  tmp <- shin_func(zz, io)
  1 - sum(tmp) # 0 when the condition is satisfied.
}

# Calculate the probabilities usin the odds ratio method,
# for a given value of the odds ratio cc.
# io = inverted odds.
or_func <- function(cc, io){
  io / (cc + io - (cc*io))
}

# The condition that the sum of the probabilites must sum to 1.
# This function calulates the true probability, given bookmaker
# probabilites xx, and the odds ratio cc.
or_solvefor <- function(cc, io){
  tmp <- or_func(cc, io)
  sum(tmp) - 1
}


pwr_func <- function(nn, io){
  io^(1/nn)
}

# The condition that the sum of the probabilites must sum to 1.
# This function calulates the true probability, given bookmaker
# probabilites xx, and the inverse exponent. nn.
pwr_solvefor <- function(nn, io){
  tmp <- pwr_func(nn, io)
  sum(tmp) - 1
}


