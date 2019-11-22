

#' Implied probabilities from bookmaker odds.
#'
#' This function calculate the implied probabilties from bookmaker odds in decimal format, while
#' accounting for overround in the odds.
#'
#' The method 'basic' is the simplest method, and computes the implied probabilities by
#' dividing the inverted odds by the sum of the inverted odds.
#'
#' The method 'shin' uses the method by Shin (1992, 1993). This model assumes that there is a fraction of
#' insider trading, and that the bookmakers tries to maximize their profits. In addition to providing
#' implied probabilties, the method also gives an estimate of the proportion if inside trade. The method
#' implemented here is based on the algorithm in Jullien & Salanié (1994).
#'
#' The 'bb' (short for "balanced books") method is from Fingleton & Waldron (1999), and is a variant of Shin's method. It too assume
#' a fraction of insiders, but instead of assuming that the bookmakers maximize their profits, they
#' minimize their risk.
#'
#' Both the 'shin' and 'bb' methods can be used together with the 'grossmargin' argument. This is also
#' from the Fingleton & Waldron (1999) paper, and adds some further assumption to the calculations,
#' related to opperating costs. grossmargin should be 0 (default) or greater, typical range is 0 to 0.05.
#' For values other than 0, this might sometimes cause some probabilities to not be identifiable. A warning
#' will be given if this happens.
#'
#' The methods 'wpo' (Weights Proportional to the Odds), 'or' (Odds Ratio) and 'power' are form the Wisdom of the Crowds document (the updated version) by
#' Joseph Buchdahl. The method 'or' is origianlly by Cheung (2015), and the method 'power' is there referred
#' to as the logarithmic method.
#'
#'
#'
#' @param odds A matrix or numeric of bookmaker odds. The odds must be in the decimal format.
#' @param method A string giving the method to use. Valid methods are 'basic', 'shin', 'bb',
#' 'wpo', 'or', 'power' or 'additive'.
#' @param normalize Logical. Some of the methods will give small rounding errors. If TRUE (default)
#' a final normalization is applied to make absoultely sure the
#' probabilities sum to 1.
#' @param grossmargin Numeric. Must be 0 or greater. See the details.
#'
#'
#' @return A named list. The first component is named 'probabilities' and contain a matrix of
#' implied probabilities. The second in the bookmaker margins (aka the overround). The third
#' depends on the method used to compute the probabilities:
#' \itemize{
#'  \item{ zvalues (method = 'shin' and method='bb'): The estimated amount of insider trade.}
#'  \item{ specific_margins (method = 'wpo'): Matrix of the margins applied to each outcome.}
#'  \item{ odds_ratios (method = 'or'): Numeric with the odds ratio that is used to convert true
#'  probabilities to bookamker probabilties.}
#'  \item{ exponents (method = 'power'): The (inverse) exponents that is used to convert true
#'  probabilities to bookamker probabilties.}
#' }
#'
#' The fourth compnent 'problematic' is a logical vector called indicating if any probabilites has fallen
#' outside the 0-1 range, or if there were some other problem computing the probabilities.
#'
#'
#' @section References:
#' \itemize{
#'  \item{Hyun Song Shin (1992) Prices of State Contingent Claims with Insider Traders, and the Favourite-Longshot Bias }
#'  \item{Hyun Song Shin (1993) Measuring the Incidence of Insider Trading in a Market for State-Contingent Claims}
#'  \item{Bruno Jullien & Bernard Salanié (1994) Measuring the incidence of insider trading: A comment on Shin.}
#'  \item{John Fingleton & Patrick Waldron (1999) Optimal Determination of Bookmakers' Betting Odds: Theory and Tests.}
#'  \item{Joseph Buchdahl - USING THE WISDOM OF THE CROWD TO FIND VALUE IN A FOOTBALL MATCH BETTING MARKET (http://www.football-data.co.uk/wisdom_of_crowd_bets)}
#'  \item{Keith Cheung (2015) Fixed-odds betting and traditional odds (www.sportstradingnetwork.com/article/fixed-odds-betting-traditional-odds/)}
#' }
#' @export
implied_probabilities <- function(odds, method='basic', normalize=TRUE, grossmargin = 0){

  stopifnot(length(method) == 1,
            tolower(method) %in% c('basic', 'shin', 'bb', 'wpo', 'or', 'power', 'additive'),
            all(odds >= 1),
            grossmargin >= 0)

  if (!is.matrix(odds)){

    if ('data.frame' %in% class(odds)){
      odds <- as.matrix(odds)
    } else {
      odds <- matrix(odds, nrow=1,
                     dimnames = list(NULL, names(odds)))
    }
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

    problematic_shin <- logical(n_odds)
    for (ii in 1:n_odds){

      # initialize zz at 0
      zz_tmp <- 0

      for (jj in 1:1000){
        zz_prev <- zz_tmp

        if (grossmargin != 0){
          zz_tmp <- (sum(sqrt(zz_prev^2 + 4*(1 - zz_prev) * (((inverted_odds[ii,]^2 * (1-grossmargin)))/inverted_odds_sum[ii])))-2) / (n_outcomes - 2)
        } else {
          zz_tmp <- (sum(sqrt(zz_prev^2 + 4*(1 - zz_prev) * (((inverted_odds[ii,])^2)/inverted_odds_sum[ii])))-2) / (n_outcomes - 2)
        }

        if (abs(zz_tmp - zz_prev)  <= .Machine$double.eps^0.25){
          break
        } else if (jj >= 1000){
          problematic_shin[ii] <- TRUE
        }

        zvalues[ii] <- zz_tmp
        probs[ii,] <- shin_func(zz=zz_tmp, io = inverted_odds[ii,])
      }

      out$probabilities <- probs
      out$zvalues <- zvalues

    }

    if (any(problematic_shin)){
      warning(sprintf('Could not find z: Did not converge in %d instances. Some results may be unreliable. See the "problematic" vector in the output.',
                      sum(problematic_shin)))
    }

  } else if (method == 'bb'){

    zz <- (((1-grossmargin)*inverted_odds_sum) - 1) / (n_outcomes-1)
    probs <- (((1-grossmargin) * inverted_odds) - zz) / (1-zz)

    out$probabilities <- probs
    out$zvalues <- zz


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

  } else if (method == 'additive'){

    probs <- matrix(nrow=n_odds, ncol=n_outcomes)

    for (ii in 1:n_odds){
      probs[ii,] <- inverted_odds[ii,] - ((inverted_odds_sum[ii] - 1) / n_outcomes)
    }

    out$probabilities <- probs

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

  # check if there are any probabilites outside the 0-1 range.
  problematic <- apply(out$probabilities, MARGIN = 1, FUN=function(x){any(x > 1 | x < 0)})
  if (any(problematic)){
    warning(sprintf('Probabilities outside the 0-1 range produced at %d instances.\n',
                    sum(problematic)))
  }

  if (method == 'shin'){
    problematic <- problematic | problematic_shin
  }

  if (method %in% c('shin', 'bb')){
    negative_z <- out$zvalues < 0
    if (any(negative_z)){
      warning(sprintf('z estimated to be negative: Some results may be unreliable. See the "problematic" vector in the output.',
                      negative_z))
    }
  }

  out$problematic <- problematic


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

# power function.
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


