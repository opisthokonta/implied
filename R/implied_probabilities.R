

#' Implied probabilities from bookmaker odds.
#'
#' This function calculate the implied probabilities from bookmaker odds in decimal format, while
#' accounting for over-round in the odds.
#'
#' The method 'basic' is the simplest method, and computes the implied probabilities by
#' dividing the inverted odds by the sum of the inverted odds.
#'
#' The methods 'wpo' (Weights Proportional to the Odds), 'or' (Odds Ratio) and 'power' are form the Wisdom of the Crowds document (the updated version) by
#' Joseph Buchdahl. The method 'or' is originally by Cheung (2015), and the method 'power' is there referred
#' to as the logarithmic method.
#'
#' The method 'shin' uses the method by Shin (1992, 1993). This model assumes that there is a fraction of
#' insider trading, and that the bookmakers tries to maximize their profits. In addition to providing
#' implied probabilities, the method also gives an estimate of the proportion if inside trade, denoted z. Two algorithms
#' are implemented for finding the probabilities and z. Which algorithm to use is chosen via the shin_method argument.
#' The default method (shin_method = 'js') is based on the algorithm in Jullien & Salanié (1994). The 'uniroot'
#' method uses R's built in equation solver to find the probabilities. The uniroot approach is also used for the
#' 'pwr' and 'or' methods. The two methods might give slightly different answers, especially when the bookmaker margin
#' (and z) is small.
#'
#' The 'bb' (short for "balanced books") method is from Fingleton & Waldron (1999), and is a variant of Shin's method. It too assume
#' a fraction of insiders, but instead of assuming that the bookmakers maximize their profits, they
#' minimize their risk.
#'
#' Both the 'shin' and 'bb' methods can be used together with the 'grossmargin' argument. This is also
#' from the Fingleton & Waldron (1999) paper, and adds some further assumption to the calculations,
#' related to operating costs. grossmargin should be 0 (default) or greater, typical range is 0 to 0.05.
#' For values other than 0, this might sometimes cause some probabilities to not be identifiable. A warning
#' will be given if this happens.
#'
#' The method 'jsd' was developed by Christopher D. Long, and described in a series of Twitter postings
#' and a python implementation posted on GitHub.
#'
#' Methods 'shin', 'or', 'power', and 'jsd' use the uniroot solver to find the correct probabilities. Sometimes it will fail
#' to find a solution, but it can be made to work by tuning some setting. The uniroot_options argument accepts a list with
#' options that are passed on to the uniroot function. Currently the interval, maxit, tol and extendInt argument of
#' uniroot can be changed. See the Troubleshooting vignette for more details.
#'
#'
#' @param odds A matrix or numeric of bookmaker odds. The odds must be in the decimal format.
#' @param method A string giving the method to use. Valid methods are 'basic', 'shin', 'bb',
#' 'wpo', 'or', 'power', 'additive', and 'jsd'.
#' @param normalize Logical. Some of the methods will give small rounding errors. If TRUE (default)
#' a final normalization is applied to make absolutely sure the
#' probabilities sum to 1.
#' @param target_probability Numeric. The value the probabilities should sum to. Default is 1.
#' @param grossmargin Numeric. Must be 0 or greater. See the details.
#' @param shin_method Character. Either 'js' (default) or 'uniroot'. See the details.
#' @param uniroot_options list. Option passed on to the uniroot solver, for those methods where it is applicable. See 'details'.
#'
#'
#' @return A named list. The first component is named 'probabilities' and contain a matrix of
#' implied probabilities. The second is the bookmaker margins (aka the overround). The third
#' depends on the method used to compute the probabilities:
#' \itemize{
#'  \item{ zvalues (method = 'shin' and method='bb'): The estimated amount of insider trade.}
#'  \item{ specific_margins (method = 'wpo'): Matrix of the margins applied to each outcome.}
#'  \item{ odds_ratios (method = 'or'): Numeric with the odds ratio that are used to convert true
#'  probabilities to bookmaker probabilities.}
#'  \item{ exponents (method = 'power'): The (inverse) exponents that are used to convert true
#'  probabilities to bookmaker probabilities.}
#'  \item{ distance (method = 'jsd'): The Jensen-Shannon distances that are used to convert true
#'  probabilities to bookmaker probabilities.}
#' }
#'
#' The fourth component 'problematic' is a logical vector called indicating if any probabilities has fallen
#' outside the 0-1 range, or if there were some other problem computing the probabilities.
#'
#'
#' @section References:
#' \itemize{
#'  \item{Hyun Song Shin (1992) Prices of State Contingent Claims with Insider Traders, and the Favourite-Longshot Bias }
#'  \item{Hyun Song Shin (1993) Measuring the Incidence of Insider Trading in a Market for State-Contingent Claims}
#'  \item{Bruno Jullien & Bernard Salanié (1994) Measuring the incidence of insider trading: A comment on Shin.}
#'  \item{John Fingleton & Patrick Waldron (1999) Optimal Determination of Bookmakers' Betting Odds: Theory and Tests.}
#'  \item{Joseph Buchdahl - USING THE WISDOM OF THE CROWD TO FIND VALUE IN A FOOTBALL MATCH BETTING MARKET (https://www.football-data.co.uk/wisdom_of_crowd_bets)}
#'  \item{Keith Cheung (2015) Fixed-odds betting and traditional odds (https://www.sportstradingnetwork.com/article/fixed-odds-betting-traditional-odds/)}
#' }
#'
#' @examples
#'# Two sets of odds for a three-outcome game.
#'my_odds <- rbind(c(4.20, 3.70, 1.95),
#'                 c(2.45, 3.70, 2.90))
#'
#'# Convert to probabilities using Shin's method.
#'converted_odds <- implied_probabilities(my_odds, method='shin')
#'
#'# Look at the probabilities
#'converted_odds$probabilities
#'
#' @export
implied_probabilities <- function(odds, method='basic', normalize=TRUE, target_probability = 1,
                                  grossmargin = 0, shin_method = 'js', uniroot_options = NULL){

  stopifnot(length(method) == 1,
            tolower(method) %in% c('basic', 'shin', 'bb', 'wpo', 'or', 'power', 'additive', 'jsd'),
            all(odds >= 1, na.rm=TRUE),
            length(target_probability) == 1,
            target_probability > 0,
            grossmargin >= 0,
            shin_method %in% c('js', 'uniroot'),
            length(shin_method) == 1,
            is.null(uniroot_options) | is.list(uniroot_options))


  if (method == 'shin' & shin_method == 'uniroot' & grossmargin != 0){
    shin_method <- 'js'
    message('shin_method uniroot does not work when grossmargin is not 0. Method js will be used.')
  }

  if (method == 'shin' & shin_method == 'js' & target_probability != 1){
    shin_method <- 'uniroot'
    grossmargin <- 0
    message('shin_method js does not work when target_probability is not 1. Method uniroot will be used with grossmargin = 0.')
  }


  if (!is.matrix(odds)){

    if ('data.frame' %in% class(odds)){
      odds <- as.matrix(odds)
    } else {
      odds <- matrix(odds, nrow=1,
                     dimnames = list(NULL, names(odds)))
    }
  }

  if (method %in% c('shin', 'or', 'power', 'jsd')){
    uniroot_opts <- default_uniroot_opts(method = method)

    if (is.list(uniroot_options)){
      uniroot_opts <- utils::modifyList(uniroot_opts, uniroot_options)
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
  out$margin <- inverted_odds_sum - target_probability

  # Missing values
  missing_idx <- apply(odds, MARGIN = 1,
                       FUN = function(x) any(is.na(x)))


  if (any(inverted_odds_sum[!missing_idx] < 1)){
    stop('Some inverse odds sum to less than 1.')
  }


  if (method == 'basic'){
    out$probabilities <- (target_probability * inverted_odds) / inverted_odds_sum

  } else if (method == 'shin'){

    zvalues <- numeric(n_odds) # The proportion of insider trading.
    probs <- matrix(nrow=n_odds, ncol=n_outcomes)

    problematic_shin <- logical(n_odds)

    if (shin_method == 'js'){
      for (ii in 1:n_odds){

        # Skip rows with missing values.
        if (missing_idx[ii] == TRUE){
          next
        }

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
      }
    } else if (shin_method == 'uniroot'){
      for (ii in 1:n_odds){

        # Skip rows with missing values.
        if (missing_idx[ii] == TRUE){
          next
        }


        res <- stats::uniroot(f=shin_solvefor, io=inverted_odds[ii,], trgtprob = target_probability,
                              interval = uniroot_opts$interval, extendInt = uniroot_opts$extendInt,
                              tol = uniroot_opts$tol, maxiter = uniroot_opts$maxiter)

        zvalues[ii] <- res$root
        probs[ii,] <- shin_func(zz=res$root, io = inverted_odds[ii,])

      }
    }

    out$probabilities <- probs
    out$zvalues <- zvalues

    if (any(problematic_shin[!missing_idx])){
      warning(sprintf('Could not find z: Did not converge in %d instances. Some results may be unreliable. See the "problematic" vector in the output.',
                      sum(problematic_shin)))
    }

  } else if (method == 'bb'){

    # zz <- (((1-grossmargin) * inverted_odds_sum) - 1) / (n_outcomes-1)
    zz <- (((1-grossmargin) * inverted_odds_sum) - target_probability) / (n_outcomes-target_probability)
    probs <- (((1-grossmargin) * inverted_odds) - zz) / (1-zz)

    out$probabilities <- probs
    out$zvalues <- zz


  } else if (method == 'wpo'){
    # Margin Weights Proportional to the Odds.
    # Method from the Wisdom of the Crowds pdf.
    fair_odds <- (n_outcomes * odds) / (n_outcomes - (out$margin * odds))
    out$probabilities <- 1 / fair_odds
    out$specific_margins <- (out$margin * fair_odds) / n_outcomes
  } else if (method == 'or'){

    odds_ratios <- numeric(n_odds)
    probs <- matrix(nrow=n_odds, ncol=n_outcomes)

    for (ii in 1:n_odds){

      # Skip rows with missing values.
      if (missing_idx[ii] == TRUE){
        next
      }

      res <- stats::uniroot(f=or_solvefor, io=inverted_odds[ii,], trgtprob = target_probability,
                            interval = uniroot_opts$interval, extendInt = uniroot_opts$extendInt,
                            tol = uniroot_opts$tol, maxiter = uniroot_opts$maxiter)
      odds_ratios[ii] <- res$root
      probs[ii,] <- or_func(cc=res$root, io = inverted_odds[ii,])
    }

    out$probabilities <- probs
    out$odds_ratios <- odds_ratios

  } else if (method == 'power'){

    probs <- matrix(nrow=n_odds, ncol=n_outcomes)
    exponents <- numeric(n_odds)

    for (ii in 1:n_odds){

      # Skip rows with missing values.
      if (missing_idx[ii] == TRUE){
        next
      }

      res <- stats::uniroot(f=pwr_solvefor, io=inverted_odds[ii,], trgtprob = target_probability,
                            interval = uniroot_opts$interval, extendInt = uniroot_opts$extendInt,
                            tol = uniroot_opts$tol, maxiter = uniroot_opts$maxiter)
      exponents[ii] <- res$root
      probs[ii,] <- pwr_func(nn=res$root, io = inverted_odds[ii,])
    }

    out$probabilities <- probs
    out$exponents <- exponents

  } else if (method == 'additive'){

    probs <- matrix(nrow=n_odds, ncol=n_outcomes)

    for (ii in 1:n_odds){

      # Skip rows with missing values.
      if (missing_idx[ii] == TRUE){
        next
      }

      probs[ii,] <- inverted_odds[ii,] - ((inverted_odds_sum[ii] - target_probability) / n_outcomes)
    }

    out$probabilities <- probs

  } else if (method == 'jsd'){

    probs <- matrix(nrow=n_odds, ncol=n_outcomes)
    jsds <- numeric(n_odds)

    for (ii in 1:n_odds){
      # Skip rows with missing values.
      if (missing_idx[ii] == TRUE){
        next
      }

      # 0.1 seems to be a reasonable upper bound.
      res <- stats::uniroot(f=jsd_solvefor, io=inverted_odds[ii,], trgtprob = target_probability,
                            interval = uniroot_opts$interval, extendInt = uniroot_opts$extendInt,
                            tol = uniroot_opts$tol, maxiter = uniroot_opts$maxiter)

      jsds[ii] <- res$root
      probs[ii,] <- jsd_func(jsd=res$root, io = inverted_odds[ii,])
    }

    out$probabilities <- probs
    out$distance <- jsds


  }

  ## do a final normalization to make sure the probabilities
  ## sum to 1 without rounding errors.
  if (normalize){
    out$probabilities <- (target_probability * out$probabilities) / rowSums(out$probabilities)
  }

  # Make sure the matrix of implied probabilities has column names.
  if (!is.null(colnames(odds))){
    colnames(out$probabilities) <- colnames(odds)
  }

  # check if there are any probabilities outside the 0-1 range.
  problematic <- apply(out$probabilities, MARGIN = 1, FUN=function(x){any(x > 1 | x < 0)})
  problematic[is.na(problematic)] <- TRUE
  problematic[missing_idx] <- NA

  if (any(problematic, na.rm=TRUE)){
    warning(sprintf('Probabilities outside the 0-1 range produced at %d instances.\n',
                    sum(problematic)))
  }

  if (method == 'shin'){
    problematic <- problematic | problematic_shin
  }

  if (method %in% c('shin', 'bb')){
    negative_z <- out$zvalues < 0
    if (any(negative_z[!missing_idx])){
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


default_uniroot_opts <- function(method){
  opts <- list(extendInt = 'yes',
               maxiter = 1000,
               tol = .Machine$double.eps^0.25)

  if (method == 'shin'){
    opts$interval <- c(0, 0.4)
  } else if (method == 'or'){
    opts$interval <- c(0.95, 5)
  } else if (method == 'power'){
    opts$interval <- c(0.0001, 1)
  } else if (method == 'jsd'){
    opts$interval <- c(0.0000001, 0.1)
    opts$tol = 0.000001
  }

  return(opts)

}


# Calculate the probabilities using Shin's formula, for a given value of z.
# io = inverted odds.
shin_func <- function(zz, io){
  bb <- sum(io)
  (sqrt(zz^2 + 4*(1 - zz) * (((io)^2)/bb)) - zz) / (2*(1-zz))
}


# Calculate the probabilities using the odds ratio method,
# for a given value of the odds ratio cc.
# io = inverted odds.
or_func <- function(cc, io){
  io / (cc + io - (cc*io))
}


# the condition that the sum of the probabilites must sum to 1.
# Used with uniroot.
shin_solvefor <- function(zz, io, trgtprob){
  tmp <- shin_func(zz, io)
  sum(tmp) - trgtprob # 0 when the condition is satisfied.
}

# The condition that the sum of the probabilites must sum to 1.
# This function calulates the true probability, given bookmaker
# probabilites xx, and the odds ratio cc.
or_solvefor <- function(cc, io, trgtprob){
  tmp <- or_func(cc, io)
  sum(tmp) - trgtprob
}

# power function.
pwr_func <- function(nn, io){
  io^(1/nn)
}

# The condition that the sum of the probabilites must sum to 1.
# This function calulates the true probability, given bookmaker
# probabilites xx, and the inverse exponent. nn.
pwr_solvefor <- function(nn, io, trgtprob){
  tmp <- pwr_func(nn, io)
  sum(tmp) - trgtprob
}

# Simple discrete KL-divergence.
kld <- function(x, y){
  sum(x * log(x/y))
}

# The binomial symmetric Jensen–Shannon distance
# assuming p and io have length 1.
binom_jsd <- function(p, io){

  pvec <- c(p, 1-p)
  iovec <- c(io, 1-io)

  mm <- (pvec + iovec) / 2
  sqrt((kld(pvec, mm)/2) + (kld(iovec, mm)/2))

}

# Find the probabilties for a given JS distance and inverted odds.
jsd_func <- function(jsd, io){

  # The function to be used by uniroot to find p from kl and io.
  tosolve <- function(p, io, jsd){
    binom_jsd(p=p, io = io) - jsd
  }

  pp <- numeric(length(io))
  for (ii in 1:length(io)){
    # Intervall from approx 0 to io, implying
    # That the underlying probability i less than the
    # inverse odds.
    pp[ii] <- stats::uniroot(f = tosolve,
                      interval = c(0.00001, io[ii]),
                      io = io[ii], jsd = jsd)$root
  }
  return(pp)
}

# Calculate the probabilities using the Jensen-Shannon distance method,
# for a given value of the odds ratio cc.
# io = inverted odds.
jsd_solvefor <- function(jsd, io, trgtprob){
  sum(jsd_func(jsd=jsd, io = io)) - trgtprob
}


