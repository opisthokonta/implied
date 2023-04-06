


# The functions xx_func_o(coef, probs) transforms proper probabilities (that sum to 1)
# into improper probabilities as a function of the input coefficient.
# The corresponding functions xx_o_solvefor(coef, probs, margin) are used
# with uniroot to find the coefficient that makes the transformed probabilities
# sum to the desired margin.

# Transform the probabilities using the Shin's method,
# for a given value of the odds ratio cc.
shin_func_o <- function(zz, probs, grossmargin=NULL){

  # Eq. 5 in Shin 1993.
  yy <- sqrt((zz*probs) + ((1-zz)*probs^2))
  res <- yy * sum(yy)

  if (!is.null(grossmargin)){
    # Eq. 14 in in Fingleton & Waldron 1999
    res <- res / (1 - grossmargin)
  }

  return(res)
}

# the condition that the sum of the probabilites must sum to 1.
# Used with uniroot.
shin_o_solvefor <- function(zz, probs, margin, grossmargin=NULL){
  tmp <- shin_func_o(zz, probs, grossmargin)
  sum(tmp) - (1 + margin)
}


# Transform the probabilities using the odds ratio method,
# for a given value of the odds ratio cc.
or_func_o <- function(cc, probs){
  or_probs <- cc * probs
  or_probs / (1 - probs + or_probs)
}

# The condition that the sum of the transformed probabilites
# must sum to 1 + margin.
or_o_solvefor <- function(cc, probs, margin){
  tmp <- or_func_o(cc, probs)
  sum(tmp) - (1 + margin)
}


# Transform the probabilities using the power method.
pwr_func_o <- function(nn, probs){
  probs^(nn)
}

# The condition that the sum of the transformed probabilites
# must sum to 1 + margin.
pwr_o_solvefor <- function(nn, probs, margin){
  tmp <- pwr_func_o(nn, probs)
  sum(tmp) - (1 + margin)
}



#' Implied odds with added margin from probabilities.
#'
#' This functions converts probabilities to odds in decimal format, while adding overround.
#' The function does the inverse of what the function \code{\link{implied_probabilities}} does.
#'
#' @param probabilities A matrix or numeric of probabilities, where each column is an outcome.
#' @param method A string giving the method to use. Valid methods are 'basic', 'shin', 'bb', 'wpo', 'or', 'power' or 'additive'.
#' @param margin numeric. How large margin (aka overround) should be added to the probabilities.
#' @param grossmargin Numeric. Must be 0 or greater. See the details.
#' @param normalize Logical. If TRUE (default), scale the input probabilites to sum to 1.
#'
#' @return A named list. The first component is named 'odds' and contain a matrix of
#' implied odds. The second depends on the method used to compute the probabilities.
#'
#' @export
implied_odds <- function(probabilities, method = 'basic', margin = 0,
                         grossmargin = NULL, normalize=TRUE){

  stopifnot(length(method) == 1,
            length(margin) == 1,
            tolower(method) %in% c('basic', 'shin', 'bb', 'wpo', 'or', 'power', 'additive'),
            all(probabilities >= 0, na.rm=TRUE))



  if (!is.matrix(probabilities)){

    if ('data.frame' %in% class(probabilities)){
      probabilities <- as.matrix(probabilities)
    } else {
      probabilities <- matrix(probabilities, nrow=1,
                     dimnames = list(NULL, names(probabilities)))
    }
  }

  # Make sure the probabilities sum to exactly 1.
  if (normalize){
    probabilities  <- probabilities / rowSums(probabilities)
  }

  # Prepare the list that will be returned.
  out <- vector(mode='list', length=1)
  names(out) <- c('odds')

  # Some useful quantities
  n_probs <- nrow(probabilities)
  n_outcomes <- ncol(probabilities)

  # Missing values
  missing_idx <- apply(probabilities, MARGIN = 1,
                       FUN = function(x) any(is.na(x)))

  # inverted_probs <- 1 / probabilities

  if (method == 'basic'){

    out$odds <- 1 / (probabilities * (1 + margin))

  } else if (method == 'shin'){

    odds <- matrix(nrow=n_probs, ncol=n_outcomes)
    zz <- numeric(n_probs)

    for (ii in 1:n_probs){

      # Skip rows with missing values.
      if (missing_idx[ii] == TRUE){
        next
      }

      if (margin != 0){
        res <- stats::uniroot(f=shin_o_solvefor, interval =  c(0, 0.4),
                              probs=probabilities[ii,],
                              margin = margin, grossmargin = grossmargin)
        zz[ii] <- res$root
      } else {
        zz[ii] <- 0
      }

      odds[ii,] <- 1 / shin_func_o(zz=zz[ii], probs = probabilities[ii,], grossmargin = grossmargin)
    }

    out$odds <- odds
    out$zvalues <- zz

  } else if (method == 'bb'){

    if (is.null(grossmargin)){
      grossmargin <- 0
    } else {
      stopifnot(grossmargin >= 0,
                length(grossmargin) == 1)
    }

    zz <- (((1-grossmargin)*(1 + margin)) - 1) / (n_outcomes-1)
    out$odds <- 1 / ((1+margin) * (((probabilities*(1-zz)) + zz) / ((n_outcomes-1)*zz + 1)))

    out$zvalues <- zz

  } else if (method == 'wpo'){
    # Margin Weights Proportional to the Odds.
    # Method from the Wisdom of the Crowds pdf.
    invprob <- 1 / probabilities
    out$specific_margins <- (margin * invprob) / n_outcomes
    out$odds <- invprob / (1 + out$specific_margins)

  } else if (method == 'or'){

    odds <- matrix(nrow=n_probs, ncol=n_outcomes)
    odds_ratios <- numeric(n_probs)

    for (ii in 1:n_probs){

      # Skip rows with missing values.
      if (missing_idx[ii] == TRUE){
        next
      }

      if (margin != 0){
        res <- stats::uniroot(f=or_o_solvefor, interval = c(0.05, 5),
                              probs=probabilities[ii,], margin = margin)
        odds_ratios[ii] <- res$root
      } else {
        odds_ratios[ii] <- 1
      }

      odds[ii,] <- 1 / or_func_o(cc=odds_ratios[ii], probs = probabilities[ii,])
    }

    out$odds <- odds
    out$odds_ratios <- odds_ratios

  } else if (method == 'power'){

    odds <- matrix(nrow=n_probs, ncol=n_outcomes)
    exponents <- numeric(n_probs)

    for (ii in 1:n_probs){

      # Skip rows with missing values.
      if (missing_idx[ii] == TRUE){
        next
      }

      if (margin != 0){
        res <- stats::uniroot(f=pwr_o_solvefor, interval = c(0.0001, 1.1),
                              probs=probabilities[ii,], margin = margin)
        exponents[ii] <- res$root
      } else {
        exponents[ii] <- 1
      }

      odds[ii,] <- 1 / pwr_func_o(nn=exponents[ii], probs = probabilities[ii,])
    }

    out$odds <- odds
    out$exponents <- exponents

  } else if (method == 'additive'){

    odds <- matrix(nrow=n_probs, ncol=n_outcomes)

    for (ii in 1:n_probs){

      # Skip rows with missing values.
      if (missing_idx[ii] == TRUE){
        next
      }

      odds[ii,] <- 1 / (probabilities[ii,] + (margin / n_outcomes))
    }

    out$odds <- odds

  }

  # Make sure the matrix of implied probabilities has column names.
  if (!is.null(colnames(probabilities))){
    colnames(out$odds) <- colnames(probabilities)
  }


  return(out)


}
