#' Implied probabilities from bookmaker odds
#'
#' This function calculate the implied probabilities from bookmaker odds in
#' decimal format, using five different methods to account for overround in the
#' odds.
#'
#' Method "\code{basic}" is the simplest method, and computes the implied
#' probabilities by dividing the inverted odds by the sum of the inverted odds.
#'
#' Method "\code{shin}" uses the method by Shin (1991). This model assumes that
#' there is a fraction of insider trading, and that the bookmakers tries to
#' maximize their profits. In addition to providing implied probabilities,
#' the method also gives an estimate of the proportion if inside trade.
#' The method implemented here is based on the description in Štrumbelj (2014).
#'
#' Methods "\code{wpo}", "\code{or}" and "\code{power}" are from the Wisdom of
#' the Crowds document (the updated version) by Joseph Buchdahl. The method
#' "\code{or}" is originally by Cheung (2015), and the method "\code{power}"
#' is there referred to as the logarithmic method.
#'
#' @param odds Data frame, matrix or numeric vector of bookmaker odds. The odds
#'    must be in the decimal format and be greater than or equal to 1.
#' @param method 	The method to be used. See ‘Details’. Can be abbreviated.
#' @param normalize Logical. Some of the methods will give small rounding
#'    errors. If \code{TRUE}, a final normalization is applied to make the
#'    probabilities sum to 1.
#'
#' @return A named list. The first component is named "\code{probabilities}"
#' and contain a matrix of implied probabilities. The second in the
#' bookmaker margins (aka the overround). The third depends on the method
#' used to compute the probabilities:
#'
#' \itemize{
#'  \item{\code{zvalues} (\code{method} = "\code{shin}"): The estimated amount
#'      of insider trade.}
#'  \item{\code{specific_margins} (\code{method = "\code{wpo}"): Matrix of the
#'      margins applied to each outcome.}}
#'  \item{\code{odds_ratios} (\code{method} = "\code{or}"): Numeric with the
#'      odds ratio that is used to convert true probabilities to bookmaker
#'      probabilities.}
#'  \item{\code{exponents} (\code{method} = "\code{power}"): The (inverse)
#'      exponents that is used to convert true probabilities to bookmaker
#'      probabilities.}
#' }
#'
#' The fourth component \code{problematic} is a logical vector called indicating
#' which, if any, probabilities has fallen outside the 0-1 range.
#'
#' @references
#'
#'    Shin, H. S. (1991). Optimal betting odds against insider traders.
#'      Economic Journal, 101(408), 1179-1185.
#'
#'    Štrumbelj, E. (2014). On determining probability forecasts from betting
#'      odds. International Journal of Forecasting, 30(4), 934-943.
#'
#'    Joseph Buchdahl - Using the wisdom of the crowd to find value in a
#'      football match betting market
#'      \url{http://www.football-data.co.uk/wisdom_of_crowd_bets}
#'
#'    Keith Cheung (2015) Fixed-odds betting and traditional odds
#'      \url{www.sportstradingnetwork.com/article/fixed-odds-
#'      betting-traditional-odds/}
#'
#' @export

implied_probabilities <- function(odds, method = c("basic", "shin", "wpo", "or",
                                                   "power", "additive"),
                                  normalize = TRUE) {

  ## Pre-processsing.

  method <- match.arg(method)
  stopifnot(all(odds >= 1))

  if (!is.matrix(odds)) {

    if ("data.frame" %in% class(odds)) {
      odds <- as.matrix(odds)
    } else {
      odds <- matrix(odds, nrow = 1,
                     dimnames = list(NULL, names(odds)))
    }

  }

  ## Calculate the probabilities.

  out <- implied_handler(odds, method = method)

  ## Post-processing stage.

  # Do a final normalization to make sure the probabilities sum to 1
  if (normalize) {
    out$probabilities <- out$probabilities / rowSums(out$probabilities)
  }

  # Make sure the matrix of implied probabilities has column names.
  if (!is.null(colnames(odds))) colnames(out$probabilities) <- colnames(odds)

  # Check if there are any probabilities outside the 0-1 range.
  problematic <- apply(out$probabilities,
                       MARGIN = 1,
                       FUN = function(x) any(x > 1 | x < 0))

  if (any(problematic)) {
    msg <- "Probabilities outside the 0-1 range produced at %d instances.\n"
    warning(sprintf(msg, sum(problematic)))
  }

  out$problematic <- problematic

  return(out)
}
