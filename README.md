<!-- README.md is generated from README.Rmd. Please edit that file -->
implied
=======

This package contain a single funcion, implied\_probabilities(), that convert bookmaker odds into proper probabiltiies. Several methods are available, with different assumptions regarding the underlying mechanism the bookmakers convert their probabilities into odds.

A naive conversion of bookmaker odds into probabilities has two main problems. The first is that the probabilities are not proper probabilities, since they sum to more than 1. The excess probability is called the bookmakers margin. The second problem is that the probabilities, even if the margin is removed, will be biased in several ways, usually because of what is called the [favorite-longshot bias](https://en.wikipedia.org/wiki/Favourite-longshot_bias). The methods in this package remove the bookmaker margin and some of them also adjust for favorite-longshot bias.

Installation
============

``` r
install.packages("devtools")
devtools::install_github("opisthokonta/implied")
```

The basic method
================

The default method used by the function implied\_probabilities() is called the basic method. This is the simplest and most common method for converting bookmaker odds into probabilties, and is obtained by dividing the naive probabilities (the inverted odds) by the sum of the inverted odds. If <i>p<sub>i</sub></i> is the true underlying probability for outcome <i>i</i>, and <i>r<sub>i</sub></i> is the cooresponding inverted odds, then the probabilities are computed as

<i>p<sub>i</sub></i> = <i>r<sub>i</sub></i> / sum(<i>r</i>)

This method tend to be the least accurate of the methods in this package. I have also seen this normalization method been referred to as the multiplicative method.

The implied\_probabilities() function return a list with the proper probabilities (as a matrix) and the bookmaker margins.

In the examples below are three sets of bookamker odds from three football matches.

``` r

library(implied)

# One column for each outcome, one row for each race or match.
my_odds <- rbind(c(4.20, 3.70, 1.95),
                 c(2.45, 3.70, 2.90),
                 c(2.05, 3.20, 3.80))
colnames(my_odds) <- c('Home', 'Draw', 'Away')

res1 <- implied_probabilities(my_odds)

res1$probabilities
#>           Home      Draw      Away
#> [1,] 0.2331556 0.2646631 0.5021813
#> [2,] 0.3988848 0.2641264 0.3369888
#> [3,] 0.4586948 0.2938514 0.2474538

res1$margin
#> [1] 0.02118602 0.02326112 0.06346277
```

Shin's method
=============

Shin's method is based on the assumption that there is a small proportion of bettors that actually knows the outcome (called inside traders), and the rest of the bettors reflect the otherwise "true" uncertainty about the outcome. We can't unfortunately know what the insiders know, but Shin's method gives an estimate of this proportion (denoted Z).

``` r
res2 <- implied_probabilities(my_odds, method = 'shin')

res2$probabilities
#>           Home      Draw      Away
#> [1,] 0.2315716 0.2635742 0.5048542
#> [2,] 0.4000221 0.2629271 0.3370508
#> [3,] 0.4645882 0.2919788 0.2434331

# The estimated proportion of inside traders.
res2$zvalues
#> [1] 0.01061048 0.01163552 0.03182353
```

Margin Weights Proportional to the Odds
=======================================

This method is from [Joseph Buchdahl's Wisom of the Crowds document](http://www.football-data.co.uk/wisdom_of_crowd_bets), and assumes that the margin applied by the bookmaker for each of the outcome is proprtional to the probabilitiy of the outcome. In other words, the excessive probabilties are unevenly applied in a way that is reflects the favorite-longshot bias.

The probabilities are calculated can be calculated from the bookamker odds <i>O</i> using the following formula

<i>p<sub>i</sub></i> = n \* O<sub>i</sub> / (n - M \* O<sub>i</sub>)

where n is the number of outcomes, and M is the bookmaker margin.

``` r
res3 <- implied_probabilities(my_odds, method = 'wpo')

res3$probabilities
#>           Home      Draw      Away
#> [1,] 0.2310332 0.2632083 0.5057585
#> [2,] 0.4004096 0.2625166 0.3370739
#> [3,] 0.4666506 0.2913457 0.2420036

# The margins applied to each outcome.
res3$specific_margins
#>            Home       Draw       Away
#> [1,] 0.03056706 0.02683049 0.01396320
#> [2,] 0.01936444 0.02953607 0.02300299
#> [3,] 0.04533211 0.07260878 0.08741297
```

The odds ratio method
=====================

The odds ratio method is also from the Wisdom of the Crowds document, but is originally from an [article by Keith Cheung](www.sportstradingnetwork.com/article/fixed-odds-betting-traditional-odds/). This method models the relationship between the proper probabilities and the improper bookmaker probabilties using the odds ratio (OR) function:

OR = <i>p<sub>i</sub></i> (1 - <i>r<sub>i</sub></i>) / <i>r<sub>i</sub></i> (1 - <i>p<sub>i</sub></i>)

This gives the probabilities

<i>p<sub>i</sub></i> = <i>r<sub>i</sub></i> / OR + <i>r<sub>i</sub></i> - (OR \* <i>r<sub>i</sub></i>)

where the odds ratio OR is selected so that sum(<i>p<sub>i</sub></i>) = 1.

``` r
res4 <- implied_probabilities(my_odds, method = 'or')

res4$probabilities
#>           Home      Draw      Away
#> [1,] 0.2320048 0.2636415 0.5043537
#> [2,] 0.3996912 0.2633869 0.3369219
#> [3,] 0.4634406 0.2919032 0.2446562

# The odds ratios converting the proper probablities to bookmaker probabilities.
res4$odds_ratios
#> [1] 1.034449 1.035805 1.102606
```

The power method
================

The power method models the bookamker probabilties as a power function of the proper probabilties. This method is also described in the Wisdom of the Crowds document, where it is referred to as the logarithmic method.

<i>p<sub>i</sub></i> = <i>r<sub>i</sub></i><sup>(1/n)</sup>

where <i>n</i> is selected so that sum(<i>p<sub>i</sub></i>) = 1.

``` r
res4 <- implied_probabilities(my_odds, method = 'power')

res4$probabilities
#>           Home      Draw      Away
#> [1,] 0.2311414 0.2630644 0.5057942
#> [2,] 0.4003156 0.2627189 0.3369655
#> [3,] 0.4667139 0.2908985 0.2423876

# The inverse exponents (n) used to convert the proper probablities to bookmaker probabilities.
res4$exponents
#> [1] 0.9797664 0.9788115 0.9419744
```

Other packages
==============

The only other R package I know of with related functionality is the [gambleR](https://github.com/DataWookie/gambleR) package.

Literature
==========

Here are some relevant references and links:

-   Hyun Song Shin (1991) Optimal betting odds against insider traders. [Link](https://www.jstor.org/stable/2234434)
-   Erik Štrumbelj (2014) On determining probability forecasts from betting odds. [Link](https://www.sciencedirect.com/science/article/pii/S0169207014000533)
-   Joseph Buchdahl - USING THE WISDOM OF THE CROWD TO FIND VALUE IN A FOOTBALL MATCH BETTING MARKET (<http://www.football-data.co.uk/wisdom_of_crowd_bets>)
-   Keith Cheung (2015) Fixed-odds betting and traditional odds (<http://www.sportstradingnetwork.com/article/fixed-odds-betting-traditional-odds/>)
