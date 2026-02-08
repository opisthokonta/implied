

# some example odds
my_odds <- rbind(c(4.20, 3.70, 1.95),
                 c(2.45, 3.70, 2.90),
                 c(2.05, 3.20, 3.80),
                 c(1.595, 3.990, 6.760),
                 c(1.19 ,7.0 , 14.0))

# Some odds desinged to be problematic with the additive method.
# It is also problematic with the wpo method.
my_odds2 <- t(matrix(1/c(0.870, 0.2, 0.1, 0.05, 0.02, 0.01)))


# tolerance for some tests
toll <- 0.00005


# Implied probabilities ----
context("Implied probabilities")


iprobs1_basic <- implied_probabilities(my_odds)
iprobs1_shin <- implied_probabilities(my_odds, method='shin')
iprobs1_shin2 <- implied_probabilities(my_odds, method='shin', grossmargin = 0.01)
iprobs1_shin3 <- implied_probabilities(my_odds, method='shin', shin_method = 'uniroot')
iprobs1_bb <- implied_probabilities(my_odds, method='bb')
iprobs1_bb2 <- implied_probabilities(my_odds, method='bb', grossmargin = 0.01)
iprobs1_wpo <- implied_probabilities(my_odds, method='wpo')
iprobs1_or <- implied_probabilities(my_odds, method='or')
iprobs1_power <- implied_probabilities(my_odds, method='power')
iprobs1_additive <- implied_probabilities(my_odds, method='additive')
iprobs1_jsd <- implied_probabilities(my_odds, method='jsd')
iprobs1_goto <- implied_probabilities(my_odds, method='goto')


# Shin method uniroot, with grossmargin != 0 should switch to shin_method = 'js'
# Make sure the output is the same.
iprobs1_shin4 <- implied_probabilities(my_odds, method='shin', shin_method = 'uniroot', grossmargin = 0.01)

iprobs2_basic <- implied_probabilities(my_odds2)
iprobs2_shin <- implied_probabilities(my_odds2, method='shin')
iprobs2_shin3 <- implied_probabilities(my_odds2, method='shin', shin_method = 'uniroot')
iprobs2_or <- implied_probabilities(my_odds2, method='or')
iprobs2_power <- implied_probabilities(my_odds2, method='power')


# The KL method does not work with my_odds2.
#iprobs2_jsd <- implied_probabilities(my_odds2, method='jsd')
#iprobs2_goto <- implied_probabilities(my_odds2, method='goto')




test_that("Output", {

  expect_silent(
    iprobs2_shin <- implied_probabilities(my_odds2, method='shin')
  )

  expect_silent(
    iprobs2_shin2 <- implied_probabilities(my_odds2, method='shin', grossmargin = 0.01)
  )

  expect_silent(
    iprobs2_basic <- implied_probabilities(my_odds2)
  )
  expect_silent(
    iprobs2_shin <- implied_probabilities(my_odds2, method='shin')
  )
  expect_silent(
    iprobs2_or <- implied_probabilities(my_odds2, method='or')
  )
  expect_silent(
    iprobs2_power <- implied_probabilities(my_odds2, method='power')
  )

  expect_warning(
    iprobs2_additive <- implied_probabilities(my_odds2, method='additive')
  )
  expect_warning(
    iprobs2_wpo <- implied_probabilities(my_odds2, method='wpo')
  )

  # New in version 0.3.1, should give error.
  expect_error(implied_probabilities(my_odds[,1:2]))

  expect_equal(class(iprobs1_basic), 'list')
  expect_equal(class(iprobs1_shin), 'list')
  expect_equal(class(iprobs1_shin2), 'list')
  expect_equal(class(iprobs1_shin3), 'list')
  expect_equal(class(iprobs1_bb), 'list')
  expect_equal(class(iprobs1_bb2), 'list')
  expect_equal(class(iprobs1_wpo), 'list')
  expect_equal(class(iprobs1_or), 'list')
  expect_equal(class(iprobs1_additive), 'list')
  expect_equal(class(iprobs1_jsd), 'list')
  expect_equal(class(iprobs1_goto), 'list')

  expect_equal(all(abs(rowSums(iprobs1_basic$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs1_shin$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs1_shin2$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs1_shin3$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs1_bb$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs1_bb2$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs1_wpo$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs1_or$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs1_power$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs1_additive$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs1_jsd$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs1_goto$probabilities) - 1) < toll), TRUE)

  expect_equal(all(iprobs1_basic$margin > 0), TRUE)
  expect_equal(all(iprobs1_shin$margin > 0), TRUE)
  expect_equal(all(iprobs1_shin2$margin > 0), TRUE)
  expect_equal(all(iprobs1_shin3$margin > 0), TRUE)
  expect_equal(all(iprobs1_bb$margin > 0), TRUE)
  expect_equal(all(iprobs1_bb2$margin > 0), TRUE)
  expect_equal(all(iprobs1_wpo$margin > 0), TRUE)
  expect_equal(all(iprobs1_or$margin > 0), TRUE)
  expect_equal(all(iprobs1_power$margin > 0), TRUE)
  expect_equal(all(iprobs1_additive$margin > 0), TRUE)
  expect_equal(all(iprobs1_jsd$margin > 0), TRUE)
  expect_equal(all(iprobs1_goto$margin > 0), TRUE)

  expect_equal(is.null(iprobs1_shin$zvalues), FALSE)
  expect_equal(is.null(iprobs1_shin2$zvalues), FALSE)
  expect_equal(is.null(iprobs1_shin3$zvalues), FALSE)
  expect_equal(is.null(iprobs1_bb$zvalues), FALSE)
  expect_equal(is.null(iprobs1_bb2$zvalues), FALSE)
  expect_equal(is.null(iprobs1_wpo$specific_margins), FALSE)
  expect_equal(is.null(iprobs1_or$odds_ratios), FALSE)
  expect_equal(is.null(iprobs1_power$exponents), FALSE)
  expect_equal(is.null(iprobs1_jsd$distance), FALSE)
  expect_equal(is.null(iprobs1_goto$zvalues), FALSE)


  expect_equal(class(iprobs2_basic), 'list')
  expect_equal(class(iprobs2_shin), 'list')
  expect_equal(class(iprobs2_shin2), 'list')
  expect_equal(class(iprobs2_shin3), 'list')
  expect_equal(class(iprobs2_wpo), 'list')
  expect_equal(class(iprobs2_or), 'list')
  expect_equal(class(iprobs2_additive), 'list')

  expect_equal(all(abs(rowSums(iprobs2_basic$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs2_shin$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs2_shin3$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs2_wpo$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs2_or$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs2_power$probabilities) - 1) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs2_additive$probabilities) - 1) < toll), TRUE)

  expect_equal(all(iprobs2_basic$margin > 0), TRUE)
  expect_equal(all(iprobs2_shin$margin > 0), TRUE)
  expect_equal(all(iprobs2_shin3$margin > 0), TRUE)
  expect_equal(all(iprobs2_wpo$margin > 0), TRUE)
  expect_equal(all(iprobs2_or$margin > 0), TRUE)
  expect_equal(all(iprobs2_power$margin > 0), TRUE)
  expect_equal(all(iprobs2_additive$margin > 0), TRUE)

  expect_equal(iprobs2_basic$problematic, FALSE)
  expect_equal(iprobs2_shin$problematic, FALSE)
  expect_equal(iprobs2_shin3$problematic, FALSE)
  expect_equal(iprobs2_wpo$problematic, TRUE)
  expect_equal(iprobs2_power$problematic, FALSE)
  expect_equal(iprobs2_additive$problematic, TRUE)

  expect_equal(is.null(iprobs2_shin$zvalues), FALSE)
  expect_equal(is.null(iprobs2_shin3$zvalues), FALSE)
  expect_equal(is.null(iprobs2_wpo$specific_margins), FALSE)
  expect_equal(is.null(iprobs2_or$odds_ratios), FALSE)
  expect_equal(is.null(iprobs2_power$exponents), FALSE)

  expect_true(all(iprobs1_shin4$probabilities == iprobs1_shin2$probabilities))
  expect_message(implied_probabilities(my_odds, method='shin', shin_method = 'uniroot', grossmargin = 0.01))

})


# Non-normalized results ----
context("Non-normalized results")

iprobs1_basic_nn <- implied_probabilities(my_odds, normalize = FALSE)
iprobs1_shin_nn <- implied_probabilities(my_odds, method='shin', normalize = FALSE)
iprobs1_shin2_nn <- implied_probabilities(my_odds, method='shin', grossmargin = 0.01, normalize = FALSE)
iprobs1_shin3_nn <- implied_probabilities(my_odds, method='shin', shin_method = 'uniroot', normalize = FALSE)
iprobs1_bb_nn <- implied_probabilities(my_odds, method='bb', normalize = FALSE)
iprobs1_bb2_nn <- implied_probabilities(my_odds, method='bb', grossmargin = 0.01, normalize = FALSE)
iprobs1_wpo_nn <- implied_probabilities(my_odds, method='wpo', normalize = FALSE)
iprobs1_or_nn <- implied_probabilities(my_odds, method='or', normalize = FALSE)
iprobs1_power_nn <- implied_probabilities(my_odds, method='power', normalize = FALSE)
iprobs1_additive_nn <- implied_probabilities(my_odds, method='additive', normalize = FALSE)
iprobs1_jsd_nn <- implied_probabilities(my_odds, method='jsd', normalize = FALSE)
iprobs1_goto_nn <- implied_probabilities(my_odds, method='goto', normalize = FALSE)

# They should all be reasonably close to 1.
test_that("Non-normalized results", {
  expect_true(all(abs((rowSums(iprobs1_basic_nn$probabilities) - 1)) < 0.01))
  expect_true(all(abs((rowSums(iprobs1_shin_nn$probabilities) - 1)) < 0.01))
  expect_true(all(abs((rowSums(iprobs1_shin2_nn$probabilities) - 1)) < 0.01))
  expect_true(all(abs((rowSums(iprobs1_shin3_nn$probabilities) - 1)) < 0.01))
  expect_true(all(abs((rowSums(iprobs1_bb_nn$probabilities) - 1)) < 0.01))
  expect_true(all(abs((rowSums(iprobs1_bb2_nn$probabilities) - 1)) < 0.01))
  expect_true(all(abs((rowSums(iprobs1_wpo_nn$probabilities) - 1)) < 0.01))
  expect_true(all(abs((rowSums(iprobs1_or_nn$probabilities) - 1)) < 0.01))
  expect_true(all(abs((rowSums(iprobs1_power_nn$probabilities) - 1)) < 0.01))
  expect_true(all(abs((rowSums(iprobs1_additive_nn$probabilities) - 1)) < 0.01))
  expect_true(all(abs((rowSums(iprobs1_jsd_nn$probabilities) - 1)) < 0.01))
  expect_true(all(abs((rowSums(iprobs1_goto_nn$probabilities) - 1)) < 0.01))


})


# Missing values ----
context("Missing values")

# some example odds, with missing value
my_odds_na <- rbind(c(4.20, 3.70, 1.95),
                    c(2.45, NA, 2.90),
                    c(2.05, 3.20, 3.80))

# Test with missing values

iprobs1na_basic <- implied_probabilities(my_odds_na)
iprobs1na_shin <- implied_probabilities(my_odds_na, method='shin')
iprobs1na_shin2 <- implied_probabilities(my_odds_na, method='shin', grossmargin = 0.01)
iprobs1na_shin3 <- implied_probabilities(my_odds_na, method='shin', shin_method = 'uniroot')
iprobs1na_bb <- implied_probabilities(my_odds_na, method='bb')
iprobs1na_bb2 <- implied_probabilities(my_odds_na, method='bb', grossmargin = 0.01)
iprobs1na_wpo <- implied_probabilities(my_odds_na, method='wpo')
iprobs1na_or <- implied_probabilities(my_odds_na, method='or')
iprobs1na_power <- implied_probabilities(my_odds_na, method='power')
iprobs1na_additive <- implied_probabilities(my_odds_na, method='additive')
iprobs1na_jsd <- implied_probabilities(my_odds_na, method='jsd')
iprobs1na_goto <- implied_probabilities(my_odds_na, method='goto')


test_that("missing values", {

  expect_true(all(is.na(iprobs1na_basic$probabilities[2,])))
  expect_true(is.na(iprobs1na_basic$problematic[2]))
  expect_true(is.na(iprobs1na_basic$margin[2]))
  expect_false(is.na(iprobs1na_basic$problematic[1]))
  expect_false(is.na(iprobs1na_basic$margin[1]))


  expect_true(all(is.na(iprobs1na_shin$probabilities[2,])))
  expect_true(is.na(iprobs1na_shin$problematic[2]))
  expect_false(is.na(iprobs1na_shin$problematic[1]))
  expect_false(is.na(iprobs1na_shin$margin[1]))

  expect_true(all(is.na(iprobs1na_shin2$probabilities[2,])))
  expect_true(is.na(iprobs1na_shin2$problematic[2]))
  expect_false(is.na(iprobs1na_shin2$problematic[1]))
  expect_false(is.na(iprobs1na_shin2$margin[1]))

  expect_true(all(is.na(iprobs1na_shin3$probabilities[2,])))
  expect_true(is.na(iprobs1na_shin3$problematic[2]))
  expect_false(is.na(iprobs1na_shin3$problematic[1]))
  expect_false(is.na(iprobs1na_shin3$margin[1]))

  expect_true(all(is.na(iprobs1na_bb$probabilities[2,])))
  expect_true(is.na(iprobs1na_bb$problematic[2]))
  expect_false(is.na(iprobs1na_bb$problematic[1]))
  expect_false(is.na(iprobs1na_bb$margin[1]))

  expect_true(all(is.na(iprobs1na_bb2$probabilities[2,])))
  expect_true(is.na(iprobs1na_bb2$problematic[2]))
  expect_false(is.na(iprobs1na_bb2$problematic[1]))
  expect_false(is.na(iprobs1na_bb2$margin[1]))

  expect_true(all(is.na(iprobs1na_wpo$probabilities[2,])))
  expect_true(is.na(iprobs1na_wpo$problematic[2]))
  expect_false(is.na(iprobs1na_wpo$problematic[1]))
  expect_false(is.na(iprobs1na_wpo$margin[1]))

  expect_true(all(is.na(iprobs1na_or$probabilities[2,])))
  expect_true(is.na(iprobs1na_or$problematic[2]))
  expect_false(is.na(iprobs1na_or$problematic[1]))
  expect_false(is.na(iprobs1na_or$margin[1]))

  expect_true(all(is.na(iprobs1na_power$probabilities[2,])))
  expect_true(is.na(iprobs1na_power$problematic[2]))
  expect_false(is.na(iprobs1na_power$problematic[1]))
  expect_false(is.na(iprobs1na_power$margin[1]))

  expect_true(all(is.na(iprobs1na_additive$probabilities[2,])))
  expect_true(is.na(iprobs1na_additive$problematic[2]))
  expect_false(is.na(iprobs1na_additive$problematic[1]))
  expect_false(is.na(iprobs1na_additive$margin[1]))

  expect_true(all(is.na(iprobs1na_jsd$probabilities[2,])))
  expect_true(is.na(iprobs1na_jsd$problematic[2]))
  expect_false(is.na(iprobs1na_jsd$problematic[1]))
  expect_false(is.na(iprobs1na_jsd$margin[1]))

  expect_true(all(is.na(iprobs1na_goto$probabilities[2,])))
  expect_true(is.na(iprobs1na_goto$problematic[2]))
  expect_false(is.na(iprobs1na_goto$problematic[1]))
  expect_false(is.na(iprobs1na_goto$margin[1]))

})


# Target probabilities other than 1 ----
context("Target probabilities other than 1")

# Some English Premier League relegation odds. Should sum to 3.
relegation_odds <- c(1.53, 1.67, 1.25, 2.38, 2.38, 4.5, 5.5, 6.5, 7)


iprobs3_basic <- implied_probabilities(relegation_odds, method='basic',
                                      target_probability = 3,
                                      normalize = FALSE)

# Shin does not work with the test odds.
# iprobs3_shin <- implied_probabilities(relegation_odds, method='shin',
#                                       shin_method = 'uniroot',
#                                       target_probability = 3, normalize = FALSE)


iprobs3_bb <- implied_probabilities(relegation_odds, method='bb',
                                     target_probability = 3, normalize = FALSE)

iprobs3_wpo <- implied_probabilities(relegation_odds, method='wpo',
                                       target_probability = 3, normalize = FALSE)

iprobs3_power <- implied_probabilities(relegation_odds, method='power',
                                      target_probability = 3, normalize = FALSE)

iprobs3_or <- implied_probabilities(relegation_odds, method='or',
                                       target_probability = 3, normalize = FALSE)

iprobs3_additive <- implied_probabilities(relegation_odds, method='additive',
                                    target_probability = 3, normalize = FALSE)


iprobs3_jsd <- implied_probabilities(relegation_odds, method='jsd',
                                          target_probability = 3, normalize = FALSE)


iprobs3_goto <- implied_probabilities(relegation_odds, method='goto',
                                     target_probability = 3, normalize = FALSE)


test_that("Target probability 3", {

  expect_equal(class(iprobs3_basic), 'list')
  expect_equal(class(iprobs3_bb), 'list')
  expect_equal(class(iprobs3_wpo), 'list')
  expect_equal(class(iprobs3_power), 'list')
  expect_equal(class(iprobs3_or), 'list')
  expect_equal(class(iprobs3_additive), 'list')
  expect_equal(class(iprobs3_jsd), 'list')
  expect_equal(class(iprobs3_goto), 'list')


  expect_equal(all(abs(rowSums(iprobs3_basic$probabilities) - 3) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs3_bb$probabilities) - 3) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs3_wpo$probabilities) - 3) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs3_power$probabilities) - 3) < 0.0001), TRUE)
  expect_equal(all(abs(rowSums(iprobs3_or$probabilities) - 3) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs3_additive$probabilities) - 3) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs3_jsd$probabilities) - 3) < toll), TRUE)
  expect_equal(all(abs(rowSums(iprobs3_goto$probabilities) - 3) < toll), TRUE)


  expect_equal(all(iprobs3_basic$margin > 0), TRUE)
  expect_equal(all(iprobs3_bb$margin > 0), TRUE)
  expect_equal(all(iprobs3_wpo$margin > 0), TRUE)
  expect_equal(all(iprobs3_power$margin > 0), TRUE)
  expect_equal(all(iprobs3_or$margin > 0), TRUE)
  expect_equal(all(iprobs3_additive$margin > 0), TRUE)
  expect_equal(all(iprobs3_additive$jsd > 0), TRUE)

  expect_equal(is.null(iprobs3_wpo$specific_margins), FALSE)
  expect_equal(is.null(iprobs3_or$odds_ratios), FALSE)
  expect_equal(is.null(iprobs3_power$exponents), FALSE)
  expect_equal(is.null(iprobs3_jsd$distance), FALSE)

  })



# Implied odds ----
context("Implied odds")


my_probs <- rbind((1/c(1.5, 5, 7.5)),
                  c(0.1, 0.2, 0.7),
                  c(0.01, 0.3, 0.69))

my_margin <- 0.022


iodds1_basic <- implied_odds(my_probs, method='basic', margin = my_margin)
iodds1_shin <- implied_odds(my_probs, method='shin', margin = my_margin)
iodds1_shin2 <- implied_odds(my_probs, method='shin', margin = my_margin, grossmargin = 0.01)
iodds1_bb <- implied_odds(my_probs, method='bb', margin = my_margin)
iodds1_bb2 <- implied_odds(my_probs, method='bb', margin = my_margin, grossmargin = 0.01)
iodds1_wpo <- implied_odds(my_probs, method='wpo', margin = my_margin)
iodds1_or <- implied_odds(my_probs, method='or', margin = my_margin)
iodds1_power <- implied_odds(my_probs, method='power', margin = my_margin)
iodds1_additive <- implied_odds(my_probs, method='additive', margin = my_margin)


iodds1_basic0 <- implied_odds(my_probs, method='basic', margin = 0)
iodds1_bb0 <- implied_odds(my_probs, method='bb', margin = 0)
iodds1_wpo0 <- implied_odds(my_probs, method='wpo', margin = 0)
iodds1_or0 <- implied_odds(my_probs, method='or', margin = 0)
iodds1_power0 <- implied_odds(my_probs, method='power', margin = 0)
iodds1_additive0 <- implied_odds(my_probs, method='additive', margin = 0)



test_that("Output", {

  expect_equal(class(iodds1_basic), 'list')
  expect_equal(class(iodds1_shin), 'list')
  expect_equal(class(iodds1_shin2), 'list')
  expect_equal(class(iodds1_bb), 'list')
  expect_equal(class(iodds1_bb0), 'list')
  expect_equal(class(iodds1_bb2), 'list')
  expect_equal(class(iodds1_wpo), 'list')
  expect_equal(class(iodds1_wpo0), 'list')
  expect_equal(class(iodds1_or), 'list')
  expect_equal(class(iodds1_or0), 'list')
  expect_equal(class(iodds1_power), 'list')
  expect_equal(class(iodds1_power0), 'list')
  expect_equal(class(iodds1_additive), 'list')
  expect_equal(class(iodds1_additive0), 'list')


  # Sum of improper probabilties sum to 1 + margin
  expect_true(all(abs(rowSums(1 / iodds1_basic$odds) - (1 + my_margin)) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_shin$odds) - (1 + my_margin)) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_shin2$odds) - (1 + my_margin)) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_bb$odds) - (1 + my_margin)) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_bb2$odds) - (1 + my_margin)) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_wpo$odds) - (1 + my_margin)) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_or$odds) - (1 + my_margin)) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_power$odds) - (1 + my_margin)) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_additive$odds) - (1 + my_margin)) <= toll))

  # When theres no margin, probabilities should sum to 1.
  expect_true(all(abs(rowSums(1 / iodds1_basic0$odds) - 1) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_bb0$odds) - 1) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_wpo0$odds) - 1) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_or0$odds) - 1) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_power0$odds) - 1) <= toll))
  expect_true(all(abs(rowSums(1 / iodds1_additive0$odds) - 1) <= toll))

  # Check the coefficients for being alright.
  expect_true(all(iodds1_shin$zvalues > 0))
  expect_true(all(iodds1_shin2$zvalues > 0))

  expect_true(all(iodds1_bb0$zvalues >= 0))
  expect_true(all(iodds1_bb$zvalues > 0))
  expect_true(all(iodds1_bb2$zvalues > 0))

  expect_true(all(iodds1_or$odds_ratios > 1))
  expect_true(all(iodds1_or0$odds_ratios == 1))

  expect_true(all(iodds1_power0$exponents == 1))
  expect_true(all(iodds1_power$exponents < 1))

  expect_true(all(iodds1_additive$odds > 1))
  expect_true(all(iodds1_additive0$odds > 1))


  # Check the odds.
  expect_true(all(iodds1_basic$odds > 1))
  expect_true(all(iodds1_shin$odds > 1))
  expect_true(all(iodds1_shin2$odds > 1))
  expect_true(all(iodds1_bb$odds > 1))
  expect_true(all(iodds1_bb2$odds > 1))
  expect_true(all(iodds1_wpo$odds > 1))
  expect_true(all(iodds1_or$odds > 1))
  expect_true(all(iodds1_power$odds > 1))
  expect_true(all(iodds1_additive$odds > 1))

  expect_true(all(iodds1_basic0$odds > 1))
  expect_true(all(iodds1_bb0$odds > 1))
  expect_true(all(iodds1_wpo0$odds > 1))
  expect_true(all(iodds1_or0$odds > 1))
  expect_true(all(iodds1_power0$odds > 1))
  expect_true(all(iodds1_additive0$odds > 1))

})



# uniroot options ----
context("uniroot options")

# Example where the interval is too narrow (true or > 1.03), and extendInt is set to 'no.


test_that("Uniroot options",

  # Example where the interval is too narrow (true or > 1.03), and extendInt is set to 'no.
  # Should throw an error, thus demonstrating that the uniroot_options works.
  expect_warning(
    implied_probabilities(my_odds, method='or', uniroot_options = list(interval = c(1, 1.01), extendInt = 'no'))
  )

)

# Converting between odds and probabilities ----
context("Converting between odds and probabilities")


# Re-compute odds.

idx <- 3 # The row in my_odds to check.

iodds1_basic_r <- implied_odds(iprobs1_basic$probabilities[idx,],
                               method='basic', margin = iprobs1_basic$margin[idx])

iodds1_shin_r <- implied_odds(iprobs1_shin$probabilities[idx,],
                               method='shin', margin = iprobs1_shin$margin[idx])

iodds1_shin2_r <- implied_odds(iprobs1_shin2$probabilities[idx,],
                              method='shin', margin = iprobs1_shin$margin[idx], grossmargin = 0.01)

iodds1_bb_r <- implied_odds(iprobs1_bb$probabilities[idx,],
                            method='bb', margin = iprobs1_bb$margin[idx])

iodds1_bb2_r <- implied_odds(iprobs1_bb2$probabilities[idx,],
                            method='bb', margin = iprobs1_bb2$margin[idx], grossmargin = 0.01)

iodds1_wpo_r <- implied_odds(iprobs1_wpo$probabilities[idx,],
                             method='wpo', margin = iprobs1_wpo$margin[idx])

iodds1_or_r <- implied_odds(iprobs1_or$probabilities[idx,],
                            method='or', margin = iprobs1_or$margin[idx])

iodds1_power_r <- implied_odds(iprobs1_power$probabilities[idx,],
                               method='power', margin = iprobs1_power$margin[idx])

iodds1_additive_r <- implied_odds(iprobs1_additive$probabilities[idx,],
                                  method='additive', margin = iprobs1_additive$margin[idx])


test_that("Results", {

  # Check that we can recover the original odds.
  expect_true(all(abs(iodds1_basic_r$odds - my_odds[idx,]) <= toll))
  expect_true(all(abs(iodds1_shin_r$odds - my_odds[idx,]) <= 0.001))
  expect_true(all(abs(iodds1_shin2_r$odds - my_odds[idx,]) <= 0.00015))

  expect_true(all(abs(iodds1_bb_r$odds - my_odds[idx,]) <= toll))
  expect_true(all(abs(iodds1_bb2_r$odds - my_odds[idx,]) <= toll))
  expect_true(all(abs(iodds1_wpo_r$odds - my_odds[idx,]) <= toll))
  expect_true(all(abs(iodds1_or_r$odds - my_odds[idx,]) <= toll))
  expect_true(all(abs(iodds1_power_r$odds - my_odds[idx,]) <= toll))
  expect_true(all(abs(iodds1_additive_r$odds - my_odds[idx,]) <= toll))

  # Check that the coefficients are the same.
  expect_true(all(abs(iodds1_shin_r$zvalues - iprobs1_shin$zvalues[idx]) <= 0.0001))
  expect_true(all(abs(iodds1_bb_r$zvalues - iprobs1_bb$zvalues[idx]) <= toll))
  expect_true(all(abs(iodds1_bb2_r$zvalues - iprobs1_bb2$zvalues[idx]) <= toll))
  expect_true(all(abs(iodds1_wpo_r$specific_margins - iprobs1_wpo$specific_margins[idx,]) <= toll))
  expect_true(abs(iodds1_or_r$odds_ratios - iprobs1_or$odds_ratios[idx]) <= toll)
  expect_true(abs(iodds1_power_r$exponents - iprobs1_power$exponents[idx]) <= toll)

})



