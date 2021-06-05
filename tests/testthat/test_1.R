

# some example odds
my_odds <- rbind(c(4.20, 3.70, 1.95),
                 c(2.45, 3.70, 2.90),
                 c(2.05, 3.20, 3.80))

# Some odds desinged to be problematic with the additive method.
# It is also problematic with the wpo method.
my_odds2 <- t(matrix(1/c(0.870, 0.2, 0.1, 0.05, 0.02, 0.01)))



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

# Shin method uniroot, with grossmargin != 0 should switch to shin_method = 'js'
# Make sure the output is the same.
iprobs1_shin4 <- implied_probabilities(my_odds, method='shin', shin_method = 'uniroot', grossmargin = 0.01)


iprobs2_basic <- implied_probabilities(my_odds2)
iprobs2_shin <- implied_probabilities(my_odds2, method='shin')
iprobs2_shin3 <- implied_probabilities(my_odds2, method='shin', shin_method = 'uniroot')
iprobs2_or <- implied_probabilities(my_odds2, method='or')
iprobs2_power <- implied_probabilities(my_odds2, method='power')



# tolerance for some tests
toll <- 0.0001


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
  expect_equal(class(iprobs1_additive), 'list')

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

  expect_equal(is.null(iprobs1_shin$zvalues), FALSE)
  expect_equal(is.null(iprobs1_shin2$zvalues), FALSE)
  expect_equal(is.null(iprobs1_shin3$zvalues), FALSE)
  expect_equal(is.null(iprobs1_bb$zvalues), FALSE)
  expect_equal(is.null(iprobs1_bb2$zvalues), FALSE)
  expect_equal(is.null(iprobs1_wpo$specific_margins), FALSE)
  expect_equal(is.null(iprobs1_or$odds_ratios), FALSE)
  expect_equal(is.null(iprobs1_power$exponents), FALSE)


  expect_equal(class(iprobs2_basic), 'list')
  expect_equal(class(iprobs2_shin), 'list')
  expect_equal(class(iprobs2_shin2), 'list')
  expect_equal(class(iprobs2_shin3), 'list')
  expect_equal(class(iprobs2_wpo), 'list')
  expect_equal(class(iprobs2_or), 'list')
  expect_equal(class(iprobs2_additive), 'list')
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
})


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
})



