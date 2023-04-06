

# implied Version 0.4.99
* Can now convert odds to probabilities that should sum to other values than 1, using the target_probability argument in implied_probabilities().
* New vignette 'Troubleshooting'.
* Introduction vignette updated with how to use the new target_probability option.
* New option 'uniroot_options' in implied_probabilities, to better control the uniroot solver.
* Fixed many spelling errors in the documentation.


# implied Version 0.4.1
* Small change to how the 'jsd' method in implied_probabilities() works, so that it works in some cases where it used to fail.
* Fixed a link in the Introduction vignette.


# implied Version 0.4.0
* New function implied_odds(), that converts probabilities to odds with a given margin.
* New method = 'jsd' in implied_probabilities(). Check the introductory vignette for more information.

# implied Version 0.3.2
* Fixed wrong formula for the WPO method in the introduction vignette.

# implied Version 0.3.1
* Raises error if the inverse odds sum to less than 1.
* NA's will be returned if there are NA's in the input odds.

# implied Version 0.3.0
* A new algorithm for Shin's method is included.
* Small bugfix for when Shin's method fails and produces NA's. Instead of crashing, it now raises a warning and flags the result as problematic.  


# implied Version 0.2.5
* First version on CRAN.
