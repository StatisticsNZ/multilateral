context("Test splice methods")
library(multilateral)


#First two windows of Turvey with TPD method
old_window <- c(1,
                1.249043375,
                1.503076003,
                1.757604732,
                2.081349132,
                2.34446107,
                2.561226171,
                2.983380052,
                3.216362813,
                3.523596524,
                3.774214392,
                4.018404899,
                4.286015359)
new_window <- c(1,
                1.201386446,
                1.400784423,
                1.656833211,
                1.863508504,
                2.037247501,
                2.375003553,
                2.560485253,
                2.802786226,
                3.005634563,
                3.21243871,
                3.429538798,
                3.663962317)

test_that("All splice methods work", {
  expect_equal(round(splice_update(old_window,new_window,"geomean"),6), 1.072298)
  expect_equal(round(splice_update(old_window,new_window,"geomean_short"),6), 1.072657)
  expect_equal(round(splice_update(old_window,new_window,"window"),6), 1.067763)
  expect_equal(round(splice_update(old_window,new_window,"movement"),6), 1.068354)
  expect_equal(round(splice_update(old_window,new_window,"half"),6), 1.074735)
})
