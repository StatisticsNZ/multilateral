context("Test splice methods")
library(multilateral)


#First two windows of Turvey with TPD method, window length 13
old_window <- c(1,
                0.971106984,
                0.949054291,
                1.047073965,
                1.308492016,
                1.286578703,
                1.30327077,
                1.276206486,
                1.105193456,
                1.028340119,
                1.086910614,
                1.054583144,
                1.107812376)
new_window <- c(1,
                0.976549871,
                1.075633141,
                1.343338214,
                1.320139911,
                1.337845896,
                1.311287199,
                1.135980815,
                1.057137399,
                1.118093556,
                1.086292759,
                1.141368481,
                1.10690248)

test_that("All splice methods work", {
  expect_equal(round(splice_update(old_window,new_window,"geomean"),6), 0.971835)
  expect_equal(round(splice_update(old_window,new_window,"geomean_short"),6), 0.972020)
  expect_equal(round(splice_update(old_window,new_window,"window"),6), 0.970309)
  expect_equal(round(splice_update(old_window,new_window,"movement"),6), 0.969803)
  expect_equal(round(splice_update(old_window,new_window,"half"),6), 0.973356)
})
