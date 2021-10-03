context("Test chain methods")

library(multilateral)

#To get the index_list
tpd_index <- multilateral(period = turvey$month,
                          id = turvey$commodity,
                          price = turvey$price,
                          quantity = turvey$quantity,
                          chain_method = "geomean",
                          window_length = 13,
                          index_method = "TPD")


index_list <- split(tpd_index$index_windows,tpd_index$index_windows$window_id)
window_length  <-  13

geomean_chained_index <- get_chain_index_df(index_list, window_length, chain_method = "geomean")
window_chained_index <- get_chain_index_df(index_list, window_length, chain_method = "window")
movement_chained_index <- get_chain_index_df(index_list, window_length, chain_method = "movement")

test_that("chain index method results\n", {
  expect_equal(round(geomean_chained_index$index$index[48],6), 1.491595)
  expect_equal(round(window_chained_index$index$index[37],6), 1.056023)
  expect_equal(round(movement_chained_index$index$index[37],6), 1.296136)
})