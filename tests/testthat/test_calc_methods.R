context("Test calculation methods with splice")
library(multilateral)

library(dplyr)
#TURVEY
index_methods <- c("TPD","GEKS-T","GEKS-J","GEKS-F")

turvey_multilats <- lapply(index_methods, function(index_method){


  temp_index <- multilateral(period = turvey$month,
                             price = turvey$price,
                             id = turvey$commodity,
                             quantity = turvey$quantity,
                             index_method = index_method)

  temp_index$index$type <- index_method

  return(temp_index$index)
})

turvey_multilats <- do.call(rbind,turvey_multilats)
latest <- turvey_multilats[turvey_multilats$period==max(turvey_multilats$period),]

synthetic_gfk <- synthetic_gfk%>%
  group_by(month_num,prodid_num)%>%
  mutate(quantity = sum(quantity),
         value = sum(value))%>%
  ungroup()%>%
  unique

#Calculate the unit value (price)
synthetic_gfk$uv <- synthetic_gfk$value/synthetic_gfk$quantity

#Extract data.frame containing features of interest
features <-synthetic_gfk[,grepl("char",colnames(synthetic_gfk))]


impute_torn <- multilateral(period = synthetic_gfk$month_num,
                            price = synthetic_gfk$uv,
                            quantity = synthetic_gfk$quantity,
                            id = synthetic_gfk$prodid_num,
                            features = features,
                            window_length = 13,
                            splice_method = "geomean",
                            index_method = "GEKS-IT",
                            matched = TRUE)



TDH <- multilateral(period = synthetic_gfk$month_num,
                        price = synthetic_gfk$uv,
                        quantity = synthetic_gfk$quantity,
                        # id = synthetic_gfk$prodid_num,
                        features = features,
                        window_length = 13,
                        splice_method = "geomean",
                        index_method = "TDH")


test_that("All ID based index calculations are as expected in latest period\n", {
  expect_equal(round(latest$index[latest$type=="TPD"],6), 1.526554)
  expect_equal(round(latest$index[latest$type=="GEKS-T"],6), 1.476020)
  expect_equal(round(latest$index[latest$type=="GEKS-J"],6), 1.169337)
  expect_equal(round(latest$index[latest$type=="GEKS-F"],6), 1.440379)
})

test_that("All feature based index calculations are as expected in latest period\n", {
  expect_equal(round(
    impute_torn$index$index[impute_torn$index$period==max(impute_torn$index$period)],6), 1.033194)

  expect_equal(round(
    TDH$index$index[TDH$index$period==max(TDH$index$period)],6), 1.061368)

})
