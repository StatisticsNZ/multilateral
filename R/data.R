#' Artificial prices of seasonal products Data created by R. Turvey
#'
#' The is dataset is presented in the ILO CPI manual. In 1979 Turvey sent his
#' artificial data set to statistical agencies around
#' the world, asking them to use their normal techniques to construct monthly
#' and annual average price indices. About 20 countries replied, and Turvey
#' summarized the responses as follows: ‘‘It will be seen that
#' the monthly indices display very large differences, e.g.,
#' a range of 129.12–169.50 in June, while the range of simple annual means
#' is much smaller. It will also be seen that the indices vary as to the
#' peak month or year.’’
#'
#' @format data frame with 176 rows and 4 variables
#' \describe{
#'   \item{month}{The time as a Date type}
#'   \item{commodity}{The seasonal product, as a factor
#'   (Apples, Grapes, Oranges, Peaches, Strawberries)}
#'   \item{price}{The price as numeric}
#'   \item{quantity}{The quantity of items sold in that month as an integar}}
#'
#' @source \url{https://www.ilo.org/wcmsp5/groups/public/---dgreports/---stat/documents/presentation/wcms_331153.pdf}
"turvey"


#' Synthetic scanner data for one consumer electronic product
#'
#' GfK have made this available as a public good for the international statistical community, to aid research into new price index methods.
#'
#'The data is based on one product category from the scanner data used in production of the New Zealand Consumers Price Index.
#'
#'The synthetic data has been heavily modified to remove identification potential, while still retaining some of the characteristics of
#'scanner data which make traditional index methods inadequate - such as high product turnover and volatile price and quantities - which motivate
#'the multilateral index methods that are currently being researched within the international statistical community.
#'
#' @format data frame with 5509 rows and 15 variables
#' \describe{
#'   \item{month_num}{Month number, 0-25}
#'   \item{char1-11}{Product characteristics}
#'   \item{prodid_num}{Product identifier, created from unique characteristics}
#'   \item{quantity}{The quantity of items sold in that month as an integar}
#'   \item{value}{Sales total (NZD)}}
#'
#' @source {GfK New Zealand}
"synthetic_gfk"

