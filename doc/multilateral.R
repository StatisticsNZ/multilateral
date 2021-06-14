## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=8
)

## ----setup---------------------------------------------------------------
#devtools::install_github("MjStansfi/multilateral")
library(multilateral)
library(ggplot2)

## ----turvey str----------------------------------------------------------
str(turvey)

## ----turvey multilat, echo=TRUE, error=FALSE, fig.keep='all', message=FALSE, warning=FALSE, results='hide'----
index_methods <- c("FEWS","GEKS-T","GEKS-J","GEKS-F")

start_run_time <- Sys.time()
turvey_multilats <- lapply(index_methods, function(index_method){
  
  
  temp_index <- multilateral(period = turvey$month,
                             id = turvey$commodity,
                             price = turvey$price,
                             quantity = turvey$quantity,
                             index_method = index_method)
  
  #For identification in plot
  temp_index$index$type <- index_method
  
  return(temp_index$index)  
})
end_run_time <- Sys.time()


turvey_multilats <- do.call(rbind,turvey_multilats)

plot <- ggplot(turvey_multilats)+geom_line(aes(x = period, y = index, colour = type))

print(plot)

## ----run_time------------------------------------------------------------
print(end_run_time-start_run_time)

## ----synthetic gfk-------------------------------------------------------
str(synthetic_gfk)

## ----wrangle, error=FALSE, warning=FALSE, message=FALSE------------------
library(dplyr)


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

## ----run itrygeks, echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='hide',fig.keep='all'----

itrygeks_index <- multilateral(period = synthetic_gfk$month_num,
                               id = synthetic_gfk$prodid_num, 
                               price = synthetic_gfk$uv,
                               quantity = synthetic_gfk$quantity,
                               features = features,
                               splice_method = "geomean",
                               index_method = "GEKS-IT")

#For identification in plot
itrygeks_index$index$type <- "itrygeks"

## ----run hedonic, echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='hide',fig.keep='all'----

#Note no unique product ID

hedonic_index <- multilateral(period = synthetic_gfk$month_num,
                              price = synthetic_gfk$uv,
                              quantity = synthetic_gfk$quantity,
                              features = features,
                              splice_method = "geomean",
                              index_method = "hedonic")


#For identification in plot
hedonic_index$index$type <- "hedonic"

## ----plot features, echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='hide',fig.keep='all'----
feature_indexes <- rbind(itrygeks_index$index,hedonic_index$index)

plot <- ggplot(feature_indexes)+geom_line(aes(x = period, y = index, colour = type))

print(plot)

## ----turvey splice, echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='hide',fig.keep='all'----

splice_methods <- c("geomean","geomean_short","window","movement","half")
turvey_splices <- lapply(splice_methods, function(splice_method){
  
  
  temp_index <- multilateral(period = turvey$month,
                             price = turvey$price,
                             id = turvey$commodity,
                             quantity = turvey$quantity,
                             window_length = 13,
                             splice_method = splice_method,
                             index_method = "FEWS")
  
  temp_index$index$type <- splice_method
  
  return(temp_index$index)  
})

turvey_splices <- do.call(rbind,turvey_splices)

plot <- ggplot(turvey_splices)+geom_line(aes(x = period, y = index, colour = type))

print(plot)

