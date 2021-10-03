## ---- include = FALSE----------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.width=8
)

## ----setup, echo=TRUE, error=FALSE, fig.keep='all', message=FALSE, warning=FALSE, results='hide'----
#devtools::install_github("MjStansfi/multilateral")
library(multilateral)

tpd_index <- multilateral(period = turvey$month,
                          id = turvey$commodity,
                          price = turvey$price,
                          quantity = turvey$quantity,
                          splice_method = "geomean",
                          window_length = 13,
                          index_method = "TPD")


## ----str of tpd_index----------------------------------------------------
str(tpd_index)

## ----turvey str----------------------------------------------------------
str(turvey)

## ----turvey multilat, echo=TRUE, error=FALSE, message=FALSE, warning=FALSE, results='hide'----
library(ggplot2)

index_methods <- c("TPD","GEKS-T","GEKS-J","GEKS-F")

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

## ----run TDH, echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='hide',fig.keep='all'----

#Note no unique product ID

TDH_index <- multilateral(period = synthetic_gfk$month_num,
                              price = synthetic_gfk$uv,
                              quantity = synthetic_gfk$quantity,
                              features = features,
                              splice_method = "geomean",
                              index_method = "TDH")


#For identification in plot
TDH_index$index$type <- "TDH"

## ----plot features, echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='hide',fig.keep='all'----
feature_indexes <- rbind(itrygeks_index$index,TDH_index$index)

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
                             index_method = "TPD")
  
  temp_index$index$type <- splice_method
  
  return(temp_index$index)  
})

turvey_splices <- do.call(rbind,turvey_splices)

plot <- ggplot(turvey_splices)+geom_line(aes(x = period, y = index, colour = type))

print(plot)

## ----turvey chain, echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='hide',fig.keep='all'----

chain_methods <- c("geomean","window","movement","half")
turvey_chains <- lapply(chain_methods, function(chain_method){
  
  
  temp_index <- multilateral(period = turvey$month,
                             price = turvey$price,
                             id = turvey$commodity,
                             quantity = turvey$quantity,
                             window_length = 13,
                             # splice_method = splice_method,
                             chain_method = chain_method,
                             index_method = "TPD")
  
  temp_index$index$type <- chain_method
  
  return(temp_index$index)  
})

turvey_chains <- do.call(rbind,turvey_chains)


plot <- ggplot()+
  geom_line(aes(x = period, y = index, colour = "chain"), data = turvey_chains[turvey_chains$type=="geomean"])+
  geom_line(aes(x = period, y = index, colour = "splice"), data = turvey_splices[turvey_splices$type=="geomean"])+
  ggtitle("Geomean chain compared to splice for turvey")

print(plot)

## ----results across package, echo=TRUE,warning=FALSE,message=FALSE,error=FALSE, results='hide',fig.keep='all'----
library(multilateral)
library(IndexNumR)
library(PriceIndices)
library(dplyr)
#---------------------#multilateral package
turvey_multilateral <- multilateral(period = turvey$month,
                                    price = turvey$price,
                                    quantity = turvey$quantity,
                                    id = turvey$commodity,
                                    window_length = 13,
                                    splice_method = "geomean",
                                    index_method = "GEKS-F",
                                    check_inputs_ind = T)

#---------------------#PriceIndices package

#Requires data.frame with specific colnames, and format of time

turvey_mod <- turvey%>%select(time = month,
                              prodID = commodity,
                              prices = price,
                              quantities = quantity)

turvey_priceindices <- geks_splice(turvey_mod, "1970-01", "1973-12", 13, splice = "mean", interval = T)


#---------------------#IndexNumR package

#Requires time to be a sequential numeric vector

turvey <- turvey%>%mutate(month_num = as.numeric(as.factor(month)))

turvey_IndexNumR <- GEKSIndex(turvey,
          pvar = "price",
          qvar = "quantity",
          pervar = "month_num",
          prodID = "commodity",
          indexMethod = "fisher",
          window = 13,
          splice = "mean")


## ----print resluts-------------------------------------------------------
print(str(turvey_multilateral))
print(str(turvey_priceindices))
print(str(turvey_IndexNumR))


## ----parallel, eval=FALSE, include=TRUE----------------------------------
#  nrow(big_data)
#  #[1] 100000000
#  
#  multilateral(period = big_data$month,
#               price = big_data$price,
#               quantity = big_data$quantity,
#               id = big_data$commodity,
#               window_length = 13,
#               splice_method = "geomean",
#               index_method = "TPD",
#               check_inputs_ind = T,
#               num_cores = 32)

