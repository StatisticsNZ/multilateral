#'GEKS Imputation Tornqvist (GEKS-IT), also known as the ITRYGEKS (Imputation
#'Tornqvist Rolling Year GEKS)
#'
#'The GEKS method is the product of many bilateral comparisons that maximises
#'the number of paired price points while being free of chain drift. The GEKS-IT
#'is a variation of the GEKS that uses a bilateral 'imputation Tornqvist'
#'(regression based) index for all bilateral index calculations.
#' @keywords internal
ITRYGEKS_t <- function(p0,p1,q0,q1,f0,f1,id0,id1){

  #Create then modify exp_share
  # used again later so cache it
  timefact <- c(rep(0,length(p0)),rep(1,length(p1)))

  mod_exp <- data.table(timefact=timefact,
                        id=c(id0,id1),
                        exp=c(q0,q1)*c(p0,p1))


  #When id observed in both t0 and t1 take average exp_share
  #When id observed in only one (t0,t1) half the exp_share
  mod_exp <- mod_exp[,"exp_share_p":=exp/sum(exp),timefact]
  mod_exp <- mod_exp[,"exp_share":=sum(exp_share_p)/2,id]

  p <- log(c(p0,p1)) #log price
  exp_share <- mod_exp$exp_share #c(q0,q1)
  f <- rbindlist(list(f0,f1))
  id <- c(id0,id1)
  timefact <- as.factor(timefact)


  model_df <- droplevels(data.frame(p = p,
                                    id = id,
                                    f = f,
                                    timefact = timefact))

  # Regression doesn't work if there is only 1 item in the time window.
  model_df[,which(sapply(model_df,nlevels)==1)] <- NULL #Final check of factors with 1 level

  #log price
  glm_formula <- p ~ .

  # Run the regression
  all_coefs <-  coef(lm(glm_formula,
                        weights = exp_share,
                        data = model_df))

  # There are coefficients returned for each time period, and each product.
  # we are only interested in change of price wrt time - so only keep theses
  # coefficients. Theses rownames start with timefact
  rows_keep <- grepl(".*timefact.*", names(all_coefs))

  all_coef <-  all_coefs[rows_keep]

  return(exp(all_coef))

}

#'Fisher
#' @keywords internal
fisher_t <- function(p0,p1,q0,q1){
  las <- fixed_t(p0,p1,q0)
  pas <- fixed_t(p0,p1,q1)
  return(sqrt((las*pas)))
}

#'Laspeyres or Paasche
#' @keywords internal
fixed_t <- function(p0,p1,q){
  return(sum(p1*q)/sum(p0*q))
}

#'Tornqvist
#' @keywords internal
tornqvist_t <- function(p0,p1,q0,q1){
  exp0 <- sum(p0*q0)
  exp1 <- sum(p1*q1)
  s0 <- (p0*q0)/exp0
  s1 <- (p1*q1)/exp1
  return(prod((p1/p0)^(0.5*(s0+s1))))
}

#'Jevons
#' @keywords internal
jevons_t <- function(p0,p1){
  return(prod((p1/p0)^(1/length(p0))))
}
