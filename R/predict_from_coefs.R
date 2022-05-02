#' Computes predicted values given coefficients
#'
#' This function takes a data frame of coefficients in the form outputted by
#' functions like \code{multiple_linear_regression} or \code{ridge_regression}.
#'
#' It calculates the predicted values of a response variable from these coefficients.
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param coefs A data frame of coefficient estimates
#'
#' @return A data frame of true and predicted values
#'
#' @import dplyr
#'
#' @export
predict_from_coefs <- function(dat, response, coefs){

  #first let make the df of coeff
  a<-data.matrix(coefs)

  #now lets make the x data frame
  x<-cbind(1, dat %>% select(-{{response}}))
  #align names
  names(x)<-names(coefs)
  #into matrix form
  x<-data.matrix(x)

  #now multiply to get predicted results
  predicted<- a %*% x
  observed<- dat %>% select({{response}})

  #formulate results
  results<-data.frame(cbind(predicted,observed))
  return(results)

}
