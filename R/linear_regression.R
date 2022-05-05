#' Implements simple linear regression by hand
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param explanatory The name of the explanatory variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
simple_linear_regression <- function(dat, response, explanatory, method = NULL){

  x <- dat %>% pull({{explanatory}})
  y <- dat %>% pull({{response}})

  explan_name <- dat %>%
    select({{explanatory}}) %>%
    names()

  x_bar <- mean(x)
  y_bar <- mean(y)

  ### Edit code after here

  sd_x <- sd(x,na.rm=T)
  sd_y <- sd(y,na.rm=T)

  beta_1 <- (cor(x,y)) * (sd_y/sd_x)
  beta_0 <- y_bar - beta_1 * x_bar

  ### Stop editing

  results <- tibble::tibble(
    Intercept = beta_0,
    Slope = beta_1
  )

  names(results)[2] <- explan_name

  return(results)

}


#' Implements linear regression with many predictors by hand
#'
#' This function computes coefficients for multiple regression by hand.
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param method The method used to compute the coefficients (NULL, "qr", "gradientdescent")
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#'@export
multiple_linear_regression <- function(dat, response, method = NULL) {

  #get the names of the explanatory variables
  names<-c(names(dat),"Intercept")

  #make matrices
  #response matrix
  y<-dat%>%select({{response}}) #{{response}}
  y<-data.matrix(y)

  #slope and intercept matrix
  x<-dat %>%
    select(-{{response}}) %>% #{{response}}
    mutate(Intercept=1) #maybe need to change order?
  x<-data.matrix(x)

  #solve for matrix
  #(t(x)*x)^-1 * t(x) * y
  result_matrix<-solve(crossprod(x)) %*% crossprod(x, y)


  results <- data.frame(t(result_matrix))
  ### This should be a data frame, with columns names
  ### "Intercept" and the same variable names as dat.

  return(results)

}
