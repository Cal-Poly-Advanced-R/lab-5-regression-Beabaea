#' Implements ridge regression with many predictors
#'
#' This function computes coefficients for ridge regression
#' All columns of the provided data frame are used as predictors, except the
#' one specified as a response.
#'
#' No interaction terms are included.
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
ridge_regression <- function(dat, response, lambda) {

  results <- lapply(lambda, FUN=single_ridge_regression,
                    dat={{dat}}, response={{response}})
  #result <- purrr::map_dfr(lambda, ~ single_ridge_regression({{dat}}, {{response}}, .x))
  #results<-data.frame(t(results))
  results <- data.table::rbindlist(results)
  return(results)

}

#' Determines the best penalty term from a set of options
#'
#' This function uses a randomly chosen test and training set
#'
#' No interaction terms are included.
#'
#'
#' @param train_dat A data frame to construct the model from
#' @param test_dat A data frame to test the model on
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A vector of penalty terms to try
#'
#' @return A data frame of penalty terms and resulting errors
#'
#' @import dplyr
#'
#' @export
find_best_lambda <- function(train_dat, test_dat, response, lambdas) {

  #make dataframe of coeff and lambda
  coeflambda <- ridge_regression(train_dat, mpg, lambdas)

  #make an x and expected dataframe using the test_data
  x <- as.matrix(test_dat%>%select(-mpg))
  x <- cbind(1,x)
  expected <- test_dat%>%select({{response}})

  #making a function cuz its easier
  RRerror <- function(coef,x,expected){
    calc <- x %*% coef
    diff <- (expected-calc)^2
    result <- sum(diff)/rnow(diff)
    return(result)
  }
  ### lambda_errors should be a data frame with two columns: "lambda" and "error"
  ### For each lambda, you should record the resulting Sum of Squared error
  ### (i.e., the predicted value minus the real value squared) from prediction
  ### on the test dataset.

  return(lambda_errors)
}

#'Runs a ridge regression for a single lambda
#'
#'
#' @param dat A data frame
#' @param response The name of a response variable in the data frame (unquoted)
#' @param lambda A numeric value of a penalty terms to try
#'
#' @return A data frame of coefficients
#'
#' @import dplyr
#'
#' @export
#'
single_ridge_regression<-function(dat, response, lambda) {

  #equation for beta
  #beta = (t(x)*x + lambda(I))-1 * t(x)Y

  #first lets create the matrices
  x <-data.matrix(dat%>%
                    select(-{{response}})%>%
                    scale())#scale
  names<-names(dat%>%
                 select(-{{response}}))#save var names
  x<-cbind(1,x) #add intercept
  colnames(x)<-c("Intercept",names) #give intercept name

  y <- data.matrix(dat%>%
                     select({{response}}))
  i <- ncol(x)



  #now the math
  beta<-solve(crossprod(x) + lambda*diag(i)) %*% crossprod(x,y)

  results <- data.frame(t(beta))%>%mutate(lambda=lambda)
  ### This should be a data frame, with columns named
  ### "Intercept" and the same variable names as dat, and also a column
  ### called "lambda".

  return(results)
}
