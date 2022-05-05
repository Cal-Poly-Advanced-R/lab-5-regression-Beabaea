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
  coeflambda <- ridge_regression({{train_dat}}, {{response}}, {{lambdas}})

  #list of data frames with only the coefficient
  coefs <- split( coeflambda[,-4], seq(nrow(coeflambda)) )

  #make an x and expected dataframe using the test_data
  x <- as.matrix(test_dat%>%select(-{{response}}))
  x <- cbind(1,x)
  expected <- test_dat%>%select({{response}})

  #making a function bc its easier
  RRerror <- function(coef,x,expected){

    #need to take a data frame and make it a usable matrix
    mcoef<-t(data.matrix({{coef}}))

    #doing the math
    calc <- x %*% mcoef

    #calulate the differences and sum of squares
    diff <- (expected-calc)^2
    result <- sum(diff)

    return(result)#should return a number
    }

  #this function will take the list and use it to make df of list
  errors<-purrr::map_dfr(coefs,~RRerror(.x,x,expected))

  #creating the dataframe and renaming
  lambda_errors<-as.data.frame(cbind(coeflambda$lambda,t(errors)))
  names(lambda_errors)<-c("lambda","error")

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
