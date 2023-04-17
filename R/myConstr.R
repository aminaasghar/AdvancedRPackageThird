#' Connstrutor function for t tests
#'
#' @param x vector of data
#' @param y vector of data
#' @param alpha alpha level
#'
#' @return a list containing the data frame and relevant information
#' @export
#'
#' @examples
#' myttest(x=rnorm(30,10,12), y=rnorm(40, 7, 10))
myConstr <- function(x, y, alpha = 0.05) {# First define the function myttest,
  # which takes two vectors and  alpha
  # as arguments

  tt <- t.test(x, y, var.equal = TRUE, conf.level = 1 - alpha) # since variances
  # equal,perform
  # this t-test

  # Here is the updated bit from the first version instead of adding NaN's when
  # printing , it is done here in the constructor by padding the shorter vector
  # vector with NaN's for missing vales so that , the length of both the vectors
  # equal when the data frame is created.

  maximum_length <- max(length(x), length(y))  # first find the maximum length
  # between two vectors, which will
  # be used to pad shorter vector

  x_padding <- c(x, rep(NaN, maximum_length - length(x))) # padding x
  y_padding <- c(y, rep(NaN, maximum_length - length(y))) # padding y

  df <- data.frame("x" = x_padding, "y" = y_padding)# creating the data frame
  # with two vectors of same
  # length
  # Create named list where result(list) will contain a data frame containing x
  # and y, alpha, The confidence interval for mux-muy, and the p value
  result <- list(
    data_frame = df,
    alpha_value = alpha,
    confidence_interval = tt$conf.int,
    p_value = tt$p.value
  )

  class(result) <- "Rttest"         # we set the class of the named list result
  # to be Rttest
  return(result)                    # The constructor will return the named list
  # called result
}
