#' Numerical Integration Using the Simpson Rule
#'
#' @param a The start value
#' @param b The end value
#' @param d The number of divisions
#' @param f The function that will be used for integration
#'
#' @return An object of the Simpson class, which has
#' \item{ab}{vector of starting and ending values}
#' \item{x}{vector internal values}
#' \item{y}{vector evaluated values values}
#' \item{result}{Simpson Rule result}
#
#'
#' @examples
#' f <- function(x) {sin(x)}
#' simpson_fun(a = 0, b = 10, d = 5, f)
#' @seealso \code{\link{trapezoid_fun}}
#' @rdname simpson_fun
#' @include simpson_fun.R
#' @aliases simpson_fun,ANY-method
#' @export
setGeneric(name= "simpson_fun",
           def = function(a, b, d, f)
             {standardGeneric("simpson_fun")})

#' @export
setMethod(f = "simpson_fun",
          definition =  function(a, b, d, f) {
  #Based on Trapezoid rule from https://www.r-bloggers.com/2017/08/the-trapezoidal-rule-of-numerical-integration-in-r/
  #Create a vector of start and end values, which will be used for the function's output
  ab <- c(a, b)
  #Define our h:
  h <- (b - a) / d
  #n is a vector of the number of divisions between a and b
  n <- 1 : (d - 1)

    #Create a vector of  odd n values
  #See: https://stackoverflow.com/questions/55080927/how-can-i-write-a-function-that-returns-odd-numbers-only-from-a-list-of-integers
  #This is selecting the odd elements from within n through subsetting
  n_odd <- n[ (n %% 2) != 0]
  #Create a vector of even n values
  #This is selecting the even elements from within n through subsetting
  n_even <- n[ (n %% 2) == 0]
  #Create  x values for for the odd n and even n
  #These vectors will allow for only the inner odd values to be multiplied by 4, and the inner even values to be multiplied by 2
  x_n_odd <- a + (n_odd * h)
  x_n_even <- a + (n_even *h)
  #Create a vector x, which will be a vector containing all of the values to be evaluted by the function, and will be in the output
  x = c(a, x_n_odd, x_n_even, b)
  #Create a vector y, which will be a vector containing all of the values evaluated by the function, which will be a part of the output
  y = c(f(a), 4*f(x_n_odd), 2*f(x_n_even), f(b))
  #Simpson function
  #(h/3) * (the function of a + 4 * f(every value of odd value of n) + 2*f(every even value of n) + f(b)
  #This will only multiply the inner values by four and two, leaving the end points as they are: f(x_0) + 4f(x_1) + 2f(x_2) + ...2f(x_d-1) + f(x_d)
  #In other words if there are 5 divisions from the numbers 0 to 10 the functions would be f(0) + 4f(2) + 2f(4) + 4f(6) + 2f(8) + f(10)
  Simp <- (h / 3) * (f(a) + (4 * sum(f(x_n_odd))) + (2 * sum(f(x_n_even)))  + f(b))
  #Return a list: result of the Simpson rule, the start/end values, vector of values, and vector of evaluated values
  return(new("Simpson", ab = ab, x = x, y = y, result = Simp))
})
