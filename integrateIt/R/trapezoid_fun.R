#' Numerical Integration Using the Trapezoid Rule
#'
#' @param a The start value
#' @param b The end value
#' @param d The number of divisions
#' @param f The function that will be used for integration
#'
#' @return An object of the Trapezoid class, which has
#' \item{ab}{vector of starting and ending values}
#' \item{x}{vector internal values}
#' \item{y}{vector evaluated values values}
#' \item{result}{Trapezoid Rule result}
#'
#' @examples
#' #' trapezoid_fun(a = 0, b = 10, d = 5, f = sin)
#'
#' @seealso simpson_fun
#' @rdname trapezoid_fun
#' @include trapezoid_fun.R
#' @aliases trapezoid_fun,ANY-method
#' @export
setGeneric(name= "trapezoid_fun",
           def = function(a, b, d, f)
           {standardGeneric("trapezoid_fun")})

#' @export
setMethod(f = "trapezoid_fun",
          definition =  function(a, b, d, f) {
  #Followed Trapezoid rule from https://www.r-bloggers.com/2017/08/the-trapezoidal-rule-of-numerical-integration-in-r/
  #Create a vector of start and end values, which will be used for the function's output
  ab <- c(a, b)
  #Define our h:
  h <- (b - a) / d
  #n is a vector of the number of divisions between a and b
  n <- 1 : (d - 1)
  #x_n is a vector of the values for each division between a and b
  #Example if a=0 and b=10 and d=5, x_n will be the values 2,4,6,8
  x_n <- a + (n * h)
  #Create a vector x, which will be a vector containing all of the values to be evaluted by the function, and will be in the output
  x = c(a, x_n, b)
  #Create a vector y, which will be a vector containing all of the values evaluated by the function, which will be a part of the output
  y = c(f(a), 2*f(x_n), f(b))
  #Trapezoid function
  #(h/2) * (the function of a + 2 * f(every value of x_n) + f(b)
  #This will only multiply the inner values by two, leaving the end points as they are
  Trap <- (h / 2) * (f(a) + (2 * sum(f(x_n)))  + f(b))
  #Return a list: result of the Trapezoid rule, the start/end values, vector of values, and vector of evaluated values
  return(new("Trapezoid", ab = ab, x = x, y = y, result = Trap))
  })
