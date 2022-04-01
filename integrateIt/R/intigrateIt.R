#' Approximates the integral
#'
#' Uses the Trapezoid and Simpson Rules to calculate the integral
#'
#' @param Rule defining the rule to be used:Trapezoid or Simpson
#' @param a a numeric object of the start value
#' @param b a numeric object of the end value
#' @param d a numeric object of the number of divisions to be made
#' @param f the function
#'
#' @return An object of class Trapezoid or Simpson containing
#'  \item{w}{a vector of values under the integral}
#'  \item{y}{a vector of evaluated values from w}
#'  \item{result}{approximated result}
#' @author Kimberly Acquilano
#' @note This produces an object of the Trapezoid or Simpson class
#' @examples
#'
#' integrateIt(Rule = Simpson, a = 0, b = 10, d = 5, f = sin)
#' @seealso \code{\link{trapezoid_fun}}, \code{\link{simpson_fun}}
#' @rdname integrateIt
#' @aliases integrateIt,ANY-method
#' @export
setGeneric(name="integrateIt",
           def=function(Rule, a, b, d, f, ...)
           {standardGeneric("integrateIt")}
)

#' @export
setMethod(f="integrateit",
          definition=function(Rule, a, b, d, f, ...){
            if (Rule == "Trapezoid") {
              .result <- getResult(trapezoid_fun(a, b, d, f))
              .w <- getW(trapezoid_fun(a, b, d, f))
              .y <- getY(trapezoid_fun(a, b, d, f))
              return(new("Trapezoid", result = .result, w = .w, y = .y))
            } else if (Rule == "Simspon") {
              .result <- getResult(simpson_fun(a, b, d, f))
              .w <- getW(simpson_fun(a, b, d, f))
              .y <- getY(simpson_fun(a, b, d, f))
              return(new("Simpson", result = .result, w = .w, y = .y))
            }
          })
