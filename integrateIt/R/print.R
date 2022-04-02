#' Prints the result of integral spproximation
#'
#' Prints result from the Trapezoid and Simpson Rules' calculation of the integral
#'
#' @param Rule defining the rule to be used:Trapezoid or Simpson - must use quotes around the name of the rule
#' @param a a numeric object of the start value
#' @param b a numeric object of the end value
#' @param d a numeric object of the number of divisions to be made
#' @param f the function
#'
#' @return An object of class Trapezoid or Simpson containing
#'  \item{result}{the approximated result}
#' @author Kimberly Acquilano
#' @note This produces an object of the Trapezoid or Simpson class
#' @examples
#'
#' print(Rule = "Simpson", a = 0, b = 10, d = 5, f = sin)
#' @seealso \code{\link{trapezoid_fun}}, \code{\link{simpson_fun}}
#' @rdname print
#' @aliases print,ANY-method
#' @export
setGeneric(name="print",
           def=function(Rule, a, b, d, f)
           standardGeneric("print")
)

#' @export
setMethod(f="print",
          definition=function(Rule, a, b, d, f){
            if (Rule == "Trapezoid") {
              .result <- getResult(trapezoid_fun(a, b, d, f))
              return(list("Trapezoid", result = .result))
            } else if (Rule == "Simpson") {
              .result <- getResult(simpson_fun(a, b, d, f))
              return(list("Simpson", result = .result))
            }
          })
