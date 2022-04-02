#' Gets the result of integral approximation
#'
#' Gets the result from the Trapezoid and Simpson Rules' calculation of the integral
#'
#' @param object An object of class Simpson or Trapezoid
#'
#' @return An object of class Trapezoid or Simpson containing
#' \item{result}{the approximated result}
#'
#' @author Kimberly Acquilano
#' @note This method is used by the integrateIt and print methods
#' @seealso \code{\link{trapezoid_fun}}, \code{\link{simpson_fun}}, \code{\link{integrateIt}}, \code{\link{print}}
#' @aliases getResult,ANY-method
#' @rdname getResult
#'
#' @export
setGeneric("getResult",
           function(object)
             standardGeneric("getResult")
)

#' @export
setMethod("getResult",
          definition = function(object) {
            #this function allows the getResult method to take only the result output of the applicable class
            #in this case getResult will take the result output from objects that are of class Trapezoid and Simpson
            #essentially this method will allow the result from the simpson_fun and trapezoid_fun functions to
            #feed into the integrateIt and print methods
            return(object@result)
          })
