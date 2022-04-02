#' Gets the vector of evaluated values (y) under the integral
#'
#' Gets the vector of evaluated values (y) under the curve for the Trapezoid and Simpson Rules' calculation of the integral
#'
#' @param object An object of class Simpson or Trapezoid
#'
#' @return An object of class Trapezoid or Simpson containing
#' \item{y}{the vector of evaluated values}
#'
#' @author Kimberly Acquilano
#' @note This method is used by the integrateIt and print methods
#' @seealso \code{\link{trapezoid_fun}}, \code{\link{simpson_fun}}, \code{\link{integrateIt}}, \code{\link{print}}
#' @aliases getY,ANY-method
#' @rdname getY
#'
#' @export
setGeneric("getY",
           function(object)
             standardGeneric("getY")
)

#' @export
setMethod("getY",
          definition = function(object) {
            #this function allows the getY method to take only the y output of the applicable class
            #in this case getY will take the y output from objects that are of class Trapezoid and Simpson
            #essentially this method will allow the y values from the simpson_fun and trapezoid_fun functions to
            #feed into the integrateIt method
            return(object@y)
          })
