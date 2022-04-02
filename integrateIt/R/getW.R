#' Gets the vector of values (x) under the integral
#'
#' Gets the vector of values (x) under the curve for the Trapezoid and Simpson Rules' calculation of the integral
#'
#' @param object An object of class Simpson or Trapezoid
#'
#' @return An object of class Trapezoid or Simpson containing
#' \item{x}{the vector of values}
#'
#' @author Kimberly Acquilano
#' @note This method is used by the integrateIt and print methods.
#' @note This method is called getW, but it is actually getting the x value
#' @seealso \code{\link{trapezoid_fun}}, \code{\link{simpson_fun}}, \code{\link{integrateIt}}, \code{\link{print}}
#' @aliases getW,ANY-method
#' @rdname getW
#'
#' @export
setGeneric("getW",
           function(object)
             standardGeneric("getW")
)

#' @export
setMethod("getW",
          definition = function(object) {
            #this function allows the getW method to take only the x output (which is the vector values that will be evaluated by the integral) of the applicable class
            #in this case getW will take the x output from objects that are of class Trapezoid and Simpson
            #essentially this method will allow the x value from the simpson_fun and trapezoid_fun functions to
            #feed into the integrateIt method
            #while it is called getW, there is no w value, it is retrieving the x value
            return(object@x)
          })



