#' Prints the result of integral approximation
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
#' @note This returns the result of the trapezoid_fun or simpson_fun functions by utilizing the getResult method
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
            #takes in the rule that the user wants to use ("Trapezoid" or "Simpson"), and all the values that are arguments for the corresponding rule functions ()
            #It then uses the getResult method to store the result output from the corresponding function
            if (Rule == "Trapezoid") {
              .result <- getResult(trapezoid_fun(a, b, d, f))
              #The return will be a list with the name of the rule used and the corresponding result
              return(list("Trapezoid", result = .result))
            } else if (Rule == "Simpson") {
              .result <- getResult(simpson_fun(a, b, d, f))
              return(list("Simpson", result = .result))
            } else {
              #If the input is not in quotes, misspelled, or is neither of the valid options, return a message
              return("That is not a valid option, please choose 'Trapezoid' or 'Simpson'")
            }
          })
