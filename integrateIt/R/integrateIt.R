#' Approximates the integral
#'
#' Uses the Trapezoid and Simpson Rules to calculate the integral
#'
#' @param Rule defining the rule to be used:Trapezoid or Simpson - must use quotes around rule name
#' @param a a numeric object of the start value
#' @param b a numeric object of the end value
#' @param d a numeric object of the number of divisions to be made
#' @param f the function
#'
#' @return An object of class Trapezoid or Simpson containing
#'  \item{result}{the approximated result}
#'  \item{x}{a vector of values under the integral}
#'  \item{y}{a vector of evaluated values from x}
#'
#' @author Kimberly Acquilano
#' @note This returns outputs from the trapezoid_fun or simpson_fun functions through the get... methods
#' @examples
#'
#' integrateIt(Rule = "Simpson", a = 0, b = 10, d = 5, f = sin)
#' @seealso \code{\link{trapezoid_fun}}, \code{\link{simpson_fun}}
#' @rdname integrateIt
#' @aliases integrateIt,ANY-method
#' @export
setGeneric(name="integrateIt",
           def=function(Rule, a, b, d, f)
           standardGeneric("integrateIt")
)

#' @export
setMethod(f="integrateIt",
          definition=function(Rule, a, b, d, f){
            #takes in the rule that the user wants to use ("Trapezoid" or "Simpson"), and all the values that are arguments for the corresponding rule functions ()
            #It then uses the getResult method to store the result output from the corresponding function
            #It uses getW to store the x vector of values from the corresponding function
            #And it uses getY to store the y vector of values from the corresponding function
            if (Rule == "Trapezoid") {
              #If the user types "Trapezoid" this rule will feed the arguments into the trapezoid_fun function
              #then use the get... methods to get the desired outputs
              .result <- getResult(trapezoid_fun(a, b, d, f))
              .x <- getW(trapezoid_fun(a, b, d, f))
              .y <- getY(trapezoid_fun(a, b, d, f))
              #The return will be a list with the name of the rule used and the corresponding result, x, and y
              return(list("Trapezoid Rule", result = .result, x = .x, y = .y))
            } else if (Rule == "Simpson") {
              #If the user type "Simpson" use the process described above, using the simpson_fun function
              .result <- getResult(simpson_fun(a, b, d, f))
              .x <- getW(simpson_fun(a, b, d, f))
              .y <- getY(simpson_fun(a, b, d, f))
              return(list("Simpson Rule", result = .result, x = .x, y = .y))
            } else {
              #If the input is not in quotes, misspelled, or is neither of the valid options, return a message
              return("That is not a valid option, please choose 'Trapezoid' or 'Simpson', \n
                     be sure the rule name is in quotes")
            }
          })
