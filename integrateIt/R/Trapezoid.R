#Create a function for the Trapezoid Rule
#Function will take in a starting and ending values, "a" and "b"
#It will also take in "d" number of divisions
#And the function "f" that will be integrated
#Followed Trapezoid rule from https://www.r-bloggers.com/2017/08/the-trapezoidal-rule-of-numerical-integration-in-r/
Trapezoid <- function(start = a, end = b, divisions = d, f) {
  #Define our h:
   h <- (b - a) / d
   #n is a vector of the number of divisions between a and b
  n <- 1 : (d - 1)
  #x_n is a vector of the values for each division between a and b
  #Example if a=0 and b=10 and d=5, x_n will be the values 2,4,6,8
  x_n <- a + (n * h)
  #Trapezoid function
  #(h/2) * (the function of a + 2 * f(every value of x_n) + f(b)
  #This will only multiply the inner values by two, leaving the end points as they are
  Trap <- (h / 2) * (f(a) + (2 * sum(f(x_n)))  + f(b))
  #Return the value from the Trapezoid Rule
  return(Trap)
}
