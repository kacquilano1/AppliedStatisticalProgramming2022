#Create a function for the Simpson Rule
#Function will take in a starting and ending values, "a" and "b"
#It will also take in "d" number of divisions
#And the function "f" that will be integrated
#Based off of the Trapezoid rule from https://www.r-bloggers.com/2017/08/the-trapezoidal-rule-of-numerical-integration-in-r/
Simpson <- function(start = a, end = b, divisions = d, f) {
  #Define our h:
  h <- (b - a) / d
  #n is a vector of the number of divisions between a and b
  n <- 1 : (d - 1)
  #Create a vector of  odd n values
  #This is selecting the odd elements from within n through subsetting
  n_odd <- n[ (n %% 2) != 0]
  #Create a vector of even n values
  #This is selecting the even elements from within n through subsetting
  n_even <- n[ (n %% 2) == 0]
  #Create  x values for for the odd n and even n
  #These vectors will allow for only the inner odd values to be multiplied by 4, and the inner even values to be multiplied by 2
  x_n_odd <- a + (n_odd * h)
  x_n_even <- a + (n_even *h)
  #Simpson function
  #(h/3) * (the function of a + 4 * f(every value of odd value of n) + 2*f(every even value of n) + f(b)
  #This will only multiply the inner values by four and two, leaving the end points as they are: f(x_0) + 4f(x_1) + 2f(x_2) + ...2f(x_d-1) + f(x_d)
  #In other words if there are 5 divisions from the numbers 0 to 10 the functions would be f(0) + 4f(2) + 2f(4) + 4f(6) + 2f(8) + f(10)
  Simp <- (h / 3) * (f(a) + (4 * sum(f(x_n_odd))) + (2 * sum(f(x_n_even)))  + f(b))
  #Return the value from the Simpson Rule
  return(Simp)
}
