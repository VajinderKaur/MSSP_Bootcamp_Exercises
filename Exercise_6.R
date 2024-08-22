# Create a function to calculate the area of a circle. Test the function by finding the area of a circle with a
# diameter of 3.4 cm. Can you use it on a vector of data?

area <- function(x){r= x/2 ; return(pi*(r*r))}
area(3.4)
area(c(3,4,5,6))  #-----Yes! it can be used on vector of data

# Write a function to convert farenheit to centegrade (oC = (oF - 32) x 5/9). Get your function to print
# out your result in the following format: “Farenheit : value of oF is equivalent to value oC centigrade.”

farneheit <- function(x){ centegrade = (x-32)*(5/2);  cat("Farenheit : value of ",x," is equivalent to value ",centegrade," centigrade.")}
farneheit(123)

# Create a vector of normally distributed data, of length 100, mean 35 and standard deviation of 15. Write
# a function to calculate the mean, median, and range of the vector, print these values out with appropriate
# labels. Also get the function to plot a histogram (as a proportion) of the values and add a density curve.

length_1 <- 100
mean_1 <- 35
sd_1 <- 15
norm_dist <- rnorm(length_1, mean_1, sd_1)
norm_dist
mean_f <- function(x){cat("Mean: ",mean(x), "\n")}
median_f <- function(x){cat("Median: ", median(x),"\n")}
range_f <- function(x){cat("Range:", range(x),"\n")}


# Write a function to calculate the median value of a vector of numbers (yes I know there’s a median() 
#function already but this is fun!). Be careful with vectors of an even sample size, as you will have to take
# the average of the two central numbers (hint: use modulo %%2 to determine whether the vector is an odd or an even size). 
#Test your function on vectors with both odd and even sample sizes


median_f1 <- function(x){size = length(x);  avar=ifelse(size%%2==0,(x[size/2] + x[(size/2) +1])/2, x[ceiling(size/2)]) ; cat("Median is: ",avar)}
median_f1(c(1,2,3,4,5,6,7))

# You are a population ecologist for the day and wish to investigate the properties of the Ricker model.
# The Ricker model is defined as:
# Where Nt is the population size at time t, r is the population growth rate and K is the carrying
# capacity. Write a function to simulate this model so you can conveniently determine the effect of changing
# r and the initial population size N0. K is often set to 100 by default, but you want the option of being able
# to change this with your function. So, you will need a function with the following arguments; nzero which
# sets the initial population size, r which will determine the population growth rate, time which sets how long
# the simulation will run for and K which we will initially set to 100 by default.

ricker <- function(nzero,r,t,K=100){
  for (i in 0:t) { n_t = nzero*(exp(r*(1-(nzero/K)))); 
nzero = n_t;
} 
  cat("Population size at", t, " is:", n_t)
}

ricker(100,2,3,10)


