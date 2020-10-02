#Homework 2
# Jesse Lieberman
# Prof. Xavi Business Analytics
# October 4, 2020

#clearing the global environment
ls()
rm(list=ls())

#QUESTION 1 Pythagorean Theorem Function

#creating a function is_pythagorean with 3 input variables (a , b , c)
# 

is_pythagorean = function(a, b, c){
  if(a*a + b*b == c*c){
    return(TRUE)
  }
  else{
    return(FALSE)
  }
}

#Asking the User for input for all 3 variables

user_a  = as.integer(readline(prompt = "Number a: "))
user_b  = as.integer(readline(prompt = "Number b: "))
user_c  = as.integer(readline(prompt = "Number c: "))

is_pythagorean(user_a, user_b, user_c)
# The following function call in line 31 with inputs 3, 4, 5 will work. This is 
# just in case the user inputs implemented in line 28 don't work
is_pythagorean(3, 4, 5)


# QUESTION 2 LOOPS

#creating is_prime function to see if a number is prime
is_prime = function(N){
  
  #The lowest prime number is 2, so there cannot be a prime number lower than 2
  if(N <= 1){
    return(FALSE)
  }
  
  # 2 is the first prime number
  if(N == 2){
    return(TRUE)
  }
  
  
  #If N>1, checking to see if it is prime using a for loop
  for(i in 2:(N-1)){
    if(N %% i == 0){
      return(FALSE)
    }
  }
  return(TRUE)
}

#now creating a for-loop to print all prime numbers from 1000:100

for(i in 1000:100){
  if((is_prime(i)) == TRUE){
    print(i)
  }
}


#Question 3 Xavi's Blends


#creating a matrix of the prices
prices = matrix(data = c(5, 45, 10), ncol = 1, nrow = 3)

#setting the row and column names of the matrix 
row.names(prices) = c("Wine", "Vodka", "Lemon Juice")
colnames(prices)  = c("Price")

#creating a function that finds the price of a blend
price_of_blend = function(wine, vodka, lemon){
  total_price = (prices["Wine", 1] *wine) + (prices["Vodka",1]*vodka) + 
    (prices["Lemon Juice",1]*lemon)
}

#creating a matrix of Xavi's blends
xavi_blends = matrix(data = c(20,30,30,30, 20,30,50,60,32), nrow = 3, ncol = 3)

#Setting the names for the rows and columns of the Matrix
rownames(xavi_blends) = c("A", "B", "C")
colnames(xavi_blends) = c("Wine", "Vodka", "Lemon Juice")

#Question 3A

#price of Blend A
blend_a = price_of_blend(xavi_blends["A", 1], xavi_blends["A",2], 
                         xavi_blends["A",3])
print(paste("Price of the blend: ", blend_a))

#price of Blend B
blend_b = price_of_blend(xavi_blends["B",1], xavi_blends["B",2], 
                         xavi_blends["B",3])
print(paste("Price of the blend: ", blend_b))

#price of Blend C
blend_c = price_of_blend(xavi_blends["C", 1], xavi_blends["C",2], 
                        xavi_blends["C",3])
print(paste("Price of the blend: ", blend_c))


#Question 3B

entire_price = (10*blend_a) + (4*blend_b) + (5*blend_c)
print(paste("The cost is:",entire_price))






