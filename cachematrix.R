## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function makeCacheMatrix creates a special vector, whose elements are functions to accompish the following tasks:
##  1. Set the value of the matrix
##  2. Get the value of the matrix 
##  3. Set inverse of the matrix 
##  4. Get inverse of the matrix

## The '<<-' operator is used in the 'setMatInv' function to load the inverse of the matrix to the cache. 
## Example:
##  m1 <- makeCacheMatrix(x)
##      where x is a matrix. 

makeCacheMatrix <- function(x = matrix()) {
    
    inv_m <- NULL # Initialize placeholder for inverse of matrix
    # Function to read input matrix
    set <- function(y) { 
        x <<- y  # Assign matrix
        inv_m <<- NULL  # inv_m - inverse matrix
    }
    get <- function() x # get matrix input
    setMatInv <- function(matInv) inv_m <<- matInv # assign inverse of the matrix
    getMatInv <- function() inv_m  # get inverse of the matrix from the cache
    list(set = set, get = get,
         setMatInv = setMatInv,
         getMatInv = getMatInv) 
}



## Write a short comment describing this function
## cacheSolve function below gets the inverse of the matrix from the cache and checks if it is NULL. If 
## inverse of the matrix is available in the cache it is reads the value and assigns it to the inv_m. If 
## the inv_m is NULL, it will get the matrix and calculates the inverse matrix using the solve() function. 
## The input to the cacheSolve is the output from the makeCacheMatrix. 
## Example:
##  matInv <- cacheSolve(m1)

cacheSolve <- function(m1, ...) {
    ## Return a matrix that is the inverse of 'm1', m1 is vector, output from makeCacheMatrix. 
    inv_m <- m1$getMatInv()
    if(!is.null(inv_m)) {
        message("getting cached data")
        return(inv_m)
    }
    data <- m1$get()
    inv_m <- solve(data)
    m1$setMatInv(inv_m)
    inv_m
}
