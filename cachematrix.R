## Put comments here that give an overall description of what your
## functions do

## Overall, these two functions work in a way similar to the example in the 
## problem Assignment 2.

## In order to set up the programs, I needed to create a matrix, using the command:
##
##      My_Matrix <- makeCacheMatrix()
##
## Then, I had to store the matrix using the set command . . .
##      (This matrix can be inverted)
##
##      test_Matrix <- matrix(1:4,3,3)
##
##      My_Matrix$set(test_Matrix)
##
##      (Ignore the warning)
##
## Then I call the program to invert the matrix using
##
##      Out_Matrix <- cacheSolve(My_Matrix)
##
## The result is a 3X3 matrix which, when multiplied by the original
## matrix consisting of matrix(1:4,3,3), we get an identity matrix
##       (multiply the two matricies using the %*% command)
##
##      Out_Matrix %*% test_Matrix
##
##


## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse
## I followed the program in the assignment for "makeVector" only replacing the
## references to vectors with matrix and using the solve() function to calculate 
## the inverse function

## This function creates a list to identify the functions to set and get the values
## of the matrix, and to set and get the values of the inverse functions


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                                     ## m will contain the matrix
        set <- function (y) {
                x <<-y
                m <<-NULL
        }        
        get <- function() x
        setMatrix <- function(solve) m<<- solve       ## creates the inverse
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}



## Write a short comment describing this function

## This function computes the inverse of a special "matrix" object returned 
## by the makeCacheMatrix above.  

##      First, it determines if this matrix already has been evaluated for its inverse
##      Then, if it hasn't been inverted, it uses solve() to invert the matrix
##
##  WE ASSUME THAT ANY MATRIX FED TO US IS ABLE TO BE INVERTED.  THERE ARE NO TESTS
##  IN THIS PROGRAM TO ADDRESS MATRICES THAT CANNONT BE INVERTED.
##
##

cacheSolve <- function(x, ...) {
        ## Is there a copy of this matrix already cached?
                m <- x$getMatrix()
        
        ## There are no matrices cached        
                if (!is.null(m)) {
                        message("getting cached data")
                        return(m)
                }
        ## Return matrix m that is the inverse of 'x'
                matrix <- x$get()
                m <- solve(matrix, ...)
                x$setMatrix(m)
                m
}
        
        
        

makeMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setMatrix <- function(inverse) m <<- solve(x)
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix
             getMatrix = getMatrix)
}
