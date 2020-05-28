#BY: JUAN MANUEL VILLEGAS
#DATE: 28/05/2020

# This file containts two functions: makeCacheMatrix and 
# cacheSolve. The first has the porpose of creating  a special
# matrix "matrix" object that cache its inverse. The second one
# computes the inverse of the special matrix returned by makeCacheMatrix.
# If the inverse has already been caculated, then the cacheSolve function
# just retrieve the inverse from the cache.

#########################################################################
# Function: makeCacheMatrix
#   
# Description: Store the matrix x an its inverse in the cache.
#
# Input: A matrix x
#
# Output: A special "matrix", which is a list with all the functions
# necessary to retrieve the matrix and its inverse.
##########################################################################

makeCacheMatrix <- function(x = matrix()) {
    
    I <- NULL
    
    # Everytime that makeCacheMatrix is called, the inverse matrix I is
    # set to NULL
    
    set <- function(B)
    {
        x <<- B
        I <<- NULL
    }
    
    get <- function()
    {
        x
    }
    
    setInverse <- function(y)
    {
        I <<- y
    }
    
    getInverse <- function()
    {
        I
    }
    
    list(get = get, set = set,
         setInverse = setInverse,
         getInverse = getInverse)
}


#########################################################################
# Function: cacheSolve
#
# Description: Returns the inverse of the special "matrix" x. If the
# inverse has already been computed, it just returns it from the cache, 
# otherwise the inverse is computed and stored in the cache. 
#
# Input: A special "matrix" x (A list returnded by the makeCacheMatrix
# function)
#
# Output: matrix. The inverse of the special "matrix" x
#########################################################################

cacheSolve <- function(x, ...) {
    
    I <- x$getInverse()
    
    #Check if the inverse is in the cache
    if(!is.null(I))  
    {
        #If it is, just return it
        return(I)   
    }
    
    #Otherwise, compute it, store it in the cache and return it
    
    A <- x$get()
    I <- solve(A)
    x$setInverse(I)
    
    I
}
