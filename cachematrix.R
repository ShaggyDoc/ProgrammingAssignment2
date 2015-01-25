## These functions will take a matrix as input and then return the inverse of that matrix.  They
## will check to see if a result exists in a cache first.  If so, then the cache is retrieved
## rather than the inverse calculated.

## makeCacheMatrix is a function that receives a matrix, calculates its inverse and caches
## the result in the global environment.

makeCacheMatrix <- function(x = matrix()) { # Define the function and specify the input as a matrix
        inv <- NULL # set the value for the inverse matrix to NULL in the local environment.
        set <- function(y) { # Nested function to set matrix that will be solved, and flush the cache of any
                             # previous calculated value
                x <<- y      # 
                inv <<- NULL # flush the cache in the event a new matrix is provided.
        }
        get <- function() x # returns the current matrix
        setinv <- function(solve) inv <<- solve # calculates the inverse of the current matrix and caches
                                                # the value to the global environment (parent environment of
                                                # makeCacheMatrix)
        getinv <- function() inv # Returns the calculated inverse of the current matrix
        list(set = set, get = get, # list the functions.  Note the inverse (inv) is not called.
             setinv = setinv,
             getinv = getinv)
}


## Create a function to check for and return a cached value for the inverse of a matrix.  If there is none, then
## calculate it and save it to the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinv() # check the cache from the makeCacheMatrix function
        if(!is.null(inv)) { # if cache has a value, then return that value
                message("getting cached data")
                return(inv)
        }
        data <- x$get() # If cache is empty, then use the matrix supplied to the makeCacheMatrix function
        inv <- solve(data, ...) # to calculate the inverse of the matrix,
        x$setinv(inv) # and then save it to the cache
        inv # print the new inverse
}
