##  This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        ## set 'inv' to NULL as a placeholder	
        inv <- NULL
	
        ## define a function to set matrix x to new matrix y, and reset inverted(inv) to NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }

        ## get source matrix
        get <- function() x     

        ## sets
        setinv <- function(solve) inv <<- solve

        ## returns the inverse	
        getinv <- function() inv    

        ## Returns a 'special' vector containing all the functions defined
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        inv <- x$getinv()

        ## if inverse has been calculated then return from cache
        if(!is.null(inv)) {
                message("getting cached matrix data")
                return(inv)
        }

        ## if it has not been calculated then calculate and return	
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
