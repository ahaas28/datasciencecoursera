## The functions compute the inverse of square matrices and 
## caches them to save time if it needs to be done repeatedly.  

## The function "makeCacheMatrix" creates a special "matrix" and 
## caches its inverse.  

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(solve) i <<- solve
        getinv <- function() i
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}



## The function "cacheSolve" computes the inverse of the special
## "matrix" created by the function "makeCacheMatrix". If the 
## inverse of the matrix has already been calculated the result 
## is retrieved from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        i <- x$getinv()
        if (!is.null(i)) {
                message ("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}

