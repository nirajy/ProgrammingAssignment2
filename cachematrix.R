## The function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set(y) - set the value of the matrix
## get() - get the value of the matrix
## setinverse(inverse) the value of the inverse
## getinverse() the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## Set X to input matrix and set inverse to NULL as input matix got changed
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function cacheSolve  calculates inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of matrix and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        ## Check if Inverse value is there in cache
        if(!is.null(i)) {
                message("Getting Inverse from cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
