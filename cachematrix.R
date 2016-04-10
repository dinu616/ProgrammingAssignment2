## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
# define the cache m
        m <- NULL
        set <- function(y) {
                x <<- y # assign the input matrix y to the variable x
                m <<- NULL # re-initialize m to null
        }
        get <- function() x # return the matrix x
        setinverse <- function(inverse) m <<- inverse # set the cache m equal to inverse of x
        getinverse <- function() m # return the cached inverse of x
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
}
# The following function calculates the inverse of the special "matrix" created
# with the above function. However, it first checks to see if the inverse
# has already been caclulated. If so, it 'get's the inverse from the cache
# and skips the computation. Otherwise, it calculates the matrix inverse
# and sets the value of the inverse in the cache via the 'setinverse' function.


cacheSolve <- function(x, ...) {
        # Return a matrix which is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
