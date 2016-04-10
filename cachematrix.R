## Put comments here that give an overall description of what your
## functions do

#Create a special "matrix", which is a list containing
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - setinverse the value of the inverse matrix
##   - getinverse the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
# define the cache m
        cache <- NULL
        
        set <- function(y) {
                x <<- y # assign the input matrix y to the variable x
                cache <<- NULL # re-initialize cache to null
        }
        get <- function()# return the matrix x
        {
                x
        }
        setinverse <- function(inverse)# set the cache m equal to inverse of x
        { 
                m <<- inverse
        } 
        getinverse <- function()  # return the cached inverse of x
        {       
                m
        }
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
}
# The following function calculates the inverse of the special "matrix" created
# with the above function. However, it first checks to see if the inverse
# has already been caclulated. If so, it 'get's the inverse from the cache
# and skips the computation. Otherwise, it calculates the matrix inverse
# and sets the value of the inverse in the cache via the 'setinverse' function.


cacheSolve <- function(x, ...) {
        # Return a matrix which is the inverse of 'x'
        cache <- x$getinverse()
        if(!is.null(cache))
        {
                message("getting cached data")
                return(cache)
        }
        data <- x$get()
        cache <- solve(data, ...)
        x$setinverse(cache)
       # return the inverse
        cache
}
