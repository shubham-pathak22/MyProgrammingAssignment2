## These functions creates a special "matrix" object that can cache its inverse
## makeCacheMatrix returns a list containing functions to set/get matrix and its inverse
## cacheSolve returns a inverse of matrix from cache if already computed, else computes the inverse , sets it in the cache and returns it.


## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the matrix inverse
## get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        mInverse <- NULL
        set <- function(y) {
                x <<- y
                mInverse <<- NULL
        }
        get <- function() x
        setMatrixInverse <- function(matrixInverse) mInverse <<- matrixInverse
        getMatrixInverse <- function() mInverse
        list(set = set, get = get,
             setMatrixInverse = setMatrixInverse,
             getMatrixInverse = getMatrixInverse)
}


## The following function calculates the inverse of the special "matrix" created with the above function. However, it first checks to see if the inverse has
## already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setMatrixInverse function.

cacheSolve <- function(x, ...) {
        m <- x$getMatrixInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(x)
        x$setMatrixInverse(m)
        m
}

