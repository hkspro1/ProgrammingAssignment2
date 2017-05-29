## makeCacheMatrix is a special "matrix" object which can cache its inverse.  
## The makeCacheMatrix assumes that the supplied matrix is always invertible
## This object is a list containing functions to 
## a) Set the value of matrix
## b) Get the value of matrix
## c) Set the value of inverse
## d) Get the value of inverse

## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

## Special "matrix" object as described above

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inverse <<- solve
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by
## makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}

## Test with a 4X4 square matrix

Special_Matrix <- makeCacheMatrix(matrix(rnorm(16), 4, 4))        

Special_Matrix$get()

Special_Matrix$getinverse()

cacheSolve(Special_Matrix)

