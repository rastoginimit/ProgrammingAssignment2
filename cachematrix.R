## Create a special matrix from the atomic matrix and derive the inverse for that matrix
## The calculation of the inverse uses caching

## Create a special matrix from an atomic matrix
## The special matrix is essentially a list with 4 functions - set, get, setinverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
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

## Finds the inverse of a matrix from the cache
## If inverse is not found in the cache, Uses Solve function to calculate the inverse and caches it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        mat <- x$get()
        i <- solve(mat, ...)
        x$setinverse(i)
        i
}
