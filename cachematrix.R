## This function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix(),) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## 	This function computes the inverse of the special "matrix"
##	returned by makeCacheMatrix
##  In case the calculation has been done and the matrix is 
##  unchanged it uses data from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
                m <- getinverse(x)
        
        ## Checks wether the cache is full or empty
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- getinverse(x)
        m <- solve(data, ...)
        x["setinverse(m)"]
        m
}



