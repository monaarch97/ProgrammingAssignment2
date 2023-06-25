## There are two functions: makeCasheMatrix and cashesolve
## makeCasheMatrix will be used to creates a matrix object that can cache its inverse.
## casheSolve computes the inverse of the matrix returned by makeCacheMatrix.


## This function consists of set,get,setInverse,getInverse
makeCacheMatrix <- function( x = matrix() ) {
        
        i <- NULL
        
        
        set <- function( matrix ) {
                x <<- matrix
                ## i <<- NULL
        }
        
        get <- function() {
               x
        }
        
        setInverse <- function(inverse) {
                i <<- inverse
        }
        
        ## The function to calculate the inverse (if the matrix IS square and invertible)
        getInverse <- function() {
                i <- solve(x)
                i
        }
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function is used to get the cache data
cacheSolve <- function(x, ...) {
        
        i <- x$getInverse()
        
        ## inverse returned from cache
        if( !is.null(i) ) {
                message("getting cached data")
                return(i)
        }
        
        cacheData <- x$get()
        
        i <- solve(cacheData) %*% cacheData
        
        x$setInverse(i)
        
        i
}
