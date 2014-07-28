## The following pair of functions cache the inverse of a given matrix.

## makeCacheMatrix creates a special "matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL ##initialize the inverse
        
        ## set the values of the matrix
        set <- function(y = matrix()){  
                x <<- y 
                i <<- NULL
                
       }
       
       ## get the matrix
       get <- function() x
       
       ## set the inverse of the matrix
       setinv <- function(inverse) i <<- inverse
       
       ## get the inverse of the matrix
       getinv <- function() i
       
       ## returns a special matrix object, which is a list containing the functions set, get, setinv, and getinv
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}

## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve function
## retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    
        i <- x$getinv() ## get the inverse of the matrix "x"
        if(!is.null(i)) {
                message("getting cached data")
                return(i) ## if the inverse has already been calculated, return the inverse from the cache 
        }
        
        mat <- x$get() ## get the matrix from the cache
        i <- solve(mat) ## calculate the inverse 
        x$setinv(i) ## set the inverse in the cache
        
        i  ## return the inverse
        
        
}
