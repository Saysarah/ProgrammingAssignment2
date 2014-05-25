## The Two functions below cache the inverse of a matrix.

##makeCacheMatrix() creates a special "matrix" object to cache its inverse. 
makeCacheMatrix <- function(x = matrix()) { 
 
    i <- NULL
    setMatrix <- function(y){   ## set the value of the matrix
        x <<- y
        i <<- NULL
    }
    get <- function() x         ## get the original value of the matrix
    setInverse <- function(solve) i <<- solve ## set the inverse matrix 
    getInverse <- function() i  ## get the value of the inverser matrix
    
    # Group functions within this object and sets datafield
    list(setMatrix = setMatrix, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)   
}


## The CacheSolve() function computes & returns the inverse of the special  
## "matrix" returned by the makeCacheMatrix() above

cacheSolve <- function(x=matrix(), ...) {
   
    i <- x$getInverse() ## calculate the inverse of matrix in makeCacheMatrix() 
    if(!is.null(i)){    ## first checks if inverse matrix already calculated
        message("getting cached inverse")
        return(i)       ## returns the inverse of the "matrix
    }
    matrix <- x$get()   ## Otherwise, if not stored in makeCacheMatrix() 
    i <- solve(matrix, ...)     ## calculates inverse of the matrix
    x$setInverse(i)     ##sets value of inverse matrix in cache via setInverse()
    i
}
    
## ginv() can be used like solve() to find the inverse of a matrix
## Note that use %*% for matrix multiplication 
