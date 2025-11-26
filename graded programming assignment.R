## 1) makeCacheMatrix

## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL   # cached inverse
        
        # set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # get the matrix
        get <- function() x
        
        # set the cached inverse
        setInverse <- function(inverse) inv <<- inverse
        
        # get the cached inverse
        getInverse <- function() inv
        
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## 2) cacheSolve

## Computes the inverse of the matrix returned by makeCacheMatrix.

## If already cached, it retrieves the cached value.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        
        # If inverse already calculated, return cached value
        if (!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
        # Otherwise compute the inverse
        mat <- x$get()
        inv <- solve(mat, ...)
        
        # Cache the result
        x$setInverse(inv)
        
        inv
}


## Example usage

m <- matrix(c(2, 1, 1, 2), nrow = 2)
cm <- makeCacheMatrix(m)

cacheSolve(cm)  # computes inverse
cacheSolve(cm)  # retrieves cached inverse
