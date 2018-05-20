## Put comments here that give an overall description of what your
## functions do

# makeCacheMatrix is a function that returns a list of functions
# Its puspose is to store a martix and a cached value of the inverse of the matrix
# 

makeCacheMatrix <- function(x = matrix()) { 
        cache <- NULL 
        setMatrix <- function(value) { 
                x <<- value
                cache <<- NULL
        }  
        
        setInverse <- function(value) {
                cache <<- value
        }
        
        getCache <- function() { 
                cache
        }
        
        getMatrix <- function() {
                x
        }
        
        list(setMatrix = setMatrix, setInverse = setInverse, getMatrix = getMatrix, getCache = getCache) 
} 
## The following function calculates the inverse of a "special" matrix created with makeCacheMatrix


cacheSolve <- function(x, ...) { 
        inverse <- x$getCache()
        if(!is.null(inverse)) {
                message("Inverse exist")
                inverse <- x$getCache() 
        } 
        else {
                message("calculating the inverse")
                matx <- x$getMatrix()
                inverse <- solve(matx)
                x$setInverse(inverse)
        }
        return(inverse)
}
