## This assignment involves writing two functions that cache the inverse of a matrix

## This function will create a special matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse property
        m <- NULL
        
        ## Method to set the matrix
        set <- function(matrix) {
                x <<- matrix
                m <<- NULL
        }
        
        ## Method to get the matrix
        get <- function() {
        x
        }
        
        ## Method to set the inverse of the matrix
        setInverse <- function(inverse) {
        m <<- inverse
        }
        
        ## Method to get the inverse of the matrix
        getInverse <- function() {
        m
        }
        
        ## Return a list of methods
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special matrix calculated by the method above
## If the inverse has been already calculated then this method will help retrieve it form the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        
        ## If the inverse is already set, just return it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## Get the mztrix from the object 
        data <- x$get()
        
        ## Caculating the inverse of the matrix using MAtrix Multiplication
        m <- solve(data) %*% data
        
        ## Set the inverse of the matrix to the object
        x$setInverse(m)
        
        ## Now return the matrix
        m
}
