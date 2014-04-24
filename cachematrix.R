# create a special "matrix" object 
makeCacheMatrix <- function(x = matrix()) {    
        invX <- NULL
        set <- function(y) {                  # set the matrix 
                x <<- y
                invX <<- NULL
        }
        get <- function() x                   # get the matrix 
        setInverse<- function(inverse) invX <<-inverse  # set inverse
        getInverse <- function() invX         # get inverse
        list(set = set, get = get,            # return a list with 4 list items
             setInverse = setInverse,         # matrix
             getInverse = getInverse)
}



# calculage the inverse of the special "matrix" returned by makeCacheMatrix()
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invX <- x$getInverse()
        if (!is.null(invX)) {                # if there is a cache 
                message("getting cached inverse special matrix") # print message
                return(invX)                 # just return it in the cache
        }                                    # (no computation)          
        invX <- solve(x$get(), ...)          # if not, compute it here
        x$setInverse(invX)                   # save the result to x's cache
        return(invX)                         # return the result
        
}


