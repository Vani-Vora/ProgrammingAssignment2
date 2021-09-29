## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                                     # no inverse is calculated yet
        set <- function(y) {                                            # y must be of class matrix()
                x <<- y                                                 # a new matrix is active now
                inv <<- NULL                                            # the inverse hasn't been calculated yet
        }
        get <- function() (x)                                           # returns the active matrix, 'x'
        setInverse <- function(inverse) (inv <<- inverse)               # value passed from cacheSolve
        getInverse <- function() (inv)                                  # returns the inverse value
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse) # list of functions passed to cacheSolve
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'                    
        inv <- x$getInverse()                                           # initialize 'i' from arg func
        if (!is.null(inv)){                                             # return inverse if it's available
                message("Getting cached data")
                return(inv)
        }
        mat <- x$get()                                                  # get data if 'i' hasn't been calculated
        inv <- solve(mat, ...)                                          # calculate inverse of the passed matrix
        x$setInverse(inv)                                               # set 'i' to calculated
        inv                                                             # return answer 
}
