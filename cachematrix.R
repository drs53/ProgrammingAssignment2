## This code is designed to help cache the inverse of a matrix
## This is helpful because it stops the program from having to redo 
## timeconsuming calculations

## The makeCacheMatrix function is used to cache the inverse of a matrix
## The saveCache function checks if the inverse has been computed before
## and returns that value, or computes the inverse as needed 

makeCacheMatrix = function(x = matrix()){
        inv <- NULL # initialize inv
        # The set function assigns into the parent environment and
        # clears any previous value of inv
        set <- function(y) {
                x <<- y 
                inv <<- NULL
        }
        get <- function() x #get is now the same value as x (the matrix)
        setsolve <- function(dummy_arg) inv <<- dummy_arg
        getsolve <- function() inv
        # now create a list of the values and assign them names.
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}

# Now to the cacheSolve function
# note that the input to cacheSolve needs to be of the format of the output of 
# makeCacheMatrix (which is a list). Inputting a matrix will not work.

cacheSolve <- function(x, ...) {
        # A new variable (inv.local) checks if something is saved in the cache
        # If there is something cached, collect the cached data
        inv.local <- x$getsolve()
        if(!is.null(inv.local)) {
                message("getting cached data")
                return(inv.local)
        }
        # if there was no cached data (the matrix has been changed)
        # the next portion calculates the inverse and stores it
        data <- x$get()
        inv.calculated <- solve(data, ...)
        # now assign the calculated inverse to the x variable in makeCacheMatrix
        x$setsolve(inv.calculated) 
        # and output the solution
        sol.calculated
}

