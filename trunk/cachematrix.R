## The functions in this script provide a way to optimize computations that requires
# inverse of a given matrix. Instead of computing the inverse of the given matrix
# every time it is required, this inverse can be cached and hence recycled, saving 
# computation time and speeding up operations that require the inverse of the given
# matrix as an input.

## The input of 'makeCacheMatrix' function is a squared matrix. It returns a list containing four
# functions.
# 1) The first function ('set') sets the value of the matrix in an environment that is different
# from the current environment (namely, a global variable).
# 2) The second function ('get') returns the value of the matrix
# 3) The third function ('setInverse') allows to set the value of the inverse. 
# In facto, this function is called by the cacheSolve function below to accomplish
# that task.
# 4) The fourth ('getInverse') function gets the inverse. Indeed, it is called by
# the cache solve function to get the inverse.

makeCacheMatrix <- function(x) {
        I <- NULL
        set <- function(y) {
                x <<- y
                I <<- NULL
        }
        get <- function() x
        setInverse <- function(stuff) I <<- stuff
        getInverse <- function() I
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}


## The input of the 'cacheSolve' matrix is the object (or list) returned by the 
# 'makeCacheMatrix' function. It uses the 'getInverse' function to get the inverse 
# of the given matrix. If the inverse has been already computed, 'cacheSolve'
# returns the matrix --this is checked through the logical statement !is.null(I)--. 
# If the inverse has not been computed, then it calls the 'get' function to get
# the given matrix, computes its inverse, and through the 'setInverse' function 
# sets the inverse of the given matrix. 
#  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        I <- x$getInverse()
        if(!is.null(I)) {
                message("getting cached data")
                return(I)
        }
	  ## If the inverse of 'x' is not found, cacheSolve computes
	  # and sets the inverse
        data <- x$get()
        I <- solve(data, ...)
        x$setInverse(I)
        I

}

## Here is an expample to illustrate the efficiency gains from the caching procedures
# implemented by the previous functions

##Here we create a large random matrix
set.seed(1234567)
n<-3000
mat<-matrix(runif(n^2),n)
#smat<-solve(mat)

## Then we use cacheSolve to calculate its inverse. We use 'system.time' to
# get the time consumed in this first run of the 'cacheSolve' function.
x<-makeCacheMatrix(mat)
system.time(cacheSolve(x))

## However, after the first run, the 'cacheSolve' function runs much, much faster.
# This is because this time it does not compute the inverse of the matrix, but 
# retrieves it.

system.time(cacheSolve(x))
#cacheSolve(x)



