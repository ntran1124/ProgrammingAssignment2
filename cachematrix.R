## The function makeCacheMatrix creates a special "matrix" object, 
## which is really a list containing a function to:
##      1. set the matrix
##      2. get the matrix
##      3. set the inverse of the matrix
##      4. get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(i) inverse <<- i
        getInverse <- function() inverse
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The function cacheSovle computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has been calculated already (and the matrix has not changed),
## then the cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setInverse(inverse)
        inverse
}

## The function testCachingInverseOfMatrix is used for testing the effectiveness of
## the function cacheSolve above. It will call cacheSolve twice and print out the running time
## of each run. Depending on how big the input matrix is, it will take some time to finish
## the first call. At the second call, it will be very fast (in a matter of milliseconds) because it just needs to get
## the inverse of the matrix from the cache.
## Using the following code will take about 10 seconds to complete on a system powered by CPU i5-2520M @2.5GHz.
##      set.seed(1124)
##      r = rnorm(4000000)
##      matrix1 = matrix(r, 2000, 2000)
##      testCachingInverseOfMatrix(matrix1)

testCachingInverseOfMatrix = function(m){
        ## input m has to be an invertible matrix
        
        tmpInverse = makeCacheMatrix(m)
        for (i in 1:2) {
                start.time = Sys.time()
                cacheSolve(tmpInverse)
                end.time = Sys.time()
                runningTime = end.time - start.time
                print(runningTime)
        }
}
