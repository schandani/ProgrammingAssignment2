########################################################################################
## makeCacheMatrix is a function that creates a special
## "matrix" which is a list containing a function to
## set: set the value of the matrix
## get: get the value of the vector
## setsolve:  set the value of the inverse of the matrix in cache
## getsolve:  get the value of the inverse of the matrix from cache
##
## x is the input matrix to the function and it is assumed that it is a square matrix.
## No explicit evaluation is occurring to ensure that it is a square matrix or not.
## If the input matrix is not provided, it is defaulted to an empty matrix
## Here's an example usage:
## First create a 2x2 matrix
##> m<-matrix(c(3,-7,5,2),2,2)
##> m
##[,1] [,2]
##[1,]    3    5
##[2,]   -7    2
## 
##> n<-makeCacheMatrix(m)
##
##> n
##$set
##function (y) 
##{
##    x <<- y
##    m <<- NULL
##}
## <environment: 0x00000000084e54c0>
##    
##    $get
## function () 
##    x
## <environment: 0x00000000084e54c0>
##    
##    $setsolve
## function (solve) 
##    m <<- solve
## <environment: 0x00000000084e54c0>
##    
##    $getsolve
## function () 
##    m
## <environment: 0x00000000084e54c0>
################################################################################
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
            x <<- y
            m <<- NULL
        }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, 
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


###########################################################################################
## cachesolve takes the special "matrix" returned by `makeCacheMatrix`  
## (the output of makeCacheMatrix) as input.  It looks
## for whether the inverse of the matrix has been calculated before and the 
## result cached, and if so, returns the inverse matrix from cached data.
## 
## If the x$getsolve() is null (that is the inverse has not been calculated and cached before)
## then the function evaluates the inverse and caches the result and returns the inverse matrix
## to the caller of the function
##
## Here's an example usage:
##> cacheSolve(n)
##[,1]        [,2]
##[1,] 0.04878049 -0.12195122
##[2,] 0.17073171  0.07317073
##
## NOTE ABOVE THAT THE INVERSE WASN'T RETURNED FROM THE CACHE SINCE IT WAS CALLED THE VERY
## FIRST TIME.  
## Now let's call the function again:
##
##> cacheSolve(n)
##getting cached data
##[,1]        [,2]
##[1,] 0.04878049 -0.12195122
##[2,] 0.17073171  0.07317073
## NOTE THAT WHEN cacheSolve WAS CALLED 2ND TIME THE INVERSE WAS RETURNED FROM THE CACHED DATA
##
## Extra: The function will not error out if argument 'x' is not passed.  Instead it will start
## with an identity matrix and return it's inverse.
## You can test that by calling cacheSolve() without passing any argument.
## 
## If an invalid argument is passed to the function, it will error out. No other
## checks have been added to the function to avoid such errors
##
###########################################################################################
cacheSolve <- function(x, ...) {
        if (missing(x)){   ## if no x is provided as input, create a 2x2 identity matrix 
            message("no special matrix provided as input, using a 2x2 identity matrix...")
            y<-matrix(c(1,0,0,1),2,2)
            x<-makeCacheMatrix(y)
        }
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)   ## cache the inverse matrix
        m

}
