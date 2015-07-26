## This function creates a special "matrix" object that can cache its inverse, get and set 
#it's values

makeCacheMatrix <- function(x = matrix()) { #my function argument name is x, a matrix
    m <- NULL
    # the next set of functions provide means to set values to the matrix, access to it, calculate it's
    # inverse
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setmatrix <- function(solve) m<<- solve
    getmatrix <- function() m
    # this code provides a list of the functions applied to the x matrix
    list(set=set, get=get,
         setmatrix=setmatrix,
         getmatrix=getmatrix)
}

## This function retrieves the inverse of the stored matrix in the makeCacheMatrix function

cacheSolve <- function(x=matrix(), ...) { #my function argument name is x, a matrix
    #this code calls the stored inverse of the matrix, if it hasn't been calculated.
    # if it hasn't been calculated, it does the calculation
    m <- x$getmatrix()
    if(!is.null(m)){
        message("getting cached data")
        return(m)
    }
    matrix <- x$get()
    m <- solve(matrix, ...)
    x$setmatrix(m)
    m
}

#for an example, the code works this way
#I create a matrix m with values 1:4, with 2 rows and 2 columns
# m <- matrix( 1:4, nrow = 2, ncol = 2)
#       [,1] [,2]
#[1,]    1    3
#[2,]    2    4
# I apply makeCacheMatrix to store my m matrix and get it's inverse
# m1 <- makeCacheMatrix(m)
# I call the cacheSolve function to get the stored inverse of my matrix
# cacheSolve(m1)
#      [,1] [,2]
#[1,]   -2  1.5
#[2,]    1 -0.5