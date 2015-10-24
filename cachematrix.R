## Using the two functions bellow we can cach the Inverse of a Matrix


## This function creates a special "matrix" object that can cache its inverse.
## Specific Object has the functions bellow to support the manipulation of matrix data
## $set(y))       : sets the matrix data, and initializes cached inverse to NULL
## $get           : get the matrix data
## $setinverse(y) : assigns the inverse matrix y to cache variable
## $getinverse    : returns the calculated inverse matrix from cache 

makeCacheMatrix <- function(x = matrix()) {
   x_inv <- NULL
   set <- function(y) {
       x <<- y
       x_inv <<- NULL
        }
   get <- function() x
   setinverse <- function(matrix_inverse) x_inv <<- matrix_inverse
   getinverse <- function() x_inv
   list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
  

}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##  The actual computation is performed the first time we execute the function, 
##  and in case that we change matrix data afterwords, using the function $set of created matrix object  
##  In all other cases the inverse matrix is returned directy from cache   

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		x_inv <- x$getinverse()
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data)
        x$setinverse(x_inv)
        x_inv

}


## Test Scenario 
## 
## > pinakas_a<-makeCacheMatrix(rbind(c(0,1,3), c(0,1,2),c(1,4,1)))
## > cacheSolve(pinakas_a)
##       [,1] [,2] [,3]
## [1,]    7  -11    1
## [2,]   -2    3    0
## [3,]    1   -1    0
## > cacheSolve(pinakas_a)
##  getting cached data
##     [,1] [,2] [,3]
##  [1,]    7  -11    1
##  [2,]   -2    3    0
##  [3,]    1   -1    0


