## Assignement 2 R-programming - Coursera
## Caching the Inverse of a Matrix
## 2 functions that cache the inverse of a matrix, in order to avoid repeated computing.


## function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

       m <- NULL
       set <- function(y) {
             x <<- y
             m <<- NULL
   }
       get <- function() x
       setinverse <- function(solve) m <<- solve
       getinverse <- function() m
       list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
 }


## Function that computes the inverse of the special "matrix" returned by the "makeCacheMatrix".
## If the inverse has already been calculated (and the matrix has mot changed), then cacheSolve
## should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      
             m <- x$getinverse()
             if(!is.null(m)) {
                   message("getting cached data")
                   return(m)
             }
             data <- x$get()
             m <- solve(data, ...)
             x$setinverse(m)
             m
}


## Part to test that the function in working with a simple matrix 2x2 defined
## in "mymatrix"! Uncomment to test the functions.
# mymatrix<- matrix(c(1,2,2,3), 2, 2);
# 
# 
# A<-makeCacheMatrix(mymatrix);
# cacheSolve(A)
# print(A$getinverse())