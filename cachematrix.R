## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The function below creates a special matrix, which is a list
## containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
    i   <- NULL
    set <- function(y) {
            x <<- y
            i <<- NULL                                  #set the value of the matrix
    }
    get <- function() x                                 #get the value of the matrix
    setinverse <- function(inverse) i <<- inverse       #set the value of the inverse
    getinverse <- function() i                          #get the value of the inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}



## Write a short comment describing this function
## The following function calculates the inverse of the special "Matrix"
## created with the above function. It first checks if the inverse has
## already been calculated. If so, it skips computation and gets the inverse
## from the cache. Otherwise it calculates the inverse of the data and sets
## the value of the inverse via the setinverse function. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}


