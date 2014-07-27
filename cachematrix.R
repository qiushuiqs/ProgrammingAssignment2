## Matrix inversion is usually a costly computation and their may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly

## makeCacheMatrix creates a special "Matrix", which contain get/set method
## and getinverse/setinverse method.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
        x <<- y
        i <<-NULL
    }
    get <- function() x
    setinverse <- function(inverse) i<<-inverse
    getinverse <- function() i
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) 
}


## cacheSolve function calculates inverse of matrix. It first checks to see
## if the inverse has already been calculate, if so, it gets inverse 
## directly. Otherwise, it calculate the inverse of matrix and save it in 
## cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i<-x$getinverse()
    if(!is.null(i)){
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data,)
    x$setinverse(i)
    i
}
