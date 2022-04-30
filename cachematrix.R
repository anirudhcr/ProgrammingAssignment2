## Caching the inverse of a matrix.
## 

##This function creates a special "matrix" object that can cache its inverse 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set<-function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse)  inv <<- inverse
        getinv <- function() inv
        list(set=set, get=get,
             setinv = setinv , getinv = getinv)
        
}


## Function to compute the inverse of the special matrix and get the inverse
##already computed

cacheSolve <- function(x, ...) {
        inv<-x$getinv()
        #print(inv)
        if(!is.null(inv)){
                message("getting cached inverse")
                return(inv)
        }
        data<-x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
