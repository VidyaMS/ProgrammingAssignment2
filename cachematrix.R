## This function is for creating a 'makeCacheMatrix' object . 
## The function 'makeCacheMatrix' returns a list  which contains four  functions : one for getting the matrix and other for getting its inverse .
# The other two are for setting the value of the matrix and other for setting the value of its  inverse . 

## 

makeCacheMatrix <- function(x = matrix()) {
         i <- NULL

        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## This function checks if the inverse of the matrix is already computed and available in the 
## global environment . if yes , retrives it with the message that its from the cache . 
## If not , then computes the value of the inverse of the matrix and sets the value of the inverse in the list and also returns the value
## of the inverse. 

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
