# makeCacheMatrix will 
# 1. set value of matrix 
# 2. get value of matrix 
# 3. set value of inverse of matrix
# 4. get value of inverse of matrix
# using a list containing a function

makeCacheMatrix <- function(x = matrix()) {
        #create variable to store inverse matrix
        inverse <- NULL
       
        #create matrix
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        
        #get value of matrix
        get <- function() x
        setinverse <- function(i) inverse <<- i
        getinverse <- function() inverse
        #now return matrix w new functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


# cacheSolve will return the inverse of the matrix
# if inverse already exists, cacheSolve will return the cached inverse. 

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  # if inverse has already been calculated, return inverse
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  #if inverse hasn't been calculated yet, calculate it
  data <- x$get()
  inverse <- solve(data, ...)
  
  #cache inverse
  x$setinverse(inverse)
  
  #return
  inverse
  
}
