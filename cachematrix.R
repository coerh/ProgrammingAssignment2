## these functions create a matrix and can cache its inverse

## create a matric object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

    ## sets the inverse object to NULL
    m <- NULL
    
    ## function to set the matrix
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    
    ## function to get the matrix
    get <- function() x
    
    ## function to set the inverse of the matrix
    setInv <- function(inverse) m <<- inverse
    
    ## function to get the inverse of the matrix and return it
    getInv <- function() m
    
    
    ## return a list
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
  
}


## Calculate the inverse of the matrix from makeCacheMatrix
## if current matrix inverse has already been calculated then return the inverse from cache

cacheSolve <- function(x, ...) {
  
    ## return a matrix that is the inverse of x
    m <- x$getInv()
    
    ## if the inverse is already set, send the 'getting cached data' message and  return the inverse
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    
    ## get the matrix
    data <- x$get()
    
    ## matrix multiplication: calculate inverse
    m <- solve(data) %*% data
    
    ## set inverse
    x$setInv(m)
    
    ## return inverse
    m
    
}
