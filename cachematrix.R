## --------------------------------------------------------------------------
## The first function, makeCacheMatrix creates a special "matrix" object that 
## can cache the input matrix and its inverse. it creates a list that contains 
## 4 functions :set,get,setinverse and getinverse.
## --------------------------------------------------------------------------

makeCacheMatrix <- function(x = matrix()) {

# sets the value of m to default value NULL.
        m <- NULL	

# set the value of the matrix		
        set <- function(y) {
                x <<- y
                m <<- NULL
        }

# get the value of the matrix
        get <- function() x

# set the value of the inverse matrix
        setinverse <- function(inverse) m <<- inverse 

# get the value of the inverse matrix
        getinverse <- function() m

										# Return a list containg all functions above
										        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
 }

## --------------------------------------------------------------------------
## The cacheSolve function returns the inverse of the matrix. First will checks 
## if the inverse has already been computed and stored. If so, it gets 
## the result and skips the computation. If not, it computes the inverse, 
## sets the value in the cache via setinverse function.
## --------------------------------------------------------------------------
cacheSolve<- function(x, ...) {

# get the inversed matrix from object x
       m <- x$getinverse()

# Check if an inverse has already been calculated. If so, return the inverse.
       if(!is.null(m)) {			
									                  message("getting cached data")
          return(m)
          }

#if not, calculate the inverse.
          data <- x$get()
          m <- solve(data, ...)

# Set the value of inverse in the cach
         x$setinverse(m)

# return the solved result
          m
}
