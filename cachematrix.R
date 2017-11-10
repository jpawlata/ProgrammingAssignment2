# Below functions use lexical scoping to calculate inverted matrix (based on the matrix given) and store the result
# in the makeCacheMatrix environment. 

# makeCacheMatrix function creates 4 functions (set, get, setinverse and getinverse). Together with 'x' and 'm' variables, 
# mentioned functions are stored in makeCacheMatrix environment.
# 'x' - matrix that is makeCacheMatrix function's argument
# 'm' - matrix that is the inverse of 'x'
# At the end of the function we return a list that contains created functions
# (for more details please check inline comments below)

makeCacheMatrix <- function(x = martix()) {
    m <- NULL    # initialize m object    
    set <- function(y) {
    x <<- y    # assing to the parent environment (makeCacheMatrix)
    m <<- NULL # assing to the parent environment, clears possible value stored in the cache
  }
  get <- function() x    # return 'x' from the makeCacheMatrix environment
  setinverse <- function(m_inverse) m <<- m_inverse    # assign calculated inverted matrix to 'm' in the makeCacheMatrix environment
  getinverse <- function() m     # return 'm' from the makeCacheMatrix environment
  list(set = set, get = get,   # return a list with functions created
       setinverse = setinverse,
       getinverse = getinverse)
}

# cacheSolve function gets cached inverted matrix or populate new one. '...' allows to pass additional arguments if needed.
# (more details below)

cacheSolve <- function(x, ...) { 
    m <- x$getinverse()    # check the value of 'm' 
    if(!is.null(m)) {    # if m != NULL get cached data, return inverted matrix and stop computation
      message("getting cached data")
      return(m)
    }
    data <- x$get()    # get the 'x' matrix using 'get' function from the 'x' passed as an argument
    m <- solve(data, ...)    # calculate inverted matrix
    x$setinverse(m)    # set calculated matrix using 'set' function from the 'x' passed as an argument
    m    # return a matrix that is the inverse of 'x'
        
}