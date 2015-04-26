makeCacheMatrix <- function(x = matrix()) {
  ## x: a square invertible matrix
  ## return: a list containing functions to
  ##              1. set the matrix
  ##              2. get the matrix
  ##              3. set the inverse
  ##              4. get the inverse
  ##         this list is used as the input to cacheSolve()
  
  mat_inv <- NULL
  set <- function(y) {
    # use `<<-` to assign a value to an object in an environment 
    # different from the current environment. 
    x <<- y
    mat_inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) mat_inv <<- inverse 
  getinv <- function() mat_inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}

cacheSolve <- function(x, ...) {
  ## @x: output of makeCacheMatrix()
  ## return: inverse of the original matrix input to makeCacheMatrix()
  
  mat_inv <- x$getinv()
  
  # if the inverse has already been calculated
  if (!is.null(mat_inv)){
    # gets it from the cache and skips the computation. 
    message("getting cached data")
    return(mat_inv)
  }
  
  # otherwise, calculates the inverse 
  mat.data <- x$get()
  mat_inv <- solve(mat.data, ...)
  
  # sets the value of the inverse in the cache via the setinv function.
  x$setinv(mat_inv)
  
  return(mat_inv)
}

test = function(mat){
  ## @mat: an invertible matrix
  ## invert the matrix twice and record the execution times. 
  temp = makeCacheMatrix(mat)

  ##The inversion is done the first time and should take longer
  start.time <- Sys.time()
  cacheSolve(temp)
  dur <- Sys.time() - start.time
  print(dur)
  
  ## Run the inversion second time. This should use the cached matrix and should take less time compares to the first run
  start.time <- Sys.time()
  cacheSolve(temp)
  dur <- Sys.time() - start.time
  print(dur)
}

set.seed(21110201)
r = rnorm(1000000)
mat1 = matrix(r, nrow=1000, ncol=1000)
test(mat1)
