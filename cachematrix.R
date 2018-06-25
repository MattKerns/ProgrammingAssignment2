## These functions will create a list that contains some info about a matrix,
## in particular it will set the function or operation we wish to perform,
## and get the value we wish to obtain.

## This function returns a list of 4 which will contain; The original matrix,
## stores the value under 'get', sets the value of the function we want to use,
## 'solve' in this case, and gives instructions to set 'inv' equal to the function of
## our original matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
              x <<- y
              inv <<-NULL
        }
        get <- function() x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function will have as it's input, the list created in the output.
## If the inverse has been found it will return it, if not it will calculate it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if(!is.null(inv)){
        message("Calculating the inverse of the matrix")
        return(inv)
        }
      data <- x$get()
      inv <- solve(data,...)
      x$setinv
      inv
      
      
}

