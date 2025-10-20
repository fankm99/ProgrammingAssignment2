
## makeCacheMatrix -----------------------------------------------------------
## If the matrix changes via set(), the cached inverse is cleared.
 
makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
 
set <- function(y) {
if (!is.matrix(y)) stop("Input must be a matrix.")
x <<- y
inv <<- NULL # new matrix -> old inverse no longer valid
}
 
get <- function() x
setinv <- function(i) inv <<- i
getinv <- function() inv
 
list(set = set, get = get,
setinv = setinv, getinv = getinv)
}
 
 
## cacheSolve ----------------------------------------------------------------
## Return the cached inverse if available; otherwise compute first then cache and return.
 
cacheSolve <- function(x, ...) {
inv <- x$getinv()
if (!is.null(inv)) {
message("getting cached inverse")
return(inv)
}
 
m <- x$get()
 
if (!is.matrix(m)) stop("Object does not contain a matrix.")
if (nrow(m) != ncol(m)) stop("Matrix must be square to invert.")
 
## use tryCatch if matrix is singular/not invertible.
inv <- tryCatch(
solve(m, ...),
error = function(e) stop("Matrix is singular or not invertible: ", e$message)
)
 
x$setinv(inv)
inv
}
