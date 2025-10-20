
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
 
 
# --- testtest -------------------------------------------------------------
m <- matrix(c(4, 7, 2, 6), 2, 2)
cm <- makeCacheMatrix(m)
cacheSolve(cm) # computes
cacheSolve(cm) # uses cache
cm$set(diag(3)) # replace matrix/cache cleared
cacheSolve(cm)
 
From: Tian Chang <Tian_Chang@edwards.com> 
Sent: Monday, September 22, 2025 10:51 AM
To: Yizhuo Wang <Yizhuo_Wang@edwards.com>
Subject: 
 
Caching the Mean of a Vector
 
In this example we introduce the <<- operator which can be used to assign a value to an object in an environment that is different from the current environment. Below are two functions that are used to create a special object that stores a numeric vector and cache's its mean.
The first function, makeVector creates a special "vector", which is really a list containing a function to
1.	set the value of the vector
2.	get the value of the vector
3.	set the value of the mean
4.	get the value of the mean
 
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}
 
 
The following function calculates the mean of the special "vector" created with the above function. However, it first checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.
 
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}
