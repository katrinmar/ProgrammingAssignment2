## create a special matrix object that can cache its inverse
makeCacheMatrix <- function (x = matrix()) {
	inverse <- NULL
	set <- function (y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function () x
	setinverse <- function (theinverse) inverse <<- theinverse
	getinverse <- function () inverse
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

##computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function (x = matrix(), ...) {
	inverse <- x$getinverse()
	##if the inverse has already been calculated, it is retrieved from the cache	
	if (!is.null(inverse)) {
		message ("getting cached data")
		return (inverse)
	}
	data <- x$get()
	inverse <- solve(data)
	x$setinverse (inverse)
	inverse
}


