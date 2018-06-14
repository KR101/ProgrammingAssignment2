## In this file, there are 2 functions: makeCacheMatrix and cacheSolve.
## makeCacheMatrix function: Matrix inversion is computationally demanding job. 
## To aviod recomputing the inverse for getting the same result repeatedly, 
## we can compute the result once and store in cache for reusing it.
## cacheSolve function: After creating the matrix, use the cacheSolve function to 
## compute the inverse and cache the result. If cacheSolve is used again on the same
## matrix, it uses the precopmuted result (inverse of matrix in this case), instead
## of recomputing it.

## Following the same format as given in the example given in the assignment.
## The "makeCacheMatrix function consists of following 5 steps:
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the martix
## 5. capture all the above four in a list.

makeCacheMatrix <- function(x = matrix()) {

## Initially set to NULL

inv <- NULL

## 1. Set the matrix

set<- function(y) {
	x <<- y
	inv <<- NULL
}

## 2. get the matrix
get <- function () x

## 3. set the inverse
setinverse <- function(inverse) inv <<- inverse

## 4. get the inverse

getinverse <- function() inv

## 5. capture all above in a list
list(set = set,
	get = get,
	setinverse = setinverse,
	getinverse = getinverse)
}


## Following the example of assignement to return a martix 
## that is inverve of "x".
## 1. Check if inverse is already computed on not yet.
## 2. If inverse is already computed, return precomputed inverserve
## and prompt a message " getting chached matrix"
## 3. If inverse is not yet computed, compute it and cache the result. 
## Return the cached result.

cacheSolve <- function(x, ...) {
	data <- x$get()
cacheSolve <- function(x, ...) {
	inv <- x$getinverse()
	
	if(!is.null(inv)) {
		message("Getting cached matrix")
		return(inv)
}
       inv <- solve(data, ...)
       x$setinverse(inv)        ## Return a matrix that is the inverse of 'x'
}
