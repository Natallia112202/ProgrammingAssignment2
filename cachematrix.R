
## COMMENTS to give an overall description of what it is all about:##

# This script contains two functions (makeCacheMatrix() and cacheSolve()).
# They work together to cache the inverse of the matrix x. 
# That will allow to avoid the repeated calculations of the inverse of the
# matrix if the inverse has laready been calculated before.

## FUNCTION 1 (makeCacheMatrix()##
# funciton sets a new value for the original matrix (non-inversed).
# This invalidates the cached inversed matrix (INV) value.
# <<- (not <) operator used here to set the value of x and INV 
# because x and INV we want to modify are defined/created  in the environment of 
# makeCacheMatrix, not in the environment local to set(). 
# We reset INV to NULL here since we are modifying the original matrix and the cached value of INV is no longer valid.

makeCacheMatrix <- function(x = matrix()) {

INV <- NULL
set <- function(y) {
         x <<- y
         INV <<- NULL
     }
     get <- function(){x}
     setinverse <- function(inverse) INV <<- inverse
     getinverse <- function() INV
     list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
 }
 
# list() function is used here to return value of the makeCacheMatrix as 
# a list of functions that will be visible to a user.
# Each function on the list is accessed with the $ operator:
# e.g. we have matrix x<-(1:4,2,2) and
# we  assign to M<-makeCacheMatrix(x), then we can retrieve 
# original matrix by calling M$get()
# we can retrieve the inverted  matrix by
# using  M$getinverse()


## FUNCTION 2 cacheSolve()##

# Return a matrix that is the inverse of 'x', assuming that the matrix is invertible
# the function first check if the inverse has been computed. If yes, it retrieves the result and skips the
# computation. If not, it computes the inverse of matrix x , sets the obtained value  in the cache using
# setinverse function

cacheSolve <- function(x, ...) {
     INV <- x$getinverse()
     if(!is.null(INV)) {
         message("getting cached data.")
         return(INV)
     }
     data <- x$get()
     INV <- solve(data)
     x$setinverse(INV)
     INV 
 }
 
 

##EXAMPLE 1 of how the functions work:##
> M<-makeCacheMatrix(x)
> M$get()
     [,1] [,2]
[1,]    1    3
[2,]    2    4
> M$getinverse()
NULL #the cacheSolve() has not calculated the inverse of matrix x yet #
> cacheSolve(M)
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> M$getinverse()#inverse of the matrix has been calculated usin cacheSolve and put in cache#
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
>

##EXAMPLE 2 ## 

> x<-matrix(c(1,0,5,2,1,6,3,4,0),3,3)
> x
     [,1] [,2] [,3]
[1,]    1    2    3
[2,]    0    1    4
[3,]    5    6    0
> L<-makeCacheMatrix(x)
> L
$set
function (y) 
{
    x <<- y
    INV <<- NULL
}
<environment: 0x0000000010c9f468>

$get
function () 
{
    x
}
<environment: 0x0000000010c9f468>

$setinverse
function (inverse) 
INV <<- inverse
<environment: 0x0000000010c9f468>

$getinverse
function () 
INV
<environment: 0x0000000010c9f468>

> L$get()
     [,1] [,2] [,3]
[1,]    1    2    3
[2,]    0    1    4
[3,]    5    6    0
> L$getinverse()
NULL
> cacheSolve(L)
     [,1] [,2] [,3]
[1,]  -24   18    5
[2,]   20  -15   -4
[3,]   -5    4    1
> L$getinverse()
     [,1] [,2] [,3]
[1,]  -24   18    5
[2,]   20  -15   -4
[3,]   -5    4    1
> 

