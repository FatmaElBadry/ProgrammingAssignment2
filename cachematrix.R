## Week 3 Assignment;cachematrix;Developed By: Fatma ElBadry

## Put comments here that give an overall description of what your
## functions do

## Function "makeCacheMatrix" gets a matrix as an input, And has a list of functions that can do the following:
## 1.set the value of the matrix,
## 2.get the value of the matrix, 
## 3.set the inverse Matrix and 
## 4.get the inverse Matrix. 
## The matrix object can cache its own object.

## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  #take the matrix as an input
  invMatrix <- NULL
  # 1.set the value of the Matrix
   setMatrix <- function(y) {
    x <<- y
    invMatrix <<- NULL
   }
   
  # 2.get the value of the Matrix
    getMatrix <- function() x 
    
  # 3.set the value of the invertible matrix  
    setInverse <- function(inverse) invMatrix <<- inverse  
    
  # 4.get the value of the invertible matrix
    getInverse <- function() invMatrix  
  
  ##define the below list in order to refer to the functions with the $ operator
    list(setMatrix = setMatrix, getMatrix = getMatrix,
    setInverse = setInverse, getInverse = getInverse)
    
}



## Write a short comment describing this function

## The function "cacheSolve" takes the output of makeCacheMatrix(matrix) as an 
# input and checks inverse matrix from makeCacheMatrix(matrix) has any value in it or not.
# In case inverse matrix from makeCacheMatrix((matrix) is empty, it gets the original matrix data from 
# and set the invertible  matrix by using the solve function.
# In case inverse matrix from makeCacheMatrix((matrix) has some value in it (always works
# after running the code 1st time), it returns the cached object

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invMatrix <- x$getInverse()
          if(!is.null(invMatrix)) {                         #if inverse matrix is not NULL
              return(invMatrix)                             #return the invertible matrix
            }
            
    #if value of the invertible matrix is NULL then  
            MatrixData <- x$getMatrix()                     #get the original Matrix Data 
            invMatrix <- solve(MatrixData, ...)             #use solve function to inverse the matrix
            x$setInverse(invMatrix)                         #set the invertible matrix 
            return(invMatrix)                               #return the invertible matrix
}
