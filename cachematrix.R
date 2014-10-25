# How to run (notes for evaluator)
#
# Create matrix 10x10 with random values
# (of course you can create square matrix with any other dimention: 5x5, 100x100, 1000x1000... etc )
#> cmatrix<-makeCacheMatrix(matrix(runif(100,1,50),ncol=10,nrow=10))
# 
# Perform matrix inversion:
#> cacheSolve(cmatrix)
# <inverted matrix will be printed>
# 
# Perform matrix inversion again (data will be taken from cache) 
#> cacheSolve(cmatrix)
#
# "getting inverted matrix from cache" message will appear
# <inverted matrix from cache will be printed>
#

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function(){
    x
  } 
  setmatrix<-function(solve){ 
    m<<- solve
  }
  getmatrix<-function(){
    m
  }
  
  list(set=set, 
       get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix()
  if(!is.null(m)){
    message("getting inverted matrix from cache")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix)
  x$setmatrix(m)
  m
}