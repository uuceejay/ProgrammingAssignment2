#creates a matrix in computer memory
makeCacheMatrix <- function (x = matrix()){
  inv<-NULL # sets inverse to NULL
  set <-function(newValue) {
    x<<-newValue  # hold value of matrix in memory as newValue
    inv  <<- NULL #holds value of inv as NULL
  }
  
  get<- function() x
  setinv<-function(storeInverse) inv <<-storeInverse # stores inverse
  getinv <- function() inv # gets the value of inv after computed
  list(set=set,get=get,setinv=setinv,getinv=getinv)
  
}
cacheSolve <-function(x,..) {   #functon computes and returns the inverse
  inv <- x$getinv()
  if(!is.null(inv)){             #checks for cached value
    message("getting cached data")
    return(inv)
  }
  data <-x$get()         # if not found in cache computes inverse
  inv<-solve(data,..)
  x$setinv(inv)
  inv
}

