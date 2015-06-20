#the following functions give the inverse of a matrix and
#if the inverse has already been calculated before, it just returnss its
#value from the cache

#makeCacheMtrix takes an invertible matrix and gives the output
#as a list of functions that set, get the value of the matrix
#and setinv,getinv set and get the value of the inverse

makeCacheMatrix<-function(x=matrix())

{     inv<-NULL

      set<- function(y){
      
      x<<-y
      inv<<-NULL
      
      
      }
      
      get<-function() x
      setinv<-function(inver) inv<<-inver
      getinv<- function() inv
   
      list(set=set, get=get,setinv=setinv, getinv=getinv)

}


#the argument to be passed to this function is the list from the
#previous function which we got aS THE OUTPUT. Store it in a variable
#and pass the variable to this function. This function checks if
#there is already a value of inverse for the same matrix
#present in the cache or not. If yes, it returns the value
#if not, (inv is null), then it caclulates it, stores it in cache
#and displays it as the output

cacheSolve<- function(x,...){
  
  inv<-x$getinv()
  
  if(!is.null(inv)){
    
    message("getting cached value")
    
    return(inv)
    
  }
  
  data<-x$get()
  
  inv<- solve(data,...)

  x$setinv(inv)
  
  inv
  
}
  


