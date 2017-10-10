##Author: Ilia Rushkin

#This function implements the calculation of the Eddington number (a.k.a Hirsch index in a different context)
#Inputs: 
#x is a vector. E.g. the vector of country names, one per user in a course
#slope: (1 by default) is the slope of the straight line used in the calculation, a way to scale the problem.
#na.rm: a boolean, whether or not to drop NAs from x.

#Output: 
#a list with the elements:
        #R - the global reach index (=Eddington number, = Hirsch index, depending on the context)
        #Rn - the global reach index normalized to the theoretical maximum (value between 0 and 1)
        #N - the number of data values used in the calculation (will drop NAs from x by default)


eddington=function(x,slope=1, na.rm=TRUE){
  if(na.rm){
    x=x[!is.na(x)]
  }
  if(length(x)==0){
    return(list(R=NA,Rn=NA,N=0))
  }else{
    temp=sort(table(x), decreasing=TRUE)
    R=max(pmin(temp,slope*(1:length(temp))))/slope
    Rn=R/sqrt(length(x)/slope)
    return(list(R=R,Rn=Rn,N=length(x)))
  }
}