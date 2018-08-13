#' Fill cell between rows of a particular column which are NA with data in the row preceding empty space
#'
#' @param x Dataframe to be manupulated
#' @param colname String containing the name of the column to be manupulated
#' @return Dataframe with the specified column filled
#' @export

fill_coll<-function(x,colname)
{
  col_ind<-which(names(x)==colname)
  naf<-which(is.na(x[,col_ind])==F | x[,col_ind]!="")
  nat<-which(is.na(x[,col_ind])==T | x[,col_ind]=="")
  for (j in 1:(length(naf)-1))
  {
    c_1<-which(nat>naf[j]&nat<naf[j+1])
    x[nat[c_1],col_ind]<-x[naf[j],col_ind]
  }
  c_2<-which(nat>naf[j+1])
  x[nat[c_2],col_ind]<-x[naf[j+1],col_ind]
  return(x)
}
