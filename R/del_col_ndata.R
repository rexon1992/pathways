#' Delete all columns which have no data
#'
#' This function deletes all the columns in a dataframe which are completely empty
#' @param dataframe Dataframe to be processed
#' @export

del_cols_ndat<-function(dataframe)
{
  to_delete<-c()
  ind<-ncol(dataframe)
  for (i in 1:ind)
  {
    na<-match(is.na(dataframe[,i]),TRUE)
    if(sum(match(is.na(dataframe[,i]),TRUE),na.rm = T)==nrow(dataframe)){
      to_delete<-c(to_delete,i)
    }
  }
  if(length(to_delete)>0){
    dataframe<-dataframe[,-to_delete]
  }

  return(dataframe)
}

