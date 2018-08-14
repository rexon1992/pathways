#' Delete rows which have no data between specified range of columns
#'
#' This function deletes all rows which have no data within a range of columns specified using start column name and end column name
#'
#' @param dataframe Dataframe to be processed
#' @param startname Name of the first column in the range on columns
#' @param endname Name of the last colume in the range of columns
#' @export

del_rows_ndat<-function(dataframe,startname,endname)
{
  names<-names(dataframe)
  m1<-which(dataframe[,which(names==startname)]=="")
  for (i in (which(names==startname):which(names==endname)))
  {
    assign(paste("m",i,sep = ""),which(dataframe[,i]==""))
    m1<-intersect(m1,get(paste("m",i,sep = "")))
  }
  if(length(m1>0)){
    dataframe<-dataframe[-m1,]
  }
  return(dataframe)
}
