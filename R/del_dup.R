#' Delete duplicates
#'
#' Delete rows which have duplicate cells in a particular column
#' @param colname Name of the column containing duplicate names
#' @param data_frame Dataframe to be processed
#' @return Dataframe with columns deleted
#' @export

del_dup<-function(colname,data_frame){
  col_ind<-which(names(data_frame)==colname)
  indicators<-unique(data_frame[,col_ind])
  for (i in 1:length(indicators))
  {
    vect<-data_frame[which(data_frame[,col_ind]==indicators[i]),col_ind]
    dup_gas<-vect[which(duplicated(vect)==TRUE)]
    dup_gas_lev<-unique(dup_gas)

    if (length(dup_gas_lev)>=1){
      ind<-which(data_frame[,col_ind]==dup_gas_lev)
      data_frame<-data_frame[-(ind[2:length(ind)]),]
    }
  }
  return(data_frame)
}
