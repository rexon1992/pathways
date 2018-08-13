#' Find text
#'
#' Find the all the the index of all the cells which contain full or partial text
#' @param text Full or partial text to be found
#' @param data_frame Dataframe to be searched
#' @return Matrix of row and colunm names of all cells containing the text
#' @export

find_text<-function(text, data_frame){
  index<-NULL
  n_col<-ncol(data_frame)
  for (c in 1:n_col){
    row<-grep(text,data[,c],fixed = T)
    if (length(row)>0){
      col<-rep(c,length(row))
      temp_ind<-rbind(row,col)
      index<-cbind(index,temp_ind)
    }
  }
  return(t(index))
}
