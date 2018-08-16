#' Add Rows
#'
#' This function adds all the rows in a dataframe within a specified range of columns and returns a data frame with one row and specified range of columns containing the sum
#'
#' @param data_frame dataframe to be added
#' @param first_col Name of the first column in the range
#' @param last_col Name of last column in the range
#'
#' @export

add_rows<-function(data_frame,first_col,last_col){
  data<-data_frame
  sum<-rep(0,length(c(which(names(data)==first_col):which(names(data)==last_col))))
  for(l in 1:nrow(data))
  {
    temp<-data[l,c(which(names(data)==first_col):which(names(data)==last_col))]
    sum=sum+temp
  }
  return(sum)
}
