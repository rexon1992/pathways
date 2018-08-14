#' Generate transition file
#'
#' This function generates and writes a file into the working directory with all the indicators avaliable in a dataset, its units and an empty column called "Default Indicator Name" to match model indicators to default indicators and writes. After updating the file it can then be used as an input to the transition function to replace model indicator names with default indicator names.
#' @param d_file A string containing name of the data file excluding the extension
#' @export

gen_tr_file<-function(d_file){
  data<-read.csv(paste(d_file,".csv",sep=""),header=T,check.names=F,stringsAsFactors=F)
  indicators<-unique(data$`Model Indicator Name`)
  units<-data$`Unit of Entry`[match(indicators,data$`Model Indicator Name`)]
  default_name<-rep("",length(indicators))
  note<-rep("",length(indicators))
  tr<-data.frame(indicators,units,default_name,note)
  colnames(tr)<-c("Model Indicator Name","Unit of Entry","Default Indicator Name","Note")
  write.csv(tr,paste(d_file,"tr.csv",sep="_"),row.names=F,na="")
}
