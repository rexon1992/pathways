#' Generate scenario meta infotmation template
#'
#' This function generates and writes a scenario meta information template for a particular data file with minimum information required to upload the template on the portal in the same directory as the data file
#'
#' @param ul_dfile_path Path to the data file for which the template is to be generated
#' @param meta_template_path Path to the default scenario meta template file in the csv format
#'
gen_sen_meta<-function(ul_dfile_path, meta_template_path){
  template<-read.csv(meta_template_path, header=T, check.names = F, stringsAsFactors = F)
  data<-read.csv(ul_dfile_path,header=T, check.names = F, stringsAsFactors = F)

  scenarios<-unique(data$Scenario)
  sen<-data.frame(matrix(nrow = nrow(template),ncol = length(scenarios)))
  for (i in 1:length(scenarios)){
    colnames(sen)[(i)]<-paste ("Scenario",i,sep=" ")
    sen[1,(i)]<-unique(data$Model)[1]
    sen[2,(i)]<-scenarios[i]
  }

  template<-cbind(template,sen)
  write.csv(template,gsub("UL.csv","sen_meta_ul.csv",ul_dfile_path),row.names = F,na="")
}


#meta_template_path<-"/Users/Rexon/Downloads/scenarios_upload_template-35.csv"
#ul_dfile_path<-"/Users/Rexon/World Resources Institute/Climate Watch - Documents/Emissions Scenarios/Data Collection/Time Series Data/Risky Business/rr_rawdata_UL.csv"
