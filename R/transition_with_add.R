#' Transition from model name to default names and adds 2 or more indicators if mentioned in the transition file
#'
#' Replaces model indicator names in the raw data file aready in the pathways template by default indicator names using the tranistion file matching both indicator names, adds indicators which are mentioned in the transition file to be added, deletes all duplicate values and generates a file to enter notes
#' @param d_file Path to the raw data file
#' @param tr_file Path to the transition file
#' @param notes A binary value for the arguement note which generates a file to add conversion factors and notes
#' @param prod_ind_list Path to the file containing list of indicators on the portal currently in Emissions Scenarios>Indicators>ind_list_production.xlsx
#' @return A data file with all the model indicator names replaced with default indicator names and all duplicates daletes. If notes is set to 1 a file to enter notes
#' @export


transition_with_add<-function(d_file,tr_file,notes,prod_ind_list)
{
  library(readxl)
  data<-read.csv(d_file,header=T,check.names=F,stringsAsFactors=F)
  tr<-read.csv(tr_file,header=T,check.names=F,stringsAsFactors=F)

  default_names<-c("Model","Scenario","Region","Model Indicator Name","Unit of Entry")
  match<-match(default_names,names(data))
  na<-which(is.na(match)==T)
  if(length(na)>=1){
    to_add<-paste(default_names[which(is.na(match)==T)],collapse = ", ")
    stop(paste("Columns ",to_add," missing from the input file",sep=""  ))
    #print(paste("Headers of the input file do not match the default format: Add columns ",to_add,sep=""  ))
  }

  to_add<-tr$`Model Indicator Name`[grep("\\+",tr$`Model Indicator Name`)]
  if (length(to_add)>=1){
    scenarios<-unique(data$Scenario)
    regions<-unique(data$Region)
    for (a in 1:length(to_add)){
      ind_add<-unlist(strsplit(to_add[a],"\\+"))
      for (s in scenarios){
        temp<-data[which(data$Scenario==s),]
        for (r in regions){
          temp2<-temp[which(temp$Region==r),]

          add_match<-match(ind_add,temp2$`Model Indicator Name`)
          add<-temp2[add_match,]
          if(nrow(add)>=1){
            sum<-add_rows(add,"2016","2015")
            new_row_inf<-add[1,c(1,2,3,4,5)]
            new_row_inf$`Model Indicator Name`<-to_add[a]
            new_row<-cbind(new_row_inf,sum)
            data<-rbind(data,new_row)
          }


        }
      }
    }
  }

  assigned<-which(tr$`Default Indicator Name`!="")

  ind_tr<-match(data$`Model Indicator Name`,tr$`Model Indicator Name`[assigned])

  data$`Model Indicator Name`<-tr$`Default Indicator Name`[assigned][ind_tr]

  colnames(data)[which(colnames(data)=="Model Indicator Name")]<-"Default Indicator Name"

  del<-which(is.na(data$`Default Indicator Name`)==T)
  if(length(del)>=1){
    data<-data[-del,]
  }

  scenario<-levels(factor(data$Scenario))
  region<-levels(factor(data$Region))

  for (s in 1:length(scenario))
  {
    for (r in 1:length(region))
    {
      vect<-data$`Default Indicator Name`[which(data$Region==region[r] & data$Scenario==scenario[s])]
      dup_ind<-vect[which(duplicated(vect)==TRUE)]
      #print(scenario[s])
      #print(region[r])
      #print(length(dup_ind))

      dup_ind_lev<-levels(factor(dup_ind))

      if (length(dup_ind_lev)>=1){
        for (d in 1:length(dup_ind_lev))
        {
          ind<-which(data$Region==region[r] & data$Scenario==scenario[s] & data$`Default Indicator Name`==dup_ind_lev[d])
          data<-data[-(ind[2:length(ind)]),]
        }
      }
    }
  }


  if(length(na)<1){
    #data<-del_rows_ndat(data,"Model","Scenario")
    write.csv(data,gsub(".csv","_UL.csv",d_file),row.names=F,na="")
  }


  if(notes==1 & length(na)<1)
  {
    prod_ind_list<-read_excel(prod_ind_list,sheet = "ind_list_production_temp",col_names = TRUE)
    indicators<-unique(tr$`Default Indicator Name`)
    model_name<-rep("",length(indicators))
    unit_entry<-tr$`Unit of Entry`[match(indicators,tr$`Default Indicator Name`)]
    default_unit<-prod_ind_list$`Standardized Unit`[match(indicators,prod_ind_list$`Default Indicator Name`)]
    conversion<-rep("",length(indicators))
    note<-tr$Note[match(indicators,tr$`Default Indicator Name`)]

    notes_file<-data.frame(indicators,model_name,unit_entry,default_unit,conversion,note)

    colnames(notes_file)<-c("Default Indicator Name","Model Name","Unit of Entry","Default Unit","Conversion Factor","Note")

    write.csv(notes_file,gsub(".csv","_NOTES.csv",d_file),row.names=F,na="")
  }
}
