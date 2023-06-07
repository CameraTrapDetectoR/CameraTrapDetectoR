#' Relabel predictions using location 
#' 
#' @description This function compares the labels provided by the model with a list of possible
#' species based on species ranges.  In some cases it relabels mis-classifications
#' resulting from assumption that all species could be present in photo. Helper
#' function for `deploy_model`
#' 
#' @param pred_df dataframe containing predictions for image
#' @param possible.labels dataframe containing possible labels based on species occurrence
#' @param label_encoder label dictionary for model being run
#' 
#' @return relabeled predictions based on location
#' 
#' @export

smart_relabel <- function(pred_df, possible.labels, label_encoder){
  
  #----Wild_Pig
  
  #--Assign Collared_Peccary to Wild_Pig
  if(all(possible.labels$label %in% "Collared_Peccary")==FALSE){
    pred_df[pred_df$prediction=="Collared_Peccary","prediction"] <- "Wild_Pig"
    pred_df[pred_df$prediction=="Wild_Pig","label"] <- label_encoder[label_encoder$label=="Wild_Pig","encoder"]
  }
  
  if(all(possible.labels$label %in% "Tayassuidae")==FALSE){
    pred_df[pred_df$prediction=="Tayassuidae","prediction"] <- "Suidae"
    pred_df[pred_df$prediction=="Suidae","label"] <- label_encoder[label_encoder$label=="Suidae","encoder"]
  }
  
  
  #--Other 
  potential.conflicts <- c("Moose","American_Black_Bear","Grizzly_Bear")
  
  for(j in 1:length(potential.conflicts)){
    if(nrow(possible.labels[possible.labels$label %in% potential.conflicts[j],])==0){
      pred_df[pred_df$prediction %in% potential.conflicts[j],"prediction"] <- "Wild_Pig"
      pred_df[pred_df$prediction=="Wild_Pig","label"] <- label_encoder[label_encoder$label=="Wild_Pig","encoder"]
    }#END Logical
  }#END Loop
  
  
  #----White-Tailed_Deer
  potential.conflicts <- c("Caribou","Mule_Deer","Pronghorn","White-Tailed_Deer","Bighorn_Sheep","Nilgai")
  
  if(length(possible.labels[possible.labels$label %in% potential.conflicts,"label"])==1){
    if(possible.labels[possible.labels$label %in% potential.conflicts,"label"]=="White-Tailed_Deer"){
      pred_df[pred_df$prediction %in% potential.conflicts,"prediction"] <- "White-Tailed_Deer"
    }
  }
  
  
  #----Bobcat
  potential.conflicts <- c("Bobcat","Canada_Lynx","Jaguarundi","Margay","Ocelot")
  
  if(length(possible.labels[possible.labels$label %in% potential.conflicts,"label"])==1){
    if(possible.labels[possible.labels$label %in% potential.conflicts,"label"]=="Bobcat"){
      pred_df[pred_df$prediction %in% potential.conflicts,"prediction"] <- "Bobcat"
    }
  }
  
  
  #----Ocelot
  potential.conflicts <- c("Bobcat","Jaguarundi","Margay","Ocelot")
  
  if(length(possible.labels[possible.labels$label %in% potential.conflicts,"label"])==1){
    if(possible.labels[possible.labels$label %in% potential.conflicts,"label"]=="Ocelot"){
      pred_df[pred_df$prediction %in% potential.conflicts,"prediction"] <- "Ocelot"
    }
  }
  
  if(nrow(pred_df[pred_df$prediction %in% possible.labels$label,])==0){
    pred_df[(pred_df$prediction %in% possible.labels$label)==FALSE,"prediction"] <- "Animal"
    pred_df[pred_df$prediction %in% "Animal","label"] <- max(label_encoder$encoder)+1
  }
  
  return(pred_df)
}#END Function

