########################################################################################
################### Append all monthly waiting times files into one ####################
########################################################################################

#Re-load IS provider by month

IS_providers_allmonths <- fread(paste0(R_workbench,"/",temp_folder,"/IS_providers_allmonths.csv"),
                                header=TRUE, sep=",", check.names=T)

#Append all files
for (j in 1:nrow(links_out_df)){
  
  #Find name of large CSV
  file.name <- list.files()[str_detect(list.files(),"full-extract")][1] #To make sure there's only one file - is it the right one?
  
  #Read in
  RTT_month <- fread(file.name, header=TRUE, sep=",", check.names=T)
  rm(file.name)
  
  #Add month-year indicator
  RTT_month$monthyr <- as.character(links_out_df$month[j])
  
  #New indicator variable to flag independent providers
  RTT_month$IS_provider <- ifelse(RTT_month$Provider.Org.Code %in% filter(IS_providers_allmonths,monthyr==links_out_df$month[j])$codes,1,0)
  
  #Successively append files
  if (j==1) {
    storage.rtt <- RTT_month
  } else {
    storage.rtt <- plyr::rbind.fill(storage.rtt,RTT_month)
  }
}

#Save
fwrite(storage.rtt, file = "RTT_allmonths_new.csv", sep = ",")


put_object(file = 'RTT_allmonths_new.csv',
           object = paste0(RTT_subfolder,"/","RTT_allmonths_new.csv"),
           bucket = IHT_bucket, show_progress = TRUE,
           multipart=TRUE)

put_object(file = 'IS_providers_allmonths.csv',
           object = paste0(RTT_subfolder,"/","IS_providers_allmonths.csv"),
           bucket = IHT_bucket, show_progress = TRUE,
           multipart=TRUE)

put_object(file = 'providers_allmonths.csv',
           object = paste0(RTT_subfolder,"/","providers_allmonths.csv"),
           bucket = IHT_bucket, show_progress = TRUE,
           multipart=TRUE)