
collate_data <- function(app_IS, file_locs){
  
  is_prov <- fread(app_IS) ## get IS provider defs
  
  #Find name of large csvs
  file.name <- grep('full-extract', file_locs, value = TRUE)
  
  #Append all files
  for (j in 1:length(file.name)){
    
    #Read in
    RTT_month <- fread(file = paste0('RTT_temp_data/temp_files/', file.name[j]),
                       header = TRUE, sep = ',', check.names = T)
    
    #Add month-year indicator
    RTT_month$monthyr <- substr(file.name[j], 1, 5)
    
    #New indicator variable to flag independent providers
    RTT_month$IS_provider <- ifelse(
      RTT_month$Provider.Org.Code %in% filter(is_prov, monthyr == substr(file.name[j], 1, 5)), 1, 0)
    
    #Successively append files
    if (j==1) {
      storage.rtt <- RTT_month
    } else {
      storage.rtt <- plyr::rbind.fill(storage.rtt,RTT_month)
    }
  }

  #Save
  return(storage.rtt)
  
}


#put_object(file = 'RTT_allmonths_new.csv',
#           object = 'RTT_waiting_times_data_t/RTT_allmonths_new.csv',
#           bucket = 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp', show_progress = TRUE,
#           multipart=TRUE)

#put_object(file = 'IS_providers_allmonths.csv',
#           object = 'RTT_waiting_times_data_t/IS_providers_allmonths.csv',
#           bucket = 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp', show_progress = TRUE,
#           multipart=TRUE)

#put_object(file = 'providers_allmonths.csv',
#           object = 'RTT_waiting_times_data_t/providers_allmonths.csv',
#           bucket = 'thf-dap-tier0-projects-iht-067208b7-projectbucket-1mrmynh0q7ljp', show_progress = TRUE,
#           multipart=TRUE)