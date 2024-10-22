
collate_data <- function(data){
  
  IS_providers_allmonths <- fread('RTT_temp_data/IS_providers_allmonths.csv',
                                  header=TRUE, sep=",", check.names=T)
  
  #Find name of large csvs
  file.name <- list.files('RTT_temp_data', recursive = TRUE)[
    str_detect(list.files('RTT_temp_data', recursive = TRUE), 'full-extract')]
  
  #Append all files
  for (j in 1:nrow(data)){
    
    #Read in
    RTT_month <- fread(file = paste0('RTT_temp_data/', file.name[j]),
                       header = TRUE, sep = ',', check.names = T)
    
    #Add month-year indicator
    RTT_month$monthyr <- as.character(data$month[j])
    
    #New indicator variable to flag independent providers
    RTT_month$IS_provider <- ifelse(
      RTT_month$Provider.Org.Code %in% filter(IS_providers_allmonths, monthyr == data$month[j])$codes, 1, 0)
    
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