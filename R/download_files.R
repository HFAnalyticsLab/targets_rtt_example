## Download all files in data
dl_files <- function(data){
  
  local_save <- 'RTT_temp_data/temp_files/'
  
  for (k in 1:nrow(data)){
    
    #Download Full CSV in workbench
    download(as.character(data$full.csv.link[k]),
             dest = paste0(local_save, data$month[k], '.zip'), mode = 'wb')
    #Unzip Full CSV in workbench
    unzip(paste0(local_save, data$month[k], '.zip'),
          exdir = paste0(local_save, data$month[k]))
    #Delete zip file
    file.remove(paste0(local_save, data$month[k], '.zip'))
    
    #Download New Providers
    download(as.character(data$providers.link.new[k]),
             dest = paste0(local_save,
                           paste(data$month[k]), '/',
                           data$month[k], '-newproviders.', 
                           file_ext(data$providers.link.new[k])),
             mode = 'wb')
    
    #Download Admitted Providers
    download(as.character(data$providers.link.adm[k]),
             dest = paste0(local_save,
                           paste(data$month[k]), '/',
                           data$month[k], '-providers-admitted.',
                           file_ext(data$providers.link.adm[k])),
             mode = 'wb')
    
    #Download Non-Admitted Providers
    download(as.character(data$providers.link.nonadm[k]),
             dest = paste0(local_save,
                           paste(data$month[k]), '/',
                           data$month[k], '-providers-nonadmitted.',
                           file_ext(data$providers.link.nonadm[k])), 
             mode = 'wb')
    
    ### Incomplete providers
    
    #Download Incomplete Providers
    download(as.character(data$providers.link.incomp[k]),
             dest=paste0(local_save,
                         paste(data$month[k]),'/',
                         data$month[k],'-providers-incomplete.',
                         tools::file_ext(data$providers.link.incomp[k])),
             mode='wb') 
  }
  
  return(
    list.files(local_save, recursive = TRUE)
  )
  
}
