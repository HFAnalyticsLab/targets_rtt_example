## Download all files in data
dl_files <- function(data){
  
  for (k in 1:nrow(data)){
    
    #Download Full CSV in workbench
    download(as.character(data$full.csv.link[k]),
             dest=paste0("RTT_temp_data/temp files/",data$month[k],".zip"), mode="wb")
    #Unzip Full CSV in workbench
    unzip(paste0("RTT_temp_data/temp files/",data$month[k],".zip"),
          exdir = paste0("RTT_temp_data/temp files/",data$month[k]))
    #Delete zip file
    file.remove(paste0("RTT_temp_data/temp files/",data$month[k],".zip"))
    
    #Download New Providers
    download(as.character(data$providers.link.new[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(data$month[k]),"/",
                         data$month[k],"-newproviders.", 
                         tools::file_ext(data$providers.link.new[k])),
             mode="wb")
    
    #Download Admitted Providers
    download(as.character(data$providers.link.adm[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(data$month[k]),"/",
                         data$month[k],"-providers-admitted.",
                         tools::file_ext(data$providers.link.adm[k])),
             mode="wb")
    
    #Download Non-Admitted Providers
    download(as.character(data$providers.link.nonadm[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(data$month[k]),"/",
                         data$month[k],"-providers-nonadmitted.",
                         tools::file_ext(data$providers.link.nonadm[k])), 
             mode="wb")
    
    ### Incomplete providers
    
    #Download Incomplete Providers
    download(as.character(data$providers.link.incomp[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(data$month[k]),"/",
                         data$month[k],"-providers-incomplete.",
                         tools::file_ext(data$providers.link.incomp[k])),
             mode="wb") 
  }
}
