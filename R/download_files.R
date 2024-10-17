## Download all files in links_out_df
dl_files <- function(links_out_df){
  
  for (k in 1:nrow(links_out_df)){
    
    #Download Full CSV in workbench
    download(as.character(links_out_df$full.csv.link[k]),
             dest=paste0("RTT_temp_data/temp files/",links_out_df$month[k],".zip"), mode="wb")
    #Unzip Full CSV in workbench
    unzip(paste0("RTT_temp_data/temp files/",links_out_df$month[k],".zip"),
          exdir = paste0("RTT_temp_data/temp files/",links_out_df$month[k]))
    #Delete zip file
    file.remove(paste0("RTT_temp_data/temp files/",links_out_df$month[k],".zip"))
    
    #Download New Providers
    download(as.character(links_out_df$providers.link.new[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(links_out_df$month[k]),"/",
                         links_out_df$month[k],"-newproviders.", 
                         tools::file_ext(links_out_df$providers.link.new[k])),
             mode="wb")
    
    #Download Admitted Providers
    download(as.character(links_out_df$providers.link.adm[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(links_out_df$month[k]),"/",
                         links_out_df$month[k],"-providers-admitted.",
                         tools::file_ext(links_out_df$providers.link.adm[k])),
             mode="wb")
    
    #Download Non-Admitted Providers
    download(as.character(links_out_df$providers.link.nonadm[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(links_out_df$month[k]),"/",
                         links_out_df$month[k],"-providers-nonadmitted.",
                         tools::file_ext(links_out_df$providers.link.nonadm[k])), 
             mode="wb")
    
    ### Incomplete providers
    
    #Download Incomplete Providers
    download(as.character(links_out_df$providers.link.incomp[k]),
             dest=paste0("RTT_temp_data/temp files/",
                         paste(links_out_df$month[k]),"/",
                         links_out_df$month[k],"-providers-incomplete.",
                         tools::file_ext(links_out_df$providers.link.incomp[k])),
             mode="wb") 
  }
}
