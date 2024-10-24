## Append all providers into one dataset
append_providers <- function(file_locs, prov = 'IS Provider'){
  
  ## get months from file locs
  unique_month <- substr(file_locs, 1, 5) |>
                  unique()
  
  for (s in 1:length(unique_month)){
    
    init <- paste0('RTT_temp_data/temp_files/', unique_month[s], '/')
    month_files <- paste0(init, list.files(init))
    
    #Open all provider files for one month and append
    incomplete <- read_excel(grep('-providers-incomplete', month_files, value = TRUE),
                             sheet = prov,
                             skip = 13)
    
    incompleteDTA <- read_excel(grep('-providers-incomplete', month_files, value = TRUE),
                                sheet = paste0(prov, ' with DTA'),
                                skip = 13)
    
    new_provider <- read_excel(grep('-newproviders', month_files, value = TRUE),
                               sheet = prov,
                               skip = 13)
    
    adm_provider <- read_excel(grep('-providers-admitted', month_files, value = TRUE),
                               sheet = prov,
                               skip = 13)
    
    nonadm_provider <- read_excel(grep('-providers-nonadmitted', month_files, value = TRUE),
                                  sheet = prov,
                                  skip = 13)
    
    #IS providers for that month
    
    codes <- c(new_provider$`Provider Code`, adm_provider$`Provider Code`, nonadm_provider$`Provider Code`,
               incomplete$`Provider Code`, incompleteDTA$`Provider Code`)
    names <- c(new_provider$`Provider Name`, adm_provider$`Provider Name`, nonadm_provider$`Provider Name`,
               incomplete$`Provider Name`, incompleteDTA$`Provider Name`)
    region <- c(new_provider$`Region Code`, adm_provider$`Region Code`, nonadm_provider$`Region Code`,
                incomplete$`Region Code`, incompleteDTA$`Region Code`)
    summary_month <- data.frame(monthyr=rep(unique_month[s],length(codes)),codes,names,region) |> unique()
    
    #Successively append files
    
    if (s==1) {
      storage <- summary_month
    } else {
      storage <- plyr::rbind.fill(storage,summary_month)
    }
  }

  #Remove duplicates
  is_prov <- storage[!duplicated(storage), ]
  
  # filename
  if(prov == 'IS Provider'){
    name <- 'RTT_temp_data/IS_prov.csv'
  } else {
    name <- 'RTT_temp_data/NHS_prov.csv'
  }
  
  
  #Save - include time in name to check in targets
  fwrite(is_prov, file = name)
  return(name) ## target name:app_IS
               ## is location of is_prov file (will need to clear out the others?)
}