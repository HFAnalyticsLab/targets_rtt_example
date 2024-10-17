## Append all IS provider files into one
append_IS <- function(data){
  
  for (s in 1:nrow(data)){
    
    init <- paste0('RTT_temp_data/temp files/', data$month[s], '/', data$month[s])
    
    #Open all provider files for one month and append
    incomplete <- read_excel(paste0(init,
                                    "-providers-incomplete.",
                                    file_ext(data$providers.link.incomp[s])),
                             sheet = "IS Provider",skip=13)
    
    incompleteDTA <- read_excel(paste0(init,
                                       "-providers-incomplete.",
                                       file_ext(data$providers.link.incomp[s])),
                                sheet = "IS Provider with DTA",skip=13)
    
    new_provider <- read_excel(paste0(init,
                                      "-newproviders.",
                                      file_ext(data$providers.link.new[s])),
                               sheet = "IS Provider",skip=13)
    
    adm_provider <- read_excel(paste0(init,
                                      "-providers-admitted.",
                                      file_ext(data$providers.link.adm[s])),
                               sheet = "IS Provider",skip=13)
    
    nonadm_provider <- read_excel(paste0(init,
                                         "-providers-nonadmitted.",
                                         file_ext(data$providers.link.nonadm[s])),
                                  sheet = "IS Provider",skip=13)
    
    #IS providers for that month
    
    codes <- c(new_provider$`Provider Code`,adm_provider$`Provider Code`,nonadm_provider$`Provider Code`,
               incomplete$`Provider Code`,incompleteDTA$`Provider Code`)
    names <- c(new_provider$`Provider Name`,adm_provider$`Provider Name`,nonadm_provider$`Provider Name`,
               incomplete$`Provider Name`,incompleteDTA$`Provider Name`)
    region <- c(new_provider$`Region Code`,adm_provider$`Region Code`,nonadm_provider$`Region Code`,
                incomplete$`Region Code`,incompleteDTA$`Region Code`)
    summary_month <- data.frame(monthyr=rep(as.character(data$month[s]),length(codes)),codes,names,region)
    
    #Successively append files
    
    if (s==1) {
      storage <- summary_month
    } else {
      storage <- plyr::rbind.fill(storage,summary_month)
    }
  }

  #Remove duplicates
  IS_providers_allmonths <- storage[!duplicated(storage), ]
  
  #Save
  fwrite(IS_providers_allmonths, file = paste0(R_workbench,"/RTT_temp_data/",
                                               "/IS_providers_allmonths.csv"), sep = ",")

}

#### Get all NHS provider locations / region
append_geo <- function(data){
  
  for (s in 1:nrow(data)){
    
    init <- paste0('RTT_temp_data/temp files/', data$month[s], '/', data$month[s])
    
    #Open all provider files for one month and append
    incomplete <- read_excel(paste0(init,
                                    "-providers-incomplete.",
                                    file_ext(data$providers.link.incomp[s])),
                             sheet = "Provider",skip=13)
    
    incompleteDTA <- read_excel(paste0(init,
                                       "-providers-incomplete.",
                                       file_ext(data$providers.link.incomp[s])),
                                sheet = "Provider with DTA",skip=13)
    
    new_provider <- read_excel(paste0(init,
                                      "-newproviders.",
                                      file_ext(data$providers.link.new[s])),
                               sheet = "Provider",skip=13)
    
    adm_provider <- read_excel(paste0(init,
                                      "-providers-admitted.",
                                      file_ext(data$providers.link.adm[s])),
                               sheet = "Provider",skip=13)
    
    nonadm_provider <- read_excel(paste0(init,
                                         "-providers-nonadmitted.",
                                         file_ext(data$providers.link.nonadm[s])),
                                  sheet = "Provider",skip=13)
    
    #IS providers for that month
    
    codes <- c(new_provider$`Provider Code`,adm_provider$`Provider Code`,nonadm_provider$`Provider Code`,
               incomplete$`Provider Code`,incompleteDTA$`Provider Code`)
    names <- c(new_provider$`Provider Name`,adm_provider$`Provider Name`,nonadm_provider$`Provider Name`,
               incomplete$`Provider Name`,incompleteDTA$`Provider Name`)
    region <- c(new_provider$`Region Code`,adm_provider$`Region Code`,nonadm_provider$`Region Code`,
                incomplete$`Region Code`,incompleteDTA$`Region Code`)
    summary_month <- data.frame(monthyr=rep(as.character(data$month[s]),length(codes)),codes,names,region)
    
    #Successively append files
    
    if (s==1) {
      storage <- summary_month
    } else {
      storage <- plyr::rbind.fill(storage,summary_month)
    }
  }
  
  #Remove duplicates
  providers_allmonths <- storage[!duplicated(storage), ]

  #Save
  fwrite(providers_allmonths, file = paste0(R_workbench,"/RTT_temp_data/",
                                            "/providers_allmonths.csv"), sep = ",")

}