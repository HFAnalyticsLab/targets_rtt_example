###########################################################################
################### Append all IS provider files into one #################
###########################################################################

for (s in 1:nrow(links_out_df)){
  
  #Open all provider files for one month and append
  incomplete <- read_excel(paste0(links_out_df$month[s],"-providers-incomplete.",
                                  tools::file_ext(links_out_df$providers.link.incomp[s])),
                           sheet = "IS Provider",skip=13)
  
  incompleteDTA <- read_excel(paste0(links_out_df$month[s],"-providers-incomplete.",
                                     tools::file_ext(links_out_df$providers.link.incomp[s])),
                              sheet = "IS Provider with DTA",skip=13)
  
  new_provider <- read_excel(paste0(links_out_df$month[s],"-newproviders.",
                                    tools::file_ext(links_out_df$providers.link.new[s])),
                             sheet = "IS Provider",skip=13)
  
  adm_provider <- read_excel(paste0(links_out_df$month[s],"-providers-admitted.",
                                    tools::file_ext(links_out_df$providers.link.adm[s])),
                             sheet = "IS Provider",skip=13)
  
  nonadm_provider <- read_excel(paste0(links_out_df$month[s],"-providers-nonadmitted.",
                                       tools::file_ext(links_out_df$providers.link.nonadm[s])),
                                sheet = "IS Provider",skip=13)
  
  #IS providers for that month
  
  codes <- c(new_provider$`Provider Code`,adm_provider$`Provider Code`,nonadm_provider$`Provider Code`,
             incomplete$`Provider Code`,incompleteDTA$`Provider Code`)
  names <- c(new_provider$`Provider Name`,adm_provider$`Provider Name`,nonadm_provider$`Provider Name`,
             incomplete$`Provider Name`,incompleteDTA$`Provider Name`)
  region <- c(new_provider$`Region Code`,adm_provider$`Region Code`,nonadm_provider$`Region Code`,
              incomplete$`Region Code`,incompleteDTA$`Region Code`)
  summary_month <- data.frame(monthyr=rep(as.character(links_out_df$month[s]),length(codes)),codes,names,region)
  rm(incomplete,incompleteDTA,new_provider,adm_provider,nonadm_provider,codes,names,region)
  
  #Successively append files
  
  if (s==1) {
    storage <- summary_month
  } else {
    storage <- plyr::rbind.fill(storage,summary_month)
  }
}

#Remove duplicates
IS_providers_allmonths <- storage[!duplicated(storage), ]
rm(storage,summary_month)

#Save
fwrite(IS_providers_allmonths, file = paste0(R_workbench,"/RTT_temp_data/",
                                             "/IS_providers_allmonths.csv"), sep = ",")
rm(IS_providers_allmonths)

#### Get all NHS provider locations / region
for (s in 1:nrow(links_out_df)){
  
  #Open all provider files for one month and append
  incomplete <- read_excel(paste0(links_out_df$month[s],"-providers-incomplete.",
                                  tools::file_ext(links_out_df$providers.link.incomp[s])),
                           sheet = "Provider",skip=13)
  
  incompleteDTA <- read_excel(paste0(links_out_df$month[s],"-providers-incomplete.",
                                     tools::file_ext(links_out_df$providers.link.incomp[s])),
                              sheet = "Provider with DTA",skip=13)
  
  new_provider <- read_excel(paste0(links_out_df$month[s],"-newproviders.",
                                    tools::file_ext(links_out_df$providers.link.new[s])),
                             sheet = "Provider",skip=13)
  
  adm_provider <- read_excel(paste0(links_out_df$month[s],"-providers-admitted.",
                                    tools::file_ext(links_out_df$providers.link.adm[s])),
                             sheet = "Provider",skip=13)
  
  nonadm_provider <- read_excel(paste0(links_out_df$month[s],"-providers-nonadmitted.",
                                       tools::file_ext(links_out_df$providers.link.nonadm[s])),
                                sheet = "Provider",skip=13)
  
  #IS providers for that month
  
  codes <- c(new_provider$`Provider Code`,adm_provider$`Provider Code`,nonadm_provider$`Provider Code`,
             incomplete$`Provider Code`,incompleteDTA$`Provider Code`)
  names <- c(new_provider$`Provider Name`,adm_provider$`Provider Name`,nonadm_provider$`Provider Name`,
             incomplete$`Provider Name`,incompleteDTA$`Provider Name`)
  region <- c(new_provider$`Region Code`,adm_provider$`Region Code`,nonadm_provider$`Region Code`,
              incomplete$`Region Code`,incompleteDTA$`Region Code`)
  summary_month <- data.frame(monthyr=rep(as.character(links_out_df$month[s]),length(codes)),codes,names,region)
  rm(incomplete,incompleteDTA,new_provider,adm_provider,nonadm_provider,codes,names,region)
  
  #Successively append files
  
  if (s==1) {
    storage <- summary_month
  } else {
    storage <- plyr::rbind.fill(storage,summary_month)
  }
}

#Remove duplicates
providers_allmonths <- storage[!duplicated(storage), ]
rm(storage,summary_month)

#Save
fwrite(providers_allmonths, file = paste0(R_workbench,"/RTT_temp_data/",
                                          "/providers_allmonths.csv"), sep = ",")
rm(IS_providers_allmonths)
