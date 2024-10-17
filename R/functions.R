#####################################################
################### Web-scraping ####################
#####################################################
year_lkup <- function(y, l=12){
  
  m <- c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
  year <- c(rep(y,9),rep(y+1,3))
  series <- rep(as.character(paste0(y, y+1)), 12)
  return(
    cbind.data.frame(month=paste0(m,year),series=series) |>
      head(l)
  )
  
}

#All together
file_dates_df <- function(m, y = 24, yb = 6){
  
  a <- lapply(FUN = year_lkup, c((y-1):(y-yb))) |>
        rbindlist()
  
  return(rbind(year_lkup(y, m), a))
  
}

#Function that reports links to 3 files for each month

return_links_rtt <- function(month,series){
  
  read.first.page <- read_html(
                     paste0('https://www.england.nhs.uk/statistics/statistical-work-areas/rtt-waiting-times/rtt-data-20',
                            substr(series,1,2),
                            '-',
                            substr(series,3,4),
                            '/'))
                        
  #This expression will extract all links associated with any text that contains the name of the month (e.g. "Jan")
  #See xpath cheat sheet here 'https://cheatography.com/alexsiminiuc/cheat-sheets/xpath/'
  #Or tutorial here 'https://levelup.gitconnected.com/master-the-art-of-writing-xpath-for-web-scraping-c14e2f7ee130'
  
  xpath_month <- paste0("//a[contains(text(),'",month,"')]/@href")
  
  #These are all the links associated with this given month
  
  links <- read.first.page %>%
    html_nodes(xpath=xpath_month) %>%
    html_text()
  
  #We're only interested in 5 of those files
  
  #ZIP
  full.csv.link <- links[str_detect(links, "Full-CSV|full-extract")][1]
  
  #Provider-level files
  providers.link.incomp <- links[str_detect(links, "Incomplete-Provider")][1]
  providers.link.adm <- links[str_detect(links, "Admitted-Provider")][1]
  providers.link.nonadm <- links[str_detect(links, "NonAdmitted-Provider")][1]
  providers.link.new <- links[str_detect(links, "New-Periods-Provider")][1]
  
  #Data frame of files to download
  out <- data.frame(month=month,
                    series=series,
                    full.csv.link,
                    providers.link.incomp,
                    providers.link.new,
                    providers.link.adm,
                    providers.link.nonadm)
  
  return(out)
  
}

#Example
#return_links_rtt("Jul","2122")

#Apply for all months to get all links, and store there is 'links_out_df'

links_out_df <- mapply(return_links_rtt,
                       month = file_dates_df(m=5)$month,
                       series = file_dates_df(m=5)$series) |>
                t() |>              
                as.data.frame() |>
                unnest(cols = c(month, series, full.csv.link, providers.link.incomp, providers.link.new,
                                providers.link.adm, providers.link.nonadm)) |>
                filter(!is.na(full.csv.link)) #Filter out months that haven't been uploaded yet or don't exist


###########################################################
################### Download all files ####################
###########################################################
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
