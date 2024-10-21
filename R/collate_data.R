
IS_providers_allmonths <- fread('RTT_temp_data/IS_providers_allmonths.csv',
                                header=TRUE, sep=",", check.names=T)

#Find name of large csvs
file.name <- list.files('RTT_temp_data', recursive = TRUE)[
              str_detect(list.files('RTT_temp_data', recursive = TRUE), 'full-extract')]

#Append all files
for (j in 1:nrow(data)){
  
  #Read in
  RTT_month <- fread(file.name[j], header = TRUE, sep = ',', check.names = T)

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