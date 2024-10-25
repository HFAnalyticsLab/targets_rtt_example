
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

  ## first cleaning steps (usually performed in "2. Produce descriptive statistics" but should be done before writing data)
  #### Create pathways variable
  
  storage.rtt <- storage.rtt |>
    mutate(pathways = case_when(
      RTT.Part.Description == 'Incomplete Pathways' ~ 'incomplete',
      RTT.Part.Description == 'Completed Pathways For Admitted Patients' ~ 'completeadmitted',
      RTT.Part.Description == 'Completed Pathways For Non-Admitted Patients' ~ 'completenonadmitted',
      RTT.Part.Description == 'Incomplete Pathways with DTA' ~ 'incompleteDTA',
      RTT.Part.Description == 'New RTT Periods - All Patients' ~ 'newRTT',
      TRUE ~ 'NA'
    ))
  
  #### Clean up specialty names (monitor for new ones)
  
  storage.rtt <- storage.rtt |>
    mutate(Treatment.Function.Name = str_replace_all(Treatment.Function.Name,' Service','')) |>
    mutate(Treatment.Function.Name = ifelse(Treatment.Function.Name == 'Ear, Nose & Throat (ENT)','Ear Nose and Throat',Treatment.Function.Name)) |>
    mutate(Treatment.Function.Name = ifelse(Treatment.Function.Name == 'Geriatric Medicine','Elderly Medicine',Treatment.Function.Name)) |> 
    mutate(Treatment.Function.Name = ifelse(Treatment.Function.Name == 'Neurosurgical','Neurosurgery',Treatment.Function.Name)) |> 
    mutate(Treatment.Function.Name = ifelse(Treatment.Function.Name == 'Trauma & Orthopaedics','Trauma and Orthopaedic',Treatment.Function.Name)) |> 
    mutate(Treatment.Function.Name = ifelse(Treatment.Function.Name == 'Other - Medicals','Other',Treatment.Function.Name)) |> 
    mutate(Treatment.Function.Name = ifelse(Treatment.Function.Name == 'Other - Mental Healths','Other',Treatment.Function.Name)) |> 
    mutate(Treatment.Function.Name = ifelse(Treatment.Function.Name == 'Other - Others','Other',Treatment.Function.Name)) |> 
    mutate(Treatment.Function.Name = ifelse(Treatment.Function.Name == 'Other - Paediatrics','Other',Treatment.Function.Name)) |> 
    mutate(Treatment.Function.Name = ifelse(Treatment.Function.Name == 'Other - Surgicals','Other',Treatment.Function.Name))

    #Save
  write_dataset(storage.rtt,
                paste0('s3://', IHT_bucket, '/arrow_rtt'),
                partitioning = 'monthyr')
  
  return(paste0('s3://', IHT_bucket, '/arrow_rtt')) # if written correctly
  
}
