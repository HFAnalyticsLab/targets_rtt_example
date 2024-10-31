
process_specialties <- function(combined_dataset){
  
  n <- 1
  
  data_list <- list()
  
  data_pathway <- c('completeadmitted', 'completenonadmitted', 'newRTT')
  # specialities removed: (need combining or missing data)
  # Cardiothoracic Surgery
  # Elderly Medicine
  # General Internal Medicine, General Medicine
  # Respiratory Medicine, Thoracic Medicine
  
  all_specialties <- c('Cardiology', 'Dermatology', 'Ear Nose and Throat', 'Gastroenterology', 
                       'General Surgery', 'Gynaecology', 'Neurology', 'Neurosurgery', 
                       'Ophthalmology', 'Oral Surgery', 'Other', 'Plastic Surgery', 
                       'Rheumatology', 'Trauma and Orthopaedic', 'Urology')
  
  ## save processed data
  for (i in data_pathway){
    
    for (j in all_specialties){
      
      data_list[[n]] <- process_rtt(combined_dataset, specialty = j, type = i)
      
      print(paste0('Processed ', n, ' of ', length(data_pathway) * length(all_specialties)))
      
      n <- n + 1
      
    }
    
  }
  
  return(
    rbindlist(data_list)
  )
  
}