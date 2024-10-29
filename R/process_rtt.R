

process_rtt <- function(combined_dataset,
                        specialty = 'Total',
                        type){
  
  all_months <- get_months(combined_dataset)
  res <- list()
  j <- 1
  
  for (i in all_months){
    
    res[[j]] <- dashboard_stats_ccg(combined_dataset,
                                    monthyear = i,
                                    specialty = specialty,
                                    type = type,
                                    independent = 0)
    res[[j+1]] <- dashboard_stats_ccg(combined_dataset,
                                    monthyear = i,
                                    specialty = specialty,
                                    type = type,
                                    independent = 1)
    j <- j + 2
  }
 
  return(
    rbindlist(res)
  )
  
}
