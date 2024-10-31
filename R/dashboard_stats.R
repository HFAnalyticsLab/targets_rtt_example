
dashboard_stats <- function(combined_dataset,
                            monthyear,
                            specialty,
                            type,
                            independent,
                            quantiles = c(0.95, 0.50)){
  
  x <- open_dataset(combined_dataset)

    dataset <- filter(x, monthyr == monthyear & Commissioner.Org.Code != 'NONC') |>
               collect()
    
    #Which wait-time columns are filled in?
    GT_names <- dataset |>
      select(all_of(starts_with('Gt'))) |>
      select(where(not_all_na)) |>
      names()
    
    #Filter based on type of provider
    
    if (independent == 0){
      monthly_data <- filter(dataset, IS_provider == 0)
      IS <- 'Non-IS'
    } else if (independent == 1) {
      monthly_data <- filter(dataset, IS_provider == 1)
      IS <- 'IS'
    } else if (independent == 2) {
      monthly_data <- dataset
      IS <- 'All'
    }
    
    #Aggregate relevant variables
    
    if (type == 'incomplete'){
      datasubset <- filter(monthly_data,
                           Treatment.Function.Name == specialty &
                           RTT.Part.Description == 'Incomplete Pathways')
    } else if (type == 'completeadmitted'){
      datasubset <- filter(monthly_data,
                           Treatment.Function.Name==specialty &
                           RTT.Part.Description == 'Completed Pathways For Admitted Patients')
    } else if (type == 'completenonadmitted'){
      datasubset <- filter(monthly_data,
                           Treatment.Function.Name == specialty &
                           RTT.Part.Description == 'Completed Pathways For Non-Admitted Patients')
    } else if (type == 'incompleteDTA'){
      datasubset <- filter(monthly_data,
                           Treatment.Function.Name == specialty &
                           RTT.Part.Description == 'Incomplete Pathways with DTA')
    } else if (type == 'newRTT'){
      datasubset <- filter(monthly_data,
                           Treatment.Function.Name == specialty &
                           RTT.Part.Description == 'New RTT Periods - All Patients')
    }
    
    #Aggregate rows and compute total patients
    if (type == 'completeadmitted' | type == 'completenonadmitted'){
      
      datasubset_sum <- datasubset |>
        select(all_of(GT_names)) |>
        summarise(across(starts_with(c('Gt')), sumnarm)) |> t()
      
      datasubset_unknown <- datasubset |>
        summarise(across(starts_with(c('Patients.with.unknown.clock.start.date')), sumnarm)) |> t()
      
      total <- sum(datasubset_sum, na.rm = TRUE) + sum(datasubset_unknown, na.rm = TRUE)
      
      total.nonmiss <- sum(datasubset_sum, na.rm = TRUE)
      
    } else if (type == 'incomplete' | type == 'incompleteDTA'){
      
      datasubset_sum <- datasubset |>
        select(all_of(GT_names)) |>
        summarise(across(starts_with(c('Gt')), sumnarm)) |> t()
      
      total <- sum(datasubset_sum, na.rm = TRUE)
      total.nonmiss <- total
      
    } else if (type == 'newRTT'){
      
      total <- datasubset |> select(Total.All) |> sum(na.rm = TRUE)
      
      total.nonmiss <- total
    }
    
    #Only compute quantiles if there are more than 20 patients
    #Compute them in 3 different ways: Total, IS and non-IS
    if (total.nonmiss >= 20 & type != 'newRTT'){
      #Return weeks (for a given quantile)
      
      weeks <- rep(NA, length(quantiles))
      
      for (j in 1:length(quantiles)){
        
        target <- quantiles[j] * total.nonmiss
        
        auxmat <- data.frame(weeks = 1:nrow(datasubset_sum),
                             counts = datasubset_sum,
                             cumsum = cumsum(datasubset_sum)) |>
          mutate(above = ifelse(cumsum >= target, 1, 0),
                 difference = (cumsum - target))
        
        weeks[j] <- (filter(auxmat, above == 1) |>
                       select(weeks) |> min()) - 1
      }
      
      weeks <- as.data.frame(weeks) |> t()
      colnames(weeks) <- paste0('weeks.', quantiles * 100)
      
      #Return % of patients waiting 18 weeks or less
      
      number_52_or_less <- datasubset_sum[1:52, ] |> sum(na.rm = TRUE) |> unlist()
      number_52_or_more <- total.nonmiss - number_52_or_less
      rate_52_or_more <- round(number_52_or_more / total.nonmiss * 100, 1) |> unlist()
      
      number_18_or_less <- cumsum(datasubset_sum)[18] |> unlist()
      rate_18_or_less <- round(number_18_or_less / total.nonmiss * 100, 1) |> unlist()
      
      #Function output
      output <- data.frame(monthyear = as.character(monthyear),
                           specialty = as.character(specialty),
                           type = as.character(type),
                           independent = as.character(IS),
                           total.patients = total,
                           number.18.or.less = number_18_or_less,
                           rate.18wks.or.less = rate_18_or_less,
                           number.52.or.more = number_52_or_more,
                           rate.52wks.or.more = rate_52_or_more,
                           weeks)
      
    } else if (total.nonmiss < 20 | type == 'newRTT') {
      weeks <- rep(NA, length(quantiles))
      
      weeks <- as.data.frame(weeks) |> t()
      
      colnames(weeks) <- paste0('weeks.', quantiles * 100)
      
      output <- data.frame(monthyear = as.character(monthyear),
                           specialty = as.character(specialty),
                           type = as.character(type),
                           independent = as.character(IS),
                           total.patients = total,
                           number.18.or.less = NA,
                           rate.18wks.or.less = NA,
                           number.52.or.more = NA,
                           rate.52wks.or.more = NA,
                           weeks)
    }
  
  return(output)
}
