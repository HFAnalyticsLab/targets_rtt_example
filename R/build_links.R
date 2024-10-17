### Functions (JH 2024)

## builder to get links setup for dl
year_lkup <- function(y, l=12){
  
  m <- c("Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar")
  year <- c(rep(y,9),rep(y+1,3))
  series <- rep(as.character(paste0(y, y+1)), 12)
  return(
    cbind.data.frame(month=paste0(m,year),series=series) |>
      head(l)
  )
  
}

## Combine together for all files/dates to dl
file_dates_df <- function(m, y = 24, yb = 6){
  
  a <- lapply(FUN = year_lkup, c((y - 1):(y - yb))) |>
        rbindlist()
  
  return(rbind(year_lkup(y, m), a))
  
}

#Function that reports links to 5 files for each month
return_links_rtt <- function(month, series){
  
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

## create df of all links
links_out_df <- function(m){
  
  mapply(return_links_rtt,
         month = file_dates_df(m)$month,
         series = file_dates_df(m)$series) |>
  t() |>              
  as.data.frame() |>
  unnest(cols = c(month, series, full.csv.link, providers.link.incomp, providers.link.new,
                  providers.link.adm, providers.link.nonadm)) |>
  filter(!is.na(full.csv.link)) #Filter out months that haven't been uploaded yet or don't exist
}