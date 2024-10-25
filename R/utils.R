#Useful functions
#only_letters <- function(x) { gsub("^([[:alpha:]]*).*$","\\1",x) }
sumnarm <- function(x) { sum(x,na.rm=TRUE) }
not_all_na <- function(x) any(!is.na(x))

get_months <- function(combined_dataset){
  
  return(
    open_dataset(combined_dataset) |> 
      select(monthyr) |> 
      unique() |>
      collect() |>
      unlist()
  )
}
