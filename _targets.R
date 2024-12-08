# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed.

# Set target options:
tar_option_set(
  packages = c('tibble', 'rvest', 'stringr', 'tidyr',
               'dplyr', 'downloader', 'data.table', 'readxl',
               'tools', 'aws.s3', 'arrow'), # Packages that your targets need for their tasks
  
  error = 'null' # produce a result even if the target errors
  
  # format = 'qs', # Optionally set the default storage format. qs is fast.
  #
  # Pipelines that take a long time to run may benefit from
  # optional distributed computing. To use this capability
  # in tar_make(), supply a {crew} controller
  # as discussed at https://books.ropensci.org/targets/crew.html.
  # Choose a controller that suits your needs. For example, the following
  # sets a controller that scales up to a maximum of two workers
  # which run as local R processes. Each worker launches when there is work
  # to do and exits if 60 seconds pass with no tasks to run.
  #
  #   controller = crew::crew_controller_local(workers = 2, seconds_idle = 60)
  #
  # Alternatively, if you want workers to run on a high-performance computing
  # cluster, select a controller from the {crew.cluster} package.
  # For the cloud, see plugin packages like {crew.aws.batch}.
  # The following example is a controller for Sun Grid Engine (SGE).
  # 
  #   controller = crew.cluster::crew_controller_sge(
  #     # Number of workers that the pipeline can scale up to:
  #     workers = 10,
  #     # It is recommended to set an idle time so workers can shut themselves
  #     # down if they are not running tasks.
  #     seconds_idle = 120,
  #     # Many clusters install R as an environment module, and you can load it
  #     # with the script_lines argument. To select a specific verison of R,
  #     # you may need to include a version string, e.g. 'module load R/4.3.2'.
  #     # Check with your system administrator if you are unsure.
  #     script_lines = 'module load R'
  #   )
  #
  # Set other options as needed.
)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# tar_source('other_functions.R') # Source other scripts as needed.

# Replace the target list below with your own:
list(
  tar_target(
    name = web_data,
    command = links_out_df(12) # months of current year with data
    # format = 'qs' # Efficient storage for general data objects.
  ),
  tar_target(
    name = file_locs,
    command = dl_files(web_data)
  ),
  tar_target(
    name = app_IS,
    command = append_providers(file_locs, prov = 'IS Provider') # prov is associated with the excel sheet names
  ),
#  tar_target(
#    name = app_geo,
#    command = append_providers(file_locs, prov = 'Provider')
#  ),
  tar_target(
    name = combined_dataset,
    command = collate_data(app_IS, file_locs)
  ),
  tar_target(
    name = process_adm,
    command = process_rtt(combined_dataset, type = 'completeadmitted', specialty = 'Total')
  ),
  tar_target(
    name = process_nonadm,
    command = process_rtt(combined_dataset, type = 'completenonadmitted', specialty = 'Total')
  ),
  tar_target(
    name = process_new,
    command = process_rtt(combined_dataset, type = 'newRTT', specialty = 'Total')
  ),
  tar_target(
    name = process_spec,
    command = process_specialties(combined_dataset)
  )
)