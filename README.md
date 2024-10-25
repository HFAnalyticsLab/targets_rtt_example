# targets_rtt_example
Utilising the targets package for a streamlined data pipeline in R for RTT data

## test targets pipeline
```r
library(targets)

tar_manifest() # list verbose info about targets
tar_visnetwork() # pipeline dependancy graph
tar_make()  # run the pipeline (reproducible new external R process which then reads the target script
            # and runs the correct targets in the correct order)
```

Note: you will need to create a script `R/s3_locations.R` which sets the target bucket name called IHT_bucket