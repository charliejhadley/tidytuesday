# Run this
source("data/curated/curation_scripts.R")

# Fill in the name of the folder you created in "curated", then run this.
dir_name <- "motorway-services-uk"

# Run this for each of your datasets, replacing YOUR_DATASET_DF with the name of
# a data.frame from cleaning.R.
ttsave(data_services_retailers, dir_name = dir_name)
ttsave(data_services_info, dir_name = dir_name)
ttsave(data_service_locations, dir_name = dir_name)
ttsave(data_operators, dir_name = dir_name)
