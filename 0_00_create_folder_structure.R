rm(list=ls())

# Create Folder Structure
# Long-term traffic-related air pollutant exposure and amyotrophic lateral sclerosis diagnosis in Denmark: a Bayesian hierarchical analysis
# Parks et al.

# 0a Load Packages
library(here)

####********************************
#### 1: Create Folder Structure #### 
####********************************

# 1a Declare directories (can add to over time)
project.folder <- paste0(print(here::here()),'/')
  code.folder <- paste0(project.folder, "code/")
    data.prep.code.folder <- paste0(code.folder, "data_prep/")
    packages.folder <- paste0(code.folder, "packages/")
    models.folder <- paste0(code.folder, "models/")
    model.processing.folder <- paste0(code.folder, "model_processing/")
  data.folder <- paste0(project.folder, "data/")
    file.locations.folder <- paste0(data.folder, "file_locations/")
    parish.ses.folder <- paste0(data.folder, "parish_ses/")
  output.folder <- paste0(project.folder, "output/")
    output.no2.folder <- paste0(output.folder, "output_no2/")
  figures.folder <- paste0(project.folder, "figures/")
    figure1.folder <- paste0(figures.folder,"figure1/")
  tables.folder <- paste0(project.folder, "tables/")
      table1.folder <- paste0(tables.folder, "table1/")
      table2.folder <- paste0(tables.folder, "table2/")
      table3.folder <- paste0(tables.folder, "table3/")
  reports.folder <- paste0(project.folder, "reports/")

# 1b Identify list of folder locations which have just been created above
folders.names <- grep(".folder",names(.GlobalEnv),value=TRUE)

# 1c Create function to create list of folders
# note that the function will not create a folder if it already exists 
create_folders <- function(name){
  ifelse(!dir.exists(get(name)), dir.create(get(name), recursive=TRUE), FALSE)
}

# 1d Create the folders named above
lapply(folders.names, create_folders)

# Lizzy: this is nice! I will borrow this setup for my next project :)

