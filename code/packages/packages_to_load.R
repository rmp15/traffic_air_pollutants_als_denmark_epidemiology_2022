# list of packages to use
list.of.packages = c("car", "caret", "cowplot", "dlnm", "EnvStats", 
                     "expss", "extrafont", "fBasics", "forestplot", "GGally", 
                     "ggplot2", "ggsn", "grid", "gridExtra", "gt", 
                     "gtsummary", "haven", "itsadug","janitor", "lubridate", 
                     "MASS", "mgcv", "naniar", "nortest", "olsrr", 
                     "plotly", "pROC", "pspline", "raster", "readr", 
                     "reshape", "reshape2", "rpart", "rpart.plot", "rstan", 
                     "rstanarm", "sf", "splines", "survival", "tableone",   
                     "tidyverse", "xlsx")

# check if list of packages is installed. If not, it will install ones not yet installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) invisible(install.packages(new.packages,repos = "https://cloud.r-project.org"))

# quietly load packages
invisible(lapply(list.of.packages, require, character.only = TRUE, quietly=TRUE))




