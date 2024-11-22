## install all required packages

# code for subnational GNI per capita (and other HDI component) data creation
# creator: Matti Kummu, Aalto University (matti.kummu@aalto.fi)

# List of required packages
required_packages <- c("openxlsx", "zoo", "broom", "sf", "terra", 
                       "tidyverse", "dplyr", "purrr", "mblm", "tidyterra")

# Function to check and install missing packages
check_and_install <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) install.packages(new_packages)
  sapply(packages, require, character.only = TRUE)
}

# Check and install required packages
check_and_install(required_packages)

