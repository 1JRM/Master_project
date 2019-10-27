# Main script
# 02.09.2019
# Jacob R. Mortensen
# BMP experiment
# Updated: 13.10.2019

# Load packages

source("package_BMP.R")

# Read in data

source("read_BMP.R")

# Data manipulation

source("data_mani_BMP.R")

# Detection limit for water bottle correction

source("water_detection_limit_BMP.R")

# Scale drift correction and original measurements

source("scale_drift_BMP.R")

# Leaks in bottles

source("leaks_BMP.R")

# Obtaining biogas - Not working at the moment

source("biogas_BMP.R")

# Add gc data to dataframe

source("gc_BMP.R")

# Calculate BMP for each experiment

source("BMP_BMP.R")

# Plots

source("plots_BMP.R")

# Output tables etc.

source("output_BMP.R")