# Main script
# 02.09.2019
# Jacob R. Mortensen
# BMP experiment
# Updated: 13.10.2019

# Load packages

source("package_fg.R")

# Read in data

source("read_fg.R")

# Data manipulation

source("data_mani_fg.R")

# Detection limit for water bottle correction

source("water_detection_limit_fg.R")

# Scale drift correction and original measurements

source("scale_drift_fg.R")

# Leaks in bottles

source("leaks_fg.R")

# Obtaining biogas - Not working at the moment

source("biogas_fg.R")

# Add gc data to dataframe

source("gc_fg.R")

# Calculate BMP for each experiment

source("BMP_fg.R")

# Plots

source("plots_fg.R")

# Output tables etc.

source("output_fg.R")