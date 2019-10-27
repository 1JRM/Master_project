# Main script
# 02.09.2019
# Jacob R. Mortensen
# Batch experiment
# Updated: 05.09.2019

# Load packages

source("package.R")

# Read in data

source("read.R")

# Data manipulation

source("data_mani.R")

# Detection limit for water bottle correction

source("water_detection_limit.R")

# Scale drift correction and original measurements

source("scale_drift.R")

# Leaks in bottles

source("leaks.R")

# Obtaining biogas - Not working at the moment

source("biogas.R")

# Add gc data to dataframe

source("gc.R")

# Calculate BMP for each experiment

source("BMP.R")

# Plots

source("plots.R")

# Output tables etc.

source("output.R")