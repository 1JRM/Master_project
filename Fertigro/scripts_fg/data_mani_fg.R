# data manipulation
# 02.09.2019

# Updated latest: 13.10.2019

# Batch pressure excluding leaks
#batch$pres.grav <- ((batch$vol/setup$vol.hs) * 1013) + 1013
# mean(batch$pres.grav)

# Change tibble to dataframe

batch <- as.data.frame(batch)
setup <- as.data.frame(setup)

#Change names and classes for dataset.

names(batch) <- tolower(names(batch))
names(setup) <- tolower(names(setup))

dfsumm(batch)

# For batch
batch$vol <- as.numeric(batch$vol)
batch$date <- as.numeric(batch$date)
batch$time <- as.numeric(batch$time)
# For setup
setup$date <- as.numeric(setup$date)
setup$time <- as.numeric(setup$time)

# Make date.time column and change to date.time object

batch$date <- sprintf("%08d", batch$date)
batch$time <- sprintf("%04d", batch$time)
batch$date.time <- paste(batch$date, batch$time)
batch$date.time <- dmy_hm(batch$date.time)

setup$date <- sprintf("%08d", setup$date)
setup$time <- sprintf("%04d", setup$time)
setup$date.time <- paste(setup$date, setup$time)
setup$date.time <- dmy_hm(setup$date.time)

# Find minimum time for each ID and make elapsed time column

batch1 <- summarise(group_by(batch, id), start.date.time = min(setup$date.time))

batch <- merge(batch, batch1, by = "id")

dfsumm(batch)

batch$e.time <- as.numeric(difftime(batch$date.time, batch$start.date.time, units = 'days'))
batch$e.time <- round(batch$e.time, digits = 0)

# Make id a factor 

batch$id <- factor(batch$id)
setup$id <- factor(setup$id)

# Ignore water bottles + move them to another data frame

water <- batch[batch$id %in% c("Water 01", "Water 02"), ]
batch <- batch[batch$id != "Water 01" & batch$id != "Water 02", ]

# Check for observations and if water is gone

levels(batch$id)

unique(batch$e.time)
table(batch$e.time)

# Arrange data

batch <- arrange(batch, id, e.time)

# Reorder columns for looks

batch <- batch[, c("id","date", "time", "date.time", "start.date.time", "e.time", "mass.init", "vol", "mass.final", "temp", "pres","pres_adj", "incubator_temp_sp", "incubator_temp_ac", "notes")]

# After merging, batch is a tibble again?? Change batch to dataframe
batch <- as.data.frame(batch)
# Change adjusted pressure to numeric
batch$pres_adj <- as.numeric(batch$pres_adj)
