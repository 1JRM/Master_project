# GC data import
# 05.09.2019

# Updated: 05.09.2019

# Load GC data
gc <- read_excel("../data_fg/fg_dat.xlsx", sheet = 7, skip = 1)

# Lower case names
names(gc) <- tolower(names(gc))

# Add leading zeros
gc$date <- sprintf("%08d", gc$date)

# Extract important columns
gc <- gc[, c("id", "date", "xch4_correc")]

# Rename xCH4 column for correct merging
gc <- rename(gc, xch4_gc = xch4_correc)

# Change class to data frame
gc <- as.data.frame(gc)

# Change class of date to factor for merging

biogas.vol$date <- factor(biogas.vol$date)
gc$date <- factor(gc$date)

# Change class of ID to factor for gc data
gc$id <- factor(gc$id)
class(gc$id)
dfsumm(gc)

# Merge with biogas data

biogas.vol <- merge(gc, biogas.vol, by = c("id", "date"), all = TRUE)

# Arrange data
biogas.vol <- arrange(biogas.vol, id, e.time)

# Restructure columns
unique(names(biogas.vol))
biogas.vol <- biogas.vol[, c("id", "date", "time", "date.time", "start.date.time",
                      "e.time", "mass.init", "mass.final", "temp", "pres",
                      "incubator_temp_sp", "incubator_temp_ac", "notes",
                      "vol", "mass.drift", "mass.init.orig",
                      "mass.final.orig", "vBg", "cvBg", "temp.grav", "pres.grav", "mass.tot", "mass.vent",
                      "mass.leak", "cmass.tot", "cmass.leak", "cmass.vent",
                      "xCH4.lim.flag", "vCH4", "cvCH4", "rvBg", "rvCH4", "xCH4",
                      "xch4_gc")]

# Rename xCH4 columns for identification

biogas.vol <- rename(biogas.vol, 
                 xCH4_gd = xCH4,
                 xCH4_gc = xch4_gc)

# Round xCH4_gd and xCH4_gc
biogas.vol$xCH4_gd <- round(biogas.vol$xCH4_gd, digits = 2)
biogas.vol$xCH4_gc <- round(biogas.vol$xCH4_gc, digits = 2)

# Comparing for time > 20 days
unique(biogas.vol$id)
xCH4_d9 <- biogas.vol[biogas.vol$e.time > 35
                  & biogas.vol$id %in% c("FG5 01", "FG10 01", "Ino 01"), c("id", "xCH4_gc", "xCH4_gd")] 

xCH4_d9$perc_diff <- ((xCH4_d9$xCH4_gc - xCH4_d9$xCH4_gd ) / ((xCH4_d9$xCH4_gd + xCH4_d9$xCH4_gc)/2)) * 100
xCH4_d9$perc_diff <- round(xCH4_d9$perc_diff, digits = 2)
