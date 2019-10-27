# BMP from data
# Jacob R. Mortensen
# 11.09.2019

# Redo naming and classes (m.sub.vs was a character!)
names(setup)

setup <- rename(setup, m.ino = mass_ino)

names(setup)
class(biogas.vol$cvCH4)
setup$m.sub.vs <- as.numeric(setup$m.sub.vs)
class(setup$m.sub.vs)
# Separate gd CH4 and gc CH4
names(biogas.vol)
biogas.vol <- rename(biogas.vol, vCH4.gd = vCH4)
biogas.vol <- rename(biogas.vol, cvCH4.gd = cvCH4)
names(biogas.vol)

# Add cvCH4.gc - assume gas composition is identical for bottles.

bio1.vol <- biogas.vol[biogas.vol$id %in% c('Cell 01', 'D3 01', 'D7 01', 'D14 01', 'D30 01', 'Ino 01', 'D30 12 01', 'D30 24 01', 'Nylon'), ]
bio2.vol <- biogas.vol[biogas.vol$id %in% c('Cell 02', 'D3 02', 'D7 02', 'D14 02', 'D30 02', 'Ino 02', 'D30 12 02', 'D30 24 02'), ]
bio3.vol <- biogas.vol[biogas.vol$id %in% c('Cell 03', 'D3 03', 'D7 03', 'D14 03', 'D30 03', 'Ino 03', 'D30 12 03', 'D30 24 03'), ]


bio1.vol$id <- sort(bio1.vol$id, decreasing = FALSE)
bio2.vol$id <- sort(bio2.vol$id, decreasing = FALSE)
bio3.vol$id <- sort(bio3.vol$id, decreasing = FALSE)
  
# Add gc measurements from 01 to 02 and 03. These are NA right now.

  bio2.vol[bio2.vol$id %in% c('Cell 02', 'D3 02', 'D7 02',
                    'D14 02', 'D30 02', 'Ino 02',
                    'D30 12 02', 'D30 24 02'), 'xCH4_gc'] <- bio1.vol[bio1.vol$id %in% c('Cell 01',
                                                            'D3 01', 'D7 01', 'D14 01',
                                                            'D30 01', 'Ino 01', 'D30 12 01',
                                                            'D30 24 01'), 'xCH4_gc']

  bio3.vol[bio3.vol$id %in% c('Cell 03', 'D3 03', 'D7 03',
                  'D14 03', 'D30 03', 'Ino 03',
                  'D30 12 03', 'D30 24 03'), 'xCH4_gc'] <- bio1.vol[bio1.vol$id %in% c('Cell 01', 
                                                             'D3 01', 'D7 01', 'D14 01',
                                                             'D30 01', 'Ino 01', 'D30 12 01',
                                                             'D30 24 01'), 'xCH4_gc']

# Stack data
biogas.vol <- rbind(bio1.vol, bio2.vol, bio3.vol)


biogas.vol$vCH4.gc <- biogas.vol$vBg * biogas.vol$xCH4_gc

# Set T0 volumes to 0.

biogas.vol[is.na(biogas.vol$vCH4.gc), 'vCH4.gc'] <- 0

# Make cumulative vCH4
biogas.vol <- biogas.vol %>%
  group_by(id) %>%
  mutate(cvCH4.gc = cumsum(vCH4.gc))

# Change back to data frame

biogas.vol <- as.data.frame(biogas.vol)

# Calculate BMP and yield for GD and add method to data frames

BMP.gd.vol <- summBg(biogas.vol, setup, id.name = "id", vol.name = 'cvCH4.gd',
              time.name = 'e.time', descrip.name = 'substrate',
              inoc.name = "Inoculum", inoc.m.name = "m.ino", norm.name = "m.sub.vs",
              when = 'end', when.min = 12, extrap = TRUE, set.name = 'method')

BMP.gd.vol$rsd <- 100 * BMP.gd.vol$sd/BMP.gd.vol$mean

BMP.gd.vol$method <- 'gd'

# Calculate yield for each datapoint

yld.gd.vol <- summBg(biogas.vol, setup, id.name = "id", vol.name = 'cvCH4.gd',
              time.name = 'e.time', descrip.name = 'substrate',
              inoc.name = "Inoculum", inoc.m.name = "m.ino", norm.name = "m.sub.vs",
              when = 'meas', extrap = TRUE, set.name = 'method')

# Add method
yld.gd.vol$method <- 'gd'

# Calculate BMP and yield for gc measurements

BMP.gc.vol <- summBg(biogas.vol, setup, id.name = "id", vol.name = 'cvCH4.gc',
                 time.name = 'e.time', descrip.name = 'substrate',
                 inoc.name = "Inoculum", inoc.m.name = "m.ino", norm.name = "m.sub.vs",
                 when = 'end', when.min = 12, extrap = TRUE, set.name = 'method')

BMP.gc.vol$rsd <- 100 * BMP.gc.vol$sd/BMP.gc.vol$mean

# Add method to data
BMP.gc.vol$method <- 'gc'

# Calculate yield for gc data

yld.gc.vol <- summBg(biogas.vol, setup, id.name = "id", vol.name = 'cvCH4.gc',
                 time.name = 'e.time', descrip.name = 'substrate',
                 inoc.name = "Inoculum", inoc.m.name = "m.ino", norm.name = "m.sub.vs",
                 when = 'meas', extrap = TRUE, set.name = 'method')

# Add method to data

yld.gc.vol$method <- 'gc'

# Merge data to be able to plot

BMP.gc.gd.vol <- rbind(BMP.gc.vol, BMP.gd.vol)
yld.gc.gd.vol <- rbind(yld.gc.vol, yld.gd.vol)

# Remove litterbag only experiment

BMP.gc.gd.vol <- BMP.gc.gd.vol[!BMP.gc.gd.vol$substrate %in% 'Litterbag and inoculum', ]
yld.gc.gd.vol <- yld.gc.gd.vol[!yld.gc.gd.vol$substrate %in% 'Litterbag and inoculum', ]

# Make grav calculations
options(unit.pres = 'hPa')
biogas.gd.grav <- biogas::cumBg(biogas.vol, dat.type = "mass", temp = 52.5, pres = 1600,
                             data.struct = "longcombo",
                             id.name = "id", time.name = "e.time",
                             dat.name = "mass.final", comp.name = "xCH4_gd",
                             headspace = setup, vol.hs.name = "vol.hs", headcomp = "N2",
                             temp.init = 26,
                             extrap = TRUE,
                             addt0 = FALSE)
# add method to dataset
biogas.gd.grav$method <- 'gd'
options(unit.pres = 'hPa')
biogas.gc.grav <- biogas::cumBg(biogas.vol, dat.type = "mass", temp = 52.5, pres = 1600,
                                        data.struct = "longcombo",
                                        id.name = "id", time.name = "e.time",
                                        dat.name = "mass.final", comp.name = "xCH4_gc",
                                        headspace = setup, vol.hs.name = "vol.hs", headcomp = "N2",
                                        temp.init = 26,
                                        extrap = TRUE,
                                        addt0 = FALSE)

biogas.gc.grav$method <- 'gc'

# Calculate BMP and yield for GD

BMP.gd.grav <- summBg(biogas.gd.grav, setup, id.name = "id", vol.name = 'cvCH4',
                     time.name = 'e.time', descrip.name = 'substrate',
                     inoc.name = "Inoculum", inoc.m.name = "m.ino", norm.name = "m.sub.vs",
                     when = 'end', when.min = 12, extrap = TRUE, set.name = 'method')

BMP.gd.grav$rsd <- 100 * BMP.gd.grav$sd/BMP.gd.grav$mean

# Add method to columns

BMP.gd.grav$method <- 'gd'

# Calculate yield for each datapoint

yld.gd.grav <- summBg(biogas.gd.grav, setup, id.name = "id", vol.name = 'cvCH4',
                     time.name = 'e.time', descrip.name = 'substrate',
                     inoc.name = "Inoculum", inoc.m.name = "m.ino", norm.name = "m.sub.vs",
                     when = 'meas', extrap = TRUE, set.name = 'method')

# Add method
yld.gd.grav$method <- 'gd'

# OBS STOPPED HERE 11.09.2019

# Calculate BMP and yield for gc measurements
BMP.gc.grav <- summBg(biogas.gc.grav, setup, id.name = "id", vol.name = 'cvCH4',
                     time.name = 'e.time', descrip.name = 'substrate',
                     inoc.name = "Inoculum", inoc.m.name = "m.ino", norm.name = "m.sub.vs",
                     when = 'end', when.min = 12, extrap = TRUE, set.name = 'method')

BMP.gc.grav$rsd <- 100 * BMP.gc.grav$sd/BMP.gc.grav$mean

# Add method to data
BMP.gc.grav$method <- 'gc'

# Calculate yield for gc data

yld.gc.grav <- summBg(biogas.gc.grav, setup, id.name = "id", vol.name = 'cvCH4',
                     time.name = 'e.time', descrip.name = 'substrate',
                     inoc.name = "Inoculum", inoc.m.name = "m.ino", norm.name = "m.sub.vs",
                     when = 'meas', extrap = TRUE, set.name = 'method')

# Add method to data

yld.gc.grav$method <- 'gc'

# Merge data to be able to plot

BMP.gc.gd.grav <- rbind(BMP.gc.grav, BMP.gd.grav)
yld.gc.gd.grav <- rbind(yld.gc.grav, yld.gd.grav)

BMP.gc.gd.grav <- BMP.gc.gd.grav[!BMP.gc.gd.grav$substrate %in% 'Litterbag and inoculum', ]
yld.gc.gd.grav <- yld.gc.gd.grav[!yld.gc.gd.grav$substrate %in% 'Litterbag and inoculum', ]


