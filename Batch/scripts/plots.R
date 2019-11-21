# Plots
# 02.09.2019
# Jacob R Mortensen
# Updated: 11.09.2019

# Add error bars to BMP
BMP.gc.gd.vol$lwr <- BMP.gc.gd.vol$mean - BMP.gc.gd.vol$sd
BMP.gc.gd.vol$upr <- BMP.gc.gd.vol$mean + BMP.gc.gd.vol$sd

BMP.gc.gd.grav$lwr <- BMP.gc.gd.grav$mean - BMP.gc.gd.grav$sd
BMP.gc.gd.grav$upr <- BMP.gc.gd.grav$mean + BMP.gc.gd.grav$sd

# Make BMP plot with histogram

ggplot(BMP.gc.gd.vol) + 
  geom_col(aes(substrate, mean, fill = method), position = 'dodge', colour  = 'black') +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_errorbar(aes(substrate, ymin = lwr, ymax = upr, group = method), position = 'dodge', colour = 'gray55') 
ggsave('../plots/BMP_vol.png')

# Make line diagram and compare gc and gd
ggplot(yld.gc.gd.vol, aes(e.time, mean, colour = substrate)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~ method)
ggsave('../plots/BMP_line_vol.png')


# For grav plots

ggplot(BMP.gc.gd.grav) + 
  geom_col(aes(substrate, mean, fill = method), position = 'dodge', colour  = 'black') +
  theme(axis.text.x = element_text(angle = 90)) +
  geom_errorbar(aes(substrate, ymin = lwr, ymax = upr, group = method), position = 'dodge', colour = 'gray55') 
ggsave('../plots/BMP_grav.png')

# Make line diagram and compare gc and gd
ggplot(yld.gc.gd.grav, aes(e.time, mean, colour = substrate)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~ method)
ggsave('../plots/BMP_line_grav.png')


### Compare nylon bag

BMP2.gc.gd.grav <- BMP.gc.gd.grav[BMP.gc.gd.grav$substrate %in% c("CGP30", "CGP12g", "CGP24g", "Litterbag and inoculum") & BMP.gc.gd.grav$method %in% "gc", ]
yld2.gc.gd.grav <- yld.gc.gd.grav[yld.gc.gd.grav$substrate %in% c("CGP30", "CGP12g", "CGP24g", "Litterbag and inoculum") & yld.gc.gd.grav$method %in% "gc", ]

BMP2.gc.gd.grav$substrate <- c("No_nylon_12g", "Nylon_24g", "Nylon_12g", "Nylon_only")

yld2.gc.gd.grav$substrate[1:9] <- "No_nylon_12g" 
yld2.gc.gd.grav$substrate[10:18] <- "Nylon_24g"
yld2.gc.gd.grav$substrate[19:27] <- "Nylon_12g"
yld2.gc.gd.grav$substrate[28:36] <- "Nylon_only"

ggplot(BMP2.gc.gd.grav) + 
  geom_col(aes(substrate, mean, fill = substrate), position = 'dodge', colour  = 'black') +
  theme(axis.text.x = element_text(angle = 25)) +
  geom_errorbar(aes(substrate, ymin = lwr, ymax = upr, group = method), position = 'dodge', colour = 'gray55') +
  theme(legend.position = "none") +
  labs(x = "Bottle", y = expression(Yield~(mL~CH[4]~gVS^-1)))
ggsave('../plots/BMP_grav_nylon.png')

# Make line diagram for gc compairing nylon bags
ggplot(yld2.gc.gd.grav, aes(e.time, mean, colour = substrate)) + 
  geom_line() +
  geom_point() +
  labs(x = "time [d]", y = expression(Yield~(mL~CH[4]~gVS^-1)), colour = "Bottle") +
  theme(legend.position = c(0.8, 0.35))
ggsave('../plots/BMP_line_grav_nylon.png')
