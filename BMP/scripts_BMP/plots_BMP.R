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