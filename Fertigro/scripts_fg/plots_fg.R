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
  theme(axis.text.x = element_text(angle = 25)) +
  geom_errorbar(aes(substrate, ymin = lwr, ymax = upr, group = method), position = 'dodge', colour = 'gray55') 
ggsave('../plots_BMP/BMP_vol_sub_hist.png')

# Make line diagram and compare gc and gd
ggplot(yld.gc.gd.vol, aes(e.time, mean, colour = substrate)) + 
  geom_line() +
  geom_point() +
  facet_wrap(~ method)
ggsave('../plots_BMP/BMP_vol_sub_line.png')

# For grav plots

ggplot(BMP.gc.gd.grav) + 
  geom_col(aes(substrate, mean, fill = method), position = 'dodge', colour  = 'black') +
  theme(axis.text.x = element_text(angle = 25)) +
  geom_errorbar(aes(substrate, ymin = lwr, ymax = upr, group = method), position = 'dodge', colour = 'gray55') +
  labs(x = "Substrate", y = expression(Yield~(mL~CH[4]~gVS^-1)), colour = "Method")
ggsave('../plots_BMP/BMP_grav.png')

# Make line diagram and compare gc and gd
ggplot(yld.gc.grav, aes(e.time, mean, colour = substrate)) + 
  geom_line() + 
  labs(x = 'Time (d)', y = expression(Yield~(mL~CH[4]~gVS^-1)), colour = "Substrate") +
  geom_point() +
  theme(legend.position = c(0.8, 0.15))
ggsave('../plots_BMP/BMP_grav_gc_line.png')

ggplot(yld.gd.grav, aes(e.time, mean, colour = substrate)) + 
  geom_line() + 
  labs(x = 'Time (d)', y = expression(Yield~(mL~CH[4]~gVS^-1)), colour = "Substrate") +
  geom_point() +
  theme(legend.position = c(0.8, 0.15))
ggsave('../plots_BMP/BMP_grav_gd_line.png')
