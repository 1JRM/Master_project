# Plots
# 02.09.2019
# Jacob R Mortensen
# Updated: 11.09.2019

# Add error bars to BMP
BMP.gc.gd.vol$lwr <- BMP.gc.gd.vol$mean - BMP.gc.gd.vol$sd
BMP.gc.gd.vol$upr <- BMP.gc.gd.vol$mean + BMP.gc.gd.vol$sd

BMP.gc.gd.grav$lwr <- BMP.gc.gd.grav$mean - BMP.gc.gd.grav$sd
BMP.gc.gd.grav$upr <- BMP.gc.gd.grav$mean + BMP.gc.gd.grav$sd

BMP.gc.gd.grav2 <- BMP.gc.gd.grav[BMP.gc.gd.grav$method %in% "gc", ]
BMP.gc.gd.grav2 <- BMP.gc.gd.grav2[BMP.gc.gd.grav2$substrate %in% c("Cattle manure", "Clover grass pulp", "Household waste", "Fertigro"), ]

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

ggplot(BMP.gc.gd.grav2) + 
  geom_col(aes(substrate, mean, fill = substrate), position = 'dodge', color = "black") +
  theme(axis.text.x = element_text(angle = 25)) +
  geom_errorbar(aes(substrate, ymin = lwr, ymax = upr, group = method), position = 'dodge', colour = 'gray55') +
  ylab(expression(Yield~(mL~CH[4]~gVS^-1))) +
  labs(fill = "Substrate") +
  theme(legend.position = "bottom") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  theme(legend.title = element_text( size = 10),
        legend.text = element_text( size = 8)) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE))
ggsave('../plots_BMP/BMP_grav.png')

# Make line diagram and compare gc and gd
ggplot(yld.gc.grav, aes(e.time, mean, colour = substrate)) + 
  geom_line() + 
  labs(x = 'Time (d)', y = expression(Yield~(mL~CH[4]~gVS^-1)), colour = "Substrate") +
  geom_point() +
  theme(legend.position = c(0.8, 0.15)) + 
  theme(legend.title = element_text( size = 8),
    legend.text = element_text( size = 7)) +
  guides(colour=guide_legend(nrow = 3))
ggsave('../plots_BMP/BMP_grav_gc_line.png')

ggplot(yld.gd.grav, aes(e.time, mean, colour = substrate)) + 
  geom_line() + 
  labs(x = 'Time (d)', y = expression(Yield~(mL~CH[4]~gVS^-1)), colour = "Substrate") +
  geom_point() +
  theme(legend.position = c(0.8, 0.15)) + 
  theme(legend.title = element_text( size = 8),
        legend.text = element_text( size = 7)) +
  guides(colour=guide_legend(nrow = 3))
ggsave('../plots_BMP/BMP_grav_gd_line.png')
