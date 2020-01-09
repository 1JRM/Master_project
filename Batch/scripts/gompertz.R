# Gompertz and logistic models

# Solver was performed in excel. Can be found at Dropbox/Master.project/batch/data/solver

#CGP30

loggomp <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30), 
                      gas.obs = c(0, 93, 149, 198, 230, 230, 233, 227, 238))
loggomp$Model <- "Observed"

gompertz <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30),
                      gas.obs = 232.7*exp(-exp((((59.1*2.7188)/232.7)*(0.44-loggomp$time))+1)))
gompertz$Model <- "Gompertz"

gompplot <- rbind(loggomp, gompertz)

gompplot$id <- "CGP30"

#CGP12g

loggomp12g <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30), 
                      gas.obs = c(0, 108, 172, 222, 246, 250, 256, 250, 267))
loggomp12g$Model <- "Observed"

gompertz12g <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30),
                       gas.obs = 254.4*exp(-exp((((67.2*2.7188)/254.4)*(0.40-loggomp$time))+1)))
gompertz12g$Model <- "Gompertz"

gomp12g <- rbind(loggomp12g, gompertz12g)

gomp12g$id <- "CGP12g"

#CGP24g

loggomp24g <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30), 
                         gas.obs = c(0, 61, 103, 164, 201, 213, 230, 232, 243))
loggomp24g$Model <- "Observed"

gompertz24g <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30),
                          gas.obs = 233.5*exp(-exp((((38.4*2.7188)/233.5)*(0.42-loggomp$time))+1)))
gompertz24g$Model <- "Gompertz"

gomp24g <- rbind(loggomp24g, gompertz24g)

gomp24g$id <- "CGP24g"

# Combine

gomp_fin <- rbind(gompplot, gomp12g, gomp24g)

# FOM approach (XXXX)

fFOM <- yld.gc.grav[yld.gc.grav$substrate %in% "CGP30", ]

class(fFOM)
fFOM <- rename(fFOM,
  time.d = e.time,
  id = substrate,
  cvCH4 = mean)

source("../functions/mod_fit_stats.R")

ff <-  fitFOM(fFOM, n.pool = 1, fit.to = 'yield', method = 'LM', abs.err = FALSE,
              trans = TRUE, lag.phase = FALSE, init = c(B = 230, k = 0.3))

#CGP30

ff2 <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30), 
                  gas.obs = (239.2599133 * (1-exp(-0.3220491*(loggomp$time)))))

gomp2 <- gompertz <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30),
                    gas.obs = 232.67 *exp(-exp((((59.12*2.7188)/232.67)*(0.44-loggomp$time))+1)))

ff2$Model <- "FOM"
gomp2$Model <- "Gompertz"

gompplot2 <- rbind(gomp2, ff2, loggomp)

gompplot2$id <- "CGP30"

# CGP12g

CGPfom <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30), 
                      gas.obs = c(0, 108, 172, 222, 246, 250, 256, 250, 267))
CGPfom$Model <- "Observed"

cgpmod12g <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30), 
                  gas.obs = (262.0 * (1-exp(-0.3353*(loggomp$time)))))

cgpgomp2 <- gompertz <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30),
                                gas.obs = 254.437 *exp(-exp((((67.23*2.7188)/254.437)*(0.399-loggomp$time))+1)))

cgpmod12g$Model <- "FOM"
cgpgomp2$Model <- "Gompertz"

gompplot3 <- rbind(cgpgomp2, cgpmod12g, CGPfom)

gompplot3$id <- "CGP12g"


#CGP24g

CGPfom2 <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30), 
                     gas.obs = c(0, 61, 103, 164, 201, 213, 230, 232, 243))
CGPfom2$Model <- "Observed"

cgpmod24g <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30), 
                        gas.obs = (244.55 * (1-exp(-0.2052*(loggomp$time)))))

cgpgomp3 <- data.frame(time = c(0, 2, 3, 5, 7, 9, 14, 20, 30),
                                   gas.obs = 233.47 *exp(-exp((((38.366*2.7188)/233.47)*(0.42-loggomp$time))+1)))

cgpmod24g$Model <- "FOM"
cgpgomp3$Model <- "Gompertz"

gompplot4 <- rbind(cgpgomp3, cgpmod24g, CGPfom2)

gompplot4$id <- "CGP24g"

# Final

gomp2_fin <- rbind(gompplot3, gompplot4, gompplot2)

# Plot different models:

# Observed

ggplot(data = loggomp, aes(x = time, y = gas.obs)) + 
  geom_point() +
  geom_line() +
  labs(x = "Time (d)", y = expression(Yield~(mL~CH[4]~gVS^-1)))
ggsave('../plots/obs_model.png')

# With models

gomp_fin$id <- as.character(gomp_fin$id)

ggplot(data = gomp_fin, aes(x = time, y = gas.obs, colour = Model)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = c(0.9, 0.15)) +
  scale_color_manual(values=c('dodgerblue', 'black')) +
  labs(x = "Time (d)", y = expression(Yield~(mL~CH[4]~gVS^-1))) +
  facet_wrap(~id) +
scale_y_continuous(breaks=c(0, 100, 200, 250, 300), limits = c(0,300))
ggsave('../plots/gomp_log_obs2.png')

#

ggplot(data = gomp2_fin, aes(x = time, y = gas.obs, colour = Model)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = c(0.9, 0.15)) +
  scale_color_manual(values=c('red','dodgerblue', 'black')) +
  labs(x = "Time (d)", y = expression(Yield~(mL~CH[4]~gVS^-1))) +
  facet_wrap(~id) +
  scale_y_continuous(breaks=c(0, 100, 200, 250, 300), limits = c(0,300))
ggsave('../plots/gomp_log_obs3.png')
# And for modified FOM

gompplot3 <- gompplot2[gompplot2$Model %in% "Observed", c("time", "gas.obs", "Model")]

ggplot(data = gompplot3, aes(x = time, y = gas.obs)) + 
  geom_point() +
  geom_line() +
  labs(x = "Time (d)", y = expression(Yield~(mL~CH[4]~gVS^-1)))
ggsave('../plots/obs_model_d2.png')

gompplot2 <- gompplot2[gompplot2$Model %in% c("Observed", "FOM", "Gompertz"), ]

ggplot(data = gompplot2, aes(x = time, y = gas.obs, colour = Model)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = c(0.9, 0.15)) +
  scale_color_manual(values=c('red','dodgerblue', 'black')) +
  labs(x = "Time (d)", y = expression(Yield~(mL~CH[4]~gVS^-1)))
ggsave('../plots/FOM_vs_Gomp.png')






