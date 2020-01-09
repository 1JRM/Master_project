library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
source("../functions/dfsumm.R")

# Start with mass loss fitting to models

dat <- read_excel("../data/Setup_biogas_JMOR_master_batch.xlsx", sheet = 3, skip = 3)

# Modify dataframe
dfsumm(dat)
dat$mass.bag <- as.numeric(dat$mass.bag)
dat$mass.in <- as.numeric(dat$mass.in)
dat$mass.sub.wet <- as.numeric(dat$mass.sub.wet)
dat$mass.sub.dry <- as.numeric(dat$mass.sub.dry)
dat$mass.fin.dry <- as.numeric(dat$mass.fin.dry)
dat$mass.fin.sub.dry <- as.numeric(dat$mass.fin.sub.dry)
dat$mass.remove <- as.numeric(dat$mass.remove)

# Extract whats important
dat2 <- dat[4:15, ]

# Remove triplicates for mean calculation
dat2$id <- substr(dat2$id, 1, 3)

dat3 <- summarise(group_by(dat2, id), time = mean(time), mean.mass = mean(mass.fin.sub.dry))
dat3[nrow(dat3) + 1,] = c("D0","0","2.826") # Add T0 from excel data
class(dat3)
dat3$time <- as.numeric(dat3$time) # Change to numeric

dat3 <- dat3[order(dat3$time), ] # Restructure before adding models

# Models obtained from excel solver
mod1 <- 2.826*(exp(-0.123*dat3$time))
mod2 <- 0.8376 + (2.826-0.8376)*exp(-0.444*dat3$time)
dat3$method <- c("Observed") # Add method for plot later

# Modify models for combining with other data

model1 <- data.frame(dat3$time, mod1)
model1$method <- "Classic FOM"
model1 <- rename(model1, time = dat3.time, mean.mass = mod1)
model1[order(model1$time), ]
model1$id <- c("D0", "D03", "D07", "D14", "D30")
model2 <- data.frame(dat3$time, mod2)
model2$method <- "Modified FOM"
model2 <- rename(model2, time = dat3.time, mean.mass = mod2)
model2$id <- c("D0", "D03", "D07", "D14", "D30")

# Combine data and models

dat4 <- rbind(dat3, model1, model2)
dat4$mean.mass <- as.numeric(dat4$mean.mass)
dat4$mean.mass <- round(dat4$mean.mass, digits = 3)

# Plot it up to compare models

ggplot(data = dat4, aes(x = time, y = mean.mass, colour = method)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = c(0.9, 0.9)) +
  scale_color_manual(values=c('red','dodgerblue', 'black')) +
  labs(x = "Time (d)", y = "Total mass (g)", colour = "Model")
ggsave('../plots/totalmassmineral.png')



### Next - Total N loss

dat.n <- summarise(group_by(dat2, id), time = mean(time), mean.mass.n = mean(mass.fin.n))

# Add T0

dat.n[nrow(dat.n) + 1,] = c("D0","0","0.0779") # Add T0 from excel data
class(dat.n)
dat.n$time <- as.numeric(dat.n$time) # Change to numeric
dat.n$mean.mass.n <- as.numeric(dat.n$mean.mass.n)
dat.n$mean.mass.n <- dat.n$mean.mass.n*1000

dat.n <- dat.n[order(dat.n$time), ] # Restructure before adding models

# Models obtained from excel solver
mod1.n <- 77.90*(exp(-0.479*dat.n$time))
mod2.n <- 9.83 + (77.9-9.83)*exp(-0.920*dat.n$time)
dat.n$method <- c("Observed") # Add method for plot later

model1.n <- data.frame(dat.n$time, mod1.n)
model1.n$method <- "Classic FOM"
model1.n <- rename(model1.n, time = dat.n.time, mean.mass.n = mod1.n)
model1.n[order(model1.n$time), ]
model1.n$id <- c("D0", "D03", "D07", "D14", "D30")
model2.n <- data.frame(dat.n$time, mod2.n)
model2.n$method <- "Modified FOM"
model2.n <- rename(model2.n, time = dat.n.time, mean.mass.n = mod2.n)
model2.n$id <- c("D0", "D03", "D07", "D14", "D30")

# Combine data and models

dat4.n <- rbind(dat.n, model1.n, model2.n)

ggplot(data = dat4.n, aes(x = time, y = mean.mass.n, colour = method)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = c(0.9, 0.9)) +
  scale_color_manual(values=c('red','dodgerblue', 'black')) +
  scale_y_continuous(breaks = c(0, 10, 20, 40, 60, 80), limits = c(0,80)) +
  labs(x = "Time (d)", y = "Total organic N (mg)", colour = "Model")
ggsave('../plots/totalnmineral.png')

### And for sulphur

dat.s <- summarise(group_by(dat2, id), time = mean(time), mean.mass.s = mean(mass.fin.s))

# Add T0

dat.s[nrow(dat.s) + 1,] = c("D0","0","0.0031") # Add T0 from excel data
class(dat.s)
dat.s$time <- as.numeric(dat.s$time) # Change to numeric

dat.s <- dat.s[order(dat.s$time), ] # Restructure before adding models

dat.s$mean.mass.s <- as.numeric(dat.s$mean.mass.s)
dat.s$mean.mass.s <- dat.s$mean.mass.s*1000

# Models obtained from excel solver
mod1.s <- 3.099*(exp(-0.034*dat.s$time))
mod2.s <- 1.6752 + (3.09918-1.6752)*exp(-6.372*dat.s$time)
dat.s$method <- c("Observed") # Add method for plot later

model1.s <- data.frame(dat.s$time, mod1.s)
model1.s$method <- "Classic FOM"
model1.s <- rename(model1.s, time = dat.s.time, mean.mass.s = mod1.s)
model1.s[order(model1.s$time), ]
model1.s$id <- c("D0", "D03", "D07", "D14", "D30")
model2.s <- data.frame(dat.s$time, mod2.s)
model2.s$method <- "Modified FOM"
model2.s <- rename(model2.s, time = dat.s.time, mean.mass.s = mod2.s)
model2.s$id <- c("D0", "D03", "D07", "D14", "D30")

# Combine data and models

dat4.s <- rbind(dat.s, model1.s, model2.s)

ggplot(data = dat4.s, aes(x = time, y = mean.mass.s, colour = method)) + 
  geom_point() +
  theme(legend.position = c(0.9, 0.9)) +
  geom_line() +
  scale_color_manual(values=c('red','dodgerblue', 'black')) +
  labs(x = "Time (d)", y = "Total organic S (mg)", colour = "Model")
ggsave('../plots/totalsmineral.png')


### For % degradation

# Total dataframe for % mass, % N and %S removal - T0 standardized to 100.

percTNS <- data.frame(time = c(0, 3, 7, 14, 30), tot.mass = c(100, 45.64, 38.39, 33.31, 30.08), tot.n = c(100, 17.72, 19.80, 18.36, 12.66), tot.s = c(100, 52.21, 55.85, 65.67, 54.13))

# Total mass
ggplot(data = percTNS, aes(x = time, y = tot.mass)) + 
  geom_point() +
  theme(legend.position = c(0.9, 0.9)) +
  geom_line() +
  scale_y_continuous(breaks = c(0, 20, 30, 40, 50, 75 ,100), limits = c(0,100)) +
  labs(x = "Time (d)", y = "Normalized mass (%)")
ggsave('../plots/perc_totalmineral_batch.png')

# Total N

ggplot(data = percTNS, aes(x = time, y = tot.n)) + 
  geom_point() +
  theme(legend.position = c(0.9, 0.9)) +
  geom_line() +
  scale_y_continuous(breaks = c(0, 10, 20, 50, 75 ,100), limits = c(0,100)) +
  labs(x = "Time (d)", y = "Normalized total organic N (%)")
ggsave('../plots/perc_Nmineral_batch.png')

# Total S

ggplot(data = percTNS, aes(x = time, y = tot.s)) + 
  geom_point() +
  theme(legend.position = c(0.9, 0.9)) +
  geom_line() +
  labs(x = "Time (d)", y = "Normalized total organic S (%)")
ggsave('../plots/perc_Smineral_batch.png')


