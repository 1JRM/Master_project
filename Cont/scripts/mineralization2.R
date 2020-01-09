library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
source("../functions/dfsumm.R")

# Start with mass loss fitting to models

dat <- read_excel("../data/Continuous_exp_M_v2.xlsx", sheet = 1, skip = 3)

# Modify dataframe
dfsumm(dat)
dat$mass.bag <- as.numeric(dat$mass.bag)
dat$mass.in <- as.numeric(dat$mass.in)
dat$mass.sub <- as.numeric(dat$mass.sub)
dat$mass.sub.dry <- as.numeric(dat$mass.sub.dry)
dat$mass.dry <- as.numeric(dat$mass.dry)
dat$mass.sub.dry <- as.numeric(dat$mass.sub.dry)
dat$mass.loss <- as.numeric(dat$mass.loss)
dat$condition <- as.factor(dat$condition)
dat$mass.n.fin <- as.numeric(dat$mass.n.fin)
dat$mass.s.fin <- as.numeric(dat$mass.s.fin)
dat$perc.min.n <- as.numeric(dat$perc.min.n)
dat$perc.min.s <- as.numeric(dat$perc.min.s)
dfsumm(dat)

#Lower names

names(dat) <- tolower(names(dat))

# Extract whats important
dat2 <- dat[, c("id", "days", "mass.sub.dry", "perc.min.n", "perc.min.s", "condition")]

# Remove triplicates for mean calculation

dat2$id <- substr(dat2$id, 1, 7)

dat3 <- summarise(group_by(dat2, id), time = mean(days), condition = "condition", mean.mass = mean(mass.sub.dry, na.rm = TRUE))
dat3[c(1, 6, 16, 21, 26, 36), "mean.mass"] <- 25
dat3[c(11, 31), "mean.mass"] <- 50

# Change condition
dat3[1:20, "condition"] <- "Mesophilic"
dat3[21:40, "condition"] <- "Thermophilic"
dat3$substrate <- "Clover gras pulp"
dat3[6:10, "substrate"] <- "Cattle manure"
dat3[11:15, "substrate"] <- "Fertigro"
dat3[16:20, "substrate"] <- "Household waste"
dat3[26:30, "substrate"] <- "Cattle manure"
dat3[31:35, "substrate"] <- "Fertigro"
dat3[36:40, "substrate"] <- "Household waste"

dat3$mass.norm[1:10] <- (dat3$mean.mass[1:10]/25) * 100
dat3$mass.norm[11:15] <-(dat3$mean.mass[11:15]/50) * 100
dat3$mass.norm[16:30] <- (dat3$mean.mass[16:30]/25) * 100
dat3$mass.norm[31:35] <- (dat3$mean.mass[31:35]/50) * 100
dat3$mass.norm[36:40] <- (dat3$mean.mass[36:40]/25) * 100

# Plot it up to compare models

ggplot(data = dat3, aes(x = time, y = mass.norm, colour = substrate)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = "bottom") +
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  labs(x = "Time (d)", y = "Normalized mass (%)", colour = "Substrate") + 
  theme(legend.title = element_text( size = 8),
        legend.text = element_text( size = 7)) +
  facet_wrap(~condition)
ggsave('../plots/totalmassmineral_con.png')

### Next - % N loss

dat2

dat.min <- summarise(group_by(dat2, id), time = mean(days), condition = "condition", mean.perc.n = mean(perc.min.n, na.rm = TRUE), mean.perc.s = mean(perc.min.s, na.rm = TRUE))

dat.min[1:20, "condition"] <- "Mesophilic"
dat.min[21:40, "condition"] <- "Thermophilic"
dat.min$substrate <- "Clover grass pulp"
dat.min[6:10, "substrate"] <- "Cattle manure"
dat.min[11:15, "substrate"] <- "Fertigro"
dat.min[16:20, "substrate"] <- "Household waste"
dat.min[26:30, "substrate"] <- "Cattle manure"
dat.min[31:35, "substrate"] <- "Fertigro"
dat.min[36:40, "substrate"] <- "Household waste"

ggplot(data = dat.min, aes(x = time, y = mean.perc.n, colour = substrate)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = "bottom") +
  labs(x = "Time (d)", y = "Normalized total organic N (%)", colour = "Substrate") + 
  theme(legend.title = element_text( size = 8),
        legend.text = element_text( size = 7)) +
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100), limits = c(0, 100)) +
  facet_wrap(~condition)
ggsave('../plots/totalNmineral_con.png')

# And for Sulur

ggplot(data = dat.min, aes(x = time, y = mean.perc.s, colour = substrate)) + 
  geom_point() + 
  geom_line() +
  theme(legend.position = "bottom") +
  labs(x = "Time (d)", y = "Normalized total organic S (%)", colour = "Substrate") + 
  theme(legend.title = element_text( size = 8),
        legend.text = element_text( size = 7)) +
  scale_y_continuous(breaks=c(0, 20, 40, 60, 80, 100, 150, 175), limits = c(0, 180)) +
  facet_wrap(~condition)
ggsave('../plots/totalSmineral_con.png')