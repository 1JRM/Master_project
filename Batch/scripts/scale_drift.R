# Scale drift correction
# 02.09.2019

# Updated: 05.09.2019

water$mass.drift <- mean(water$mass.init[water$date == '27082019'] - water$mass.init)
drift.dat <- unique(water[, c('date', 'mass.drift')])

batch <- merge(batch, drift.dat, by = 'date')

# Save original measurements as well
batch$mass.init.orig <- batch$mass.init
batch$mass.final.orig <- batch$mass.final
batch$mass.init <- batch$mass.init + batch$mass.drift
batch$mass.final <- batch$mass.final + batch$mass.drift

# Order is wrong again.. Change batch to id and e.time order:
batch <- arrange(batch, id, e.time)