# Detection limit for water bottles
# 02.09.2019

# Updated: 05.09.2019

# Two sets of bottles (Water 01, Water 02)

# Order water samples according to time.d
water <- arrange(water, date)

# Calculate the standard deviation for water samples
detect.limits <- summarise(group_by(water, id), 
                           mean = mean(mass.init),
                           stddev = sd(mass.init), 
                           day.start = first(mass.init), 
                           day.end = last(mass.init), 
                           interval_nb = n_distinct(date),
                           detect.lim.int = stddev*3, 
                           detect.lim.tot = detect.lim.int*sqrt(interval_nb))

# Use mean
detect.lim.int <- round(mean(detect.limits$detect.lim.int), digits = 2)
detect.lim.tot <- round(mean(detect.limits$detect.lim.tot), digits = 2)