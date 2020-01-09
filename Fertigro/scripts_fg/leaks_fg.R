# Leaks in bottles
# 02.09.2019
# Get mass loss info

# Updated: 13.10.2019

mass.loss <- biogas:::massLoss(batch, time.name = "e.time", m.pre.name = "mass.init", m.post.name = "mass.final", id.name = "id")

# Leaks for day 30
leaks.tot <- mass.loss[mass.loss$e.time > 2, ] 

# Is there a leak?
mass.loss$leaked <- mass.loss$mass.leak > detect.lim.int
leaks.tot$leaked <- leaks.tot$cmass.leak > detect.lim.tot

# Find bottles that leaked
id.leak <- unique(mass.loss[mass.loss$leaked, 'id']) 
id.leak.tot <- unique(leaks.tot[leaks.tot$leaked, 'id']) 