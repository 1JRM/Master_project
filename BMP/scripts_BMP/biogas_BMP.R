# Get biogas data
# 02.09.2019

# Not working at the moment
# SDH: Is now. See email.
# From data.mani pres.grav is estimated to 1600 without leaks. Add 200 to correct for the leaks.
# 1800 is therefore the best estimate.

options(unit.pres = 'hPa')
biogas.vol <- biogas:::cumBgGD(batch, 
             temp.vol = 'temp', pres.vol = 'pres_adj',
             temp.grav = 52.5, pres.grav = 1600,
             id.name = 'id', vol.name = 'vol',
             m.pre.name = 'mass.init', m.post.name = 'mass.final',
             comp.name = 'xCH4', time.name = 'e.time', 
             vented.mass = TRUE, averaging = 'final', vmethod = 'vol',
             extrap = TRUE, 
             addt0 = TRUE, showt0 = TRUE)


plot(xCH4 ~ id, data = biogas.vol, las = 3)

#Biogas grav:

#biogas.grav <- biogas:::cumBgGD(batch, 
                               #temp.vol = 'temp', pres.vol = 'pres_adj',
                               #temp.grav = 52.5, pres.grav = 1600,
                               #id.name = 'id', vol.name = 'vol',
                               #m.pre.name = 'mass.init', m.post.name = 'mass.final',
                               #comp.name = 'xCH4', time.name = 'e.time', 
                               #vented.mass = TRUE, averaging = 'final', vmethod = 'grav',
                               #extrap = TRUE, 
                               #addt0 = TRUE, showt0 = TRUE)
#plot(xCH4 ~ id, data = biogas.grav, las = 3)

# Tryout gdComp() instead - doesn't work so far (get minus values).
#biogas$pres <- biogas$pres/1013
#biogas$xCH4 <- biogas:::GDComp(mass = biogas$mass.vent, vol = biogas$vBg,
                        #temp = biogas$temp, pres = biogas$pres,
                        #unit.pres = 'atm', unit.temp = 'C', vol.hs = setup$vol.hs,
                        #temp.init = 20, pres.init = 1)
