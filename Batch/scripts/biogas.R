# Get biogas data
# 02.09.2019

# Not working at the moment
# SDH: Is now. See email.
# From data.mani pres.grav is estimated to 1600 without leaks. 


# biogas.vol <- calcBgGD(batch, 
#              temp.vol = 'temp', pres.vol = 'pres_adj',
#              temp.grav = 52.5, pres.grav = 1600,
#              id.name = 'id', vol.name = 'vol',
#              m.pre.name = 'mass.init', m.post.name = 'mass.final',
#              comp.name = 'xCH4', time.name = 'e.time', 
#              vented.mass = TRUE, averaging = 'final', vmethod = 'grav',
#              extrap = FALSE, 
#              addt0 = TRUE, showt0 = TRUE)
options(unit.pres = 'hPa')
biogas.vol <- biogas:::calcBgGD(batch,
                       temp.vol = 'temp', pres.vol = 'pres_adj',
                       temp.grav = 52.5, pres.grav = 1600,
                       id.name = 'id', vol.name = 'vol',
                       m.pre.name = 'mass.init', m.post.name = 'mass.final',
                       comp.name = 'xCH4', time.name = 'e.time',
                       vented.mass = TRUE, averaging = 'final', vmethod = 'grav',
                       extrap = TRUE, addt0 = TRUE)

# biogas.grav <- biogas:::calcBgGD(batch,
#                         temp.vol = 'temp', pres.vol = 'pres_adj',
#                         temp.grav = 52.5, pres.grav = 1600,
#                         id.name = 'id', vol.name = 'vol',
#                         m.pre.name = 'mass.init', m.post.name = 'mass.final',
#                         comp.name = 'xCH4', time.name = 'e.time',
#                         vented.mass = TRUE, averaging = 'final', vmethod = 'grav',
#                         extrap = FALSE, addt0 = TRUE)


plot(xCH4 ~ id, data = biogas.vol, las = 3)
