import osense


( convdata, satdata )= osense.read_osense('osense_2017091000.dat.out')

# creates a DataFrame with the mean of each satellite instrument
meanbyobtype = satdata.groupby('obtype').mean()

figuresize = (10,6)

o_dry=meanbyobtype['osense_dry'].sort_values(ascending=False)
dry_plot=o_dry.plot.barh(title = '2017091000 mean obs sensitivity, dry', figsize=figuresize);
fig=dry_plot.get_figure()
fig.savefig('obsense_dry.png',bbox_inches='tight')

fig.clear()

o_moist=meanbyobtype['osense_moist'].sort_values(ascending=False)
moist_plot=o_moist.plot.barh(title = '2017091000 mean obs sensitivity, moist', figsize=figuresize);
fig=moist_plot.get_figure()
fig.savefig('obsense_moist.png',bbox_inches='tight')

fig.clear()

o_kin=meanbyobtype['osense_kin'].sort_values(ascending=False)
kin_plot=o_kin.plot.barh(title = '2017091000 mean obs sensitivity, kinetic', figsize=figuresize);
fig=kin_plot.get_figure()
fig.savefig('obsense_kin.png',bbox_inches='tight')

fig.clear()



