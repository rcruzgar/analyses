#!/usr/bin/env python

import matplotlib as mpl
mpl.use('agg')
import matplotlib.pyplot as plt
import numpy as np
import rpy2.robjects as ro
from rpy2.robjects import pandas2ri
pandas2ri.activate()
from mpl_toolkits.basemap import Basemap
import mpl_toolkits.basemap as bm
from matplotlib import ticker

# Plotting regular-grid variables in a 2D-map
#             The dimensions of the input array (from RData) have to be:
#             dims = c(exp, ftime, lat, lon)

class Mapstereo:
	
	def __init__(self, exp, obs, var, freq, start_month, yearb, yeare, 
				 calc, dayb, daye, colmap, ub, lb, nteps, extmethod, units, 
				 title, figdir, imageformat):
	    """
	    :param exp: experiment ID
        :type exp: str
        :param obs: observational reference
        :type obs: str
        
	    """ 
		self.exp = exp # "a0dc"
		self.obs = obs # "NSIDC"
		self.var = var, # "sic"
		self.freq = freq # "day" 
		self.start_month = start_month # "nov"
		self.yearb = yearb # "1988"
		self.yeare = yeare # "2012"
		self.calc = calc # "bias"
		self.dayb =  0
		self.daye = daye # 30
		self.colmap = colmap #"seismic"
		self.ub = ub
		self.lb = lb
		self.nsteps =  nsteps # 100
		self.extmethod = extmethod #"neither"
		self.units = units # "%"
		self.title = title # "Title"
		self.figdir = figdir # "outdir"
		self.imageformat = "png"
						
	
	def load_data(self):	
		inputs = (self.var, self.freq, self.exp, self.obs, 
		self.start_month, self.yearb, self.yeare, self.calc)
		input_file = '_'.join(strings)
		
		# Load Data from R
		R_object = ro.r['load'](input_file + ".RData")
								
		array = pandas2ri.ri2py(ro.r[R_object[0]])[0, self.dayb:self.daye, :, :] # e.g.: 30 days, 43 lat, 360 lon
		return array_loaded	
				
								
	def map_polar(self):
		m = Basemap(projection = "npstere", boundinglat = 47, lon_0 = 0, resolution = "l", round = True) # Just the North Pole (47 to 90ºN)
		fig = plt.figure(figsize=(8.5, 8))
		ax = fig.add_subplot(111)

		array_loaded = load_data()
		nx = array_loaded.shape[1]
		ny = array_loaded.shape[2]
		latitude = np.linspace(47, 90, nx)
		longitude = np.linspace(0, 360, ny)
		data, lon_c = bm.addcyclic(array_loaded, longitude) # removes the empty white line of longitude		
				
				
        if len(longitude.shape) == 1 and len(latitude.shape) == 1:
			lon_c, lat = np.meshgrid(lon_c, latitude)  # creates a grid
        xi, yi = m(lon_c, lat)	
				
		for day in np.arange(self.dayb, self.daye, 1):		
				
			data_day = data[day, :, :] # 43x360 matrix
				
			m.fillcontinents(color='silver')
			m.drawcoastlines()
				
			collevels = np.linspace(self.lb, self.ub, self.nsteps)	
				
			bnorm = mpl.colors.BoundaryNorm(collevels, ncolors=len(collevels), clip=False)
			cf = ax.contourf(xi, yi, data, vmin=collevels[0], vmax=collevels[-1],
							levels=collevels, cmap=self.colmap, norm=None, extend=self.extmethod)
							
			plt.title('%s Day %s' % (self.title, day), fontsize=24)

			cb = fig.colorbar(cf, ax=ax)
			tick_locator = ticker.MaxNLocator(nbins=12)
			cb.locator = tick_locator
			cb.update_ticks()
			if self.var == 'sic':
				legend_title = 'Sea Ice Concentration (%s)' % (self.unit)
			elif self.var == 'tos':
				legend_title = 'Sea Surface Temperature (%s)' % (self.unit)
			elif self.var == 'psl':
				legend_title = 'Sea Level Pressure (%s)' % (self.unit)

			cb.set_label(legend_title, fontsize=16, weight="bold")

			for c in cf.collections: # makes the plot smoother, without white lines between contours
				c.set_edgecolor("face") 
					
						
			plt.savefig('%s_%s_%s-%s_%s_%s-%s_%s_%s.%s' % (self.var,
						self.freq, self.exp, self.obs, self.start_month,
						 self.yearb, self.yeare, day, self.calc,
						  self.imageformat))

	#def animation(self):
				#### Hacer animación cuando acabe de plotear la otra función	
						
