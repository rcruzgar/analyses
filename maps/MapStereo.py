#!/usr/bin/env python

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
import rpy2.robjects as ro
import mpl_toolkits.basemap as bm
from matplotlib import ticker
from mpl_toolkits.basemap import Basemap
from rpy2.robjects import pandas2ri
pandas2ri.activate()
mpl.use('agg')

'''
  Plotting regular-grid variables in a North Stereographic map.
  Data are obtained from RData arrays loaded with 'load_data.R' script.
  The dimensions of the input array (from RData) are:
  	Experiment, Time, Lat, Lon
  Example of usage: 
	python MapStereo.py --exp a1q0 --obs NSIDC --var sic --freq day --start_month nov --yearb 1988 --yeare 2012 --calc bias --dayb 1 --daye 3 --colmap seismic --ub 100 --lb -100 --colsteps 1 --extmethod neither --units % --title Bias --figdir /username/data --imageformat png
'''

class MapStereo:
	
	def __init__(self, exp, obs, var, freq, start_month, yearb, yeare, 
				 calc, dayb, daye, colmap, ub, lb, colsteps, extmethod, units, 
				 title, figdir, imageformat):
	    
		self.exp = exp 
		self.obs = obs 
		self.var = var,
		self.freq = freq 
		self.start_month = start_month 
		self.yearb = yearb 
		self.yeare = yeare 
		self.calc = calc 
		self.dayb = dayb 
		self.daye = daye 
		self.colmap = colmap 
		self.ub = ub
		self.lb = lb
		self.colsteps =  colsteps 
		self.extmethod = extmethod
		self.units = units 
		self.title = title 
		self.figdir = figdir 
		self.imageformat = imageformat
						
	# Load the RData to obtain the array:
	
	def load_data(self):	
		inputs = (self.var, self.freq, self.exp, self.obs, 
		self.start_month, self.yearb, self.yeare, self.calc)
		input_file = '_'.join(''.join(elems) for elems in inputs)
	
		R_object = ro.r['load'](figdir + '/'  + input_file + ".RData")
		
		day1 = int(self.dayb) - 1 
		day2 = int(self.daye)

		array_loaded = pandas2ri.ri2py(ro.r[R_object[0]])[0, day1:day2, :, :] 
		return array_loaded	
		
	# The plotting function:		
								
	def map_polar(self):
		array_loaded = self.load_data()
                nx = array_loaded.shape[1]
                ny = array_loaded.shape[2]
                latitude = np.linspace(47, 90, nx)
                longitude = np.linspace(0, 360, ny)
                # Remove the empty white line of longitude
                data, lon_c = bm.addcyclic(array_loaded, longitude)

		# Just the North Pole
		m = Basemap(projection = "npstere", boundinglat = 47, lon_0 = 0,
                           resolution = "l", round = True)

		# Create a grid
                if len(longitude.shape) == 1 and len(latitude.shape) == 1:
                	lon_c, lat = np.meshgrid(lon_c, latitude)
                xi, yi = m(lon_c, lat)

		dayini = int(self.dayb) - 1
                dayend = int(self.daye)

		# Plot the selected days
                for day in np.arange(dayini, dayend, 1):
			fig = plt.figure(figsize=(8.5, 8))
			ax = fig.add_subplot(111)
			m.fillcontinents(color='silver')
                        m.drawcoastlines()			
	
			data_day = data[day, :, :]
			
                        lb_nr = int(self.lb)
                        ub_nr = int(self.ub)
			step_nr = int(self.colsteps)

                        collevels = np.arange(lb_nr, ub_nr+step_nr, step_nr)

			bnorm = mpl.colors.BoundaryNorm(collevels, ncolors=len(collevels), clip=False)
			cf = ax.contourf(xi, yi, data_day, vmin=collevels[0], vmax=collevels[-1],
						levels=collevels, cmap=self.colmap, norm=None, extend=self.extmethod)

                        cb = fig.colorbar(cf, ax=ax)
                        tick_locator = ticker.MaxNLocator(nbins=12)
                        cb.locator = tick_locator
                        cb.update_ticks()

                        if self.var[0] == 'sic':
                                legend_title = 'Sea Ice Concentration (%s)' % (self.units)
                        elif self.var[0] == 'tos':
                                legend_title = 'Sea Surface Temperature (%s)' % (self.units)
                        elif self.var[0] == 'psl':
                                legend_title = 'Sea Level Pressure (%s)' % (self.units)

                        cb.set_label(legend_title, fontsize=16, weight="bold")
			
			day_plus = day + 1						
			plt.title('%s Day %s' % (self.title, day_plus), fontsize=24)
			
			# Make the plot smoother, without white lines between contours
			for c in cf.collections:
				c.set_edgecolor("face") 
					
			plt.savefig('%s_%s_%s-%s_%s_%s-%s_%s_%s.%s' % (self.var[0],
					self.freq, self.exp, self.obs, self.start_month,
					 self.yearb, self.yeare, day_plus, self.calc,
					  self.imageformat))


def get_command_line_arguments():
    """
    Returns a list of files that have been provided as a command line argument
    :return: list of files
    """
    # Parse and assert command line options
    import argparse
    parser = argparse.ArgumentParser()
    parser.add_argument("--exp", type=str, default=None,
                        help="The experiment ID")
    parser.add_argument("--obs", type=str, default=None,
                        help="The observational reference")          
    parser.add_argument("--var", type=str, default=None,
                        help="The variable name")   
    parser.add_argument("--freq", type=str, default=None,
			help="Frequency")   
    parser.add_argument("--start_month", type=str, default=None,
                        help="Initialization month")   
    parser.add_argument("--yearb", type=str, default=None,
			help="First forecast year")   
    parser.add_argument("--yeare", type=str, default=None,
                        help="Last forecast year")        
    parser.add_argument("--calc", type=str, default=None,
                        help="Diagnostic")   
    parser.add_argument("--dayb", type=str, default=None,
	                help="First day")   
    parser.add_argument("--daye", type=str, default=None,
                        help="Last day")   
    parser.add_argument("--colmap", type=str, default=None,
		        help="Color palette")  
    parser.add_argument("--ub", type=str, default=None,
                        help="Colorbar Upper bound")        
    parser.add_argument("--lb", type=str, default=None,
                        help="Colorbar Lower bound")   
    parser.add_argument("--colsteps", type=str, default=None,
		        help="Colorbar Steps")   
    parser.add_argument("--extmethod", type=str, default=None,
                        help="Colorbar extremes")   
    parser.add_argument("--units", type=str, default=None,
		        help="Variable units")  						
    parser.add_argument("--title", type=str, default=None,
                        help="Title")        
    parser.add_argument("--figdir", type=str, default=None,
                        help="Output directory")   
    parser.add_argument("--imageformat", type=str, default=None,
			help="Image format")   
    args = parser.parse_args()    

    return args
    
    
if __name__ == "__main__":
    # Get files from command line
	args = get_command_line_arguments()
	exp = args.exp
	obs = args.obs
	var = args.var
	freq = args.freq
	start_month = args.start_month
	yearb = args.yearb
	yeare =args.yeare
        calc = args.calc
	dayb = args.dayb
	daye = args.daye
	colmap = args.colmap
	ub = args.ub
	lb = args.lb
	colsteps =  args.colsteps 
	extmethod = args.extmethod 
	units = args.units 
	title = args.title 
	figdir = args.figdir 
	imageformat = args.imageformat
	
	mapstereo = MapStereo(exp, obs, var, freq, start_month, yearb, yeare, 
                 calc, dayb, daye, colmap, ub, lb, colsteps, extmethod, units, 
                 title, figdir, imageformat)
	mapstereo.map_polar()

		
