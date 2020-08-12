#!/usr/bin/env Rscript 

setwd("/username/data/")
library(s2dverification)
library(startR)
library(lubridate)

# -----------------------------------------------------------------------------------------------------
# > Script for loading netCDF files for sets of retrospective climate predictions and satellite data
#   -Calculate climatologies for each forecast time
#   -Calculate mean bias
#   [When everything ends, go to MapStereo.py to plot daily bias]
# -----------------------------------------------------------------------------------------------------

#=== Choose the dates, variables, experiments, observations and initialization month ===#
pathin = '/username/data/'
YEAR1 = 1988 # First year to be loaded
YEAR2 = 2012 # Last year to be loaded
lst_var = c('sic') # c('sic', 'psl', 'tos')
lst_exp = c('a1q0') # Experiment ID
obs = 'NSIDC' # Observational Dataset
lst_months = c('nov') # c('nov', 'may') # November or May initialized predictions

for (var in lst_var) {

  if (var == 'sic' || var == 'sit') {
    field = 'seaIce'
  } else if (var == 'tos' || var == 'sos') {
    field = 'ocean'
  } else if (var == 'tos' || var == 'sos') {
    field = 'atmos'
  }

  year1 = as.character(YEAR1)
  year2 = as.character(YEAR2)      
  
  for (exp in lst_exp) {
    
    for (start_month in lst_months) {
      
      # === Load the Experiment data === #
      #==================================#
      path_exp = paste('/archive/exp/ecearth/', exp, '/cmorfiles/EC-Earth-Consortium/',
                       'EC-Earth3-LR/historical/S$sdate$/day/', field, '/$var$/r360x180/$member$/v20190201/',
                       '$var$_day_EC-Earth3-LR_*S$sdate$_$member$_$chunk$.nc', sep='')
      start_dates = switch(start_month, 'may'= paste(seq(YEAR1, YEAR2, 1),'0501', sep=''), 
                           'nov' = paste(seq(YEAR1, YEAR2, 1),'1101', sep='')) 
      savename_exp = paste(var, '_day_', exp,'_', start_month, '_', year1, '-', year2, '.RData', sep='')
      
      # startR package allows to retrieve large multi-dimensional distributed
      # data sets (following the MapReduce paradigm)

      if (file.exists(paste(pathin, savename_exp, sep=''))) {
        load(paste(pathin, savename_exp, sep=''))
      } else {
        data_exp = Start(dataset = path_exp,
                          var = var,
                          member = paste0('r', c(2:5, 7:10), 'i1p1'), # member = 'all',
                          sdate = start_dates,
                          time = 'all',
                          chunk = 'all',
                          chunk_depends = 'sdate',
                          time_across = 'chunk',
                          lat = values(list(47, 90)), # Cut the latitudes (to focus just on the Arctic)
                          lon = 'all',
                          lon_reorder = CircularSort(0, 360),
                          return_vars = list(lat = NULL, lon = NULL),
                          retrieve = TRUE)
        
        latitudes = rev(attr(data_exp, 'Variables')$common$lat)
        longitudes = rev(attr(data_exp, 'Variables')$common$lon)
        dim(data_exp) # 1 x 1 x 8 x 25 x 214 x 1 x 43 x 360
        model_data = data_exp[1,1,,,1:212,1,,] # dims = (members, start dates, forecast time, lat, lon)
        save(model_data, latitudes, longitudes, file=paste(pathin, savename_exp, sep=''))
      }
      
      # === Load the observations === # 
      #===============================#
      if (obs = 'nsidc') {
        sdate_obs = '19880201'
      } else {
        stop("Write the start date of your observational dataset to load it")
      }
      
      savename_obs = paste(var, '_day_', obs,'_', sdate_obs, '.RData', sep='')
      savename_obs_formatted = paste(var, '_day_', obs, '_', start_month, '_', year1, '-', year2, '_formatted.RData', sep='')

      if (!file.exists(paste(pathin, savename_obs_formatted, sep=''))) {
        if (file.exists(paste(pathin, savename_obs, sep=''))) {
          load(paste(pathin, savename_obs, sep=''))
          print(paste("Loaded ", savename_obs, sep=''))
        } else { 
          
          path_obs = paste('/archive/obs/daily_mean/', obs, '/', var, '/r360x180/', obs, '_$date$.nc', sep='')
          data_obs = Start(dataset = path_obs,
                            date = sdate_obs, 
                            var = var,
                            time = 'all',
                            time_across = 'date',
                            lat = values(list(47, 90)),
                            lon = 'all',
                            lon_reorder = CircularSort(0, 360),
                            synonims = list(lon = c('XAX', 'lon'), lat = c('YAX', 'lat'), time = c('TIME', 'time')),
                            retrieve = TRUE)
          
          dim(data_obs) # dims = (1 x 1 x 1 x 10562 x 43 x 360)
          dim(data_obs) = c(1, length(data_obs[1,1,1,,1,1]), 43, 360)
          save(data_obs, file = paste(pathin, savename_obs, sep=''))
          print(paste("Saved ", savename_obs, sep=''))
        } 

        # Data have to be formatted from daily continous dimensions of satellite data to the format of seasonal predictions
        # Typically with multiple dimensions: c(model, experiment, start dates, forecast time, member, latitude, longitude)
        source('h2h.R')
        # https://github.com/rcruzgar/analyses/tree/master/maps/h2h.R 
        
        s_dates = ymd(start_dates)
        nr_years = length(YEAR1:YEAR2)
        
        obs_formatted = array(dim=c(1, nr_years, length(model_data[1,1,,1,1]), 43, 360)) 
        for (j in 1:360) {
          for (i in 1:43) {
            variable = data_obs[,,i,j,drop=F]
            dim(variable) = c(1, length(data_obs[1,,1,1])) 
            names(dim(variable)) = c('dat', 'ftime')
            obs_formatted[,,,i,j] = h2h(variable,
                                         sdate = as.POSIXct(ymd(sdate_obs)),
                                         sdatesout = as.POSIXct(s_dates),
                                         n_ftimes = length(model_data[1,1,,1,1]),
                                         freq = as.difftime(1, units = 'days'),
                                         calendar_in ='leap', calendar_out = 'leap')
          }
        }
        
        rm(variable, data_obs)
        save(obs_formatted, file=paste(pathin, savename_obs_formatted, sep=''))
        print(paste("Saved ", savename_obs_formatted, sep=''))
      
      } else {
        load(paste(pathin, savename_obs_formatted, sep=''))
        print(paste("Loaded ", savename_obs_formatted, sep=''))
      }

      # === Calculate Mean Bias: EXP minus OBS === #
      #============================================#
      ens_mean_exp = Mean1Dim(model_data, 1) # Ensemble Mean 
      exp_date_mean = Mean1Dim(ens_mean_exp, 1) # Start Date Mean
      dim(exp_date_mean) = c(1, length(model_data[1,1,,1,1]), 43, 360)
      
      obs_date_mean = Mean1Dim(obs_formatted, 2) # Start Date Mean
      
      bias_exp_obs = exp_date_mean - obs_date_mean
      
      savename_bias_exp_obs = paste(var, '_day_', exp,'_', obs, '_', start_month, '_', year1, '_', year2, '_bias.RData', sep='')
      if (file.exists(paste(pathin, savename_bias_exp_obs, sep=''))) {
        load(file = paste(pathin, savename_bias_exp_obs, sep=''))
      } else {
        save(bias_exp_obs, file = paste(pathin, savename_bias_exp_obs, sep=''))
        print(paste("Saved ", savename_bias_exp_obs, sep=''))
      }
      
    }
  }
}



