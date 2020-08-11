#!/usr/bin/env Rscript 

setwd("/username/data/")
library(s2dverification)
library(startR)

pathin = '/username/data/'

YEAR1 = 1988 # First year to be loaded
YEAR2 = 2012 # Last year to be loaded
lst_var = c('sic') # c('sic', 'psl', 'tos') # Variable (Sea Ice Concentration)
lst_exp = c('a1q0') # Experiment ID
obs = 'NSIDC'
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
    
    path_exp = paste('/esarchive/exp/ecearth/', exp, '/cmorfiles/EC-Earth-Consortium/',
                      'EC-Earth3-LR/historical/S$sdate$/day/', field, '/$var$/r360x180/$member$/v20190201/',
                      '$var$_day_EC-Earth3-LR_historical_*S$sdate$_$member$_$chunk$.nc', sep='')

    for (start_month in lst_months) {
      
      start_dates = switch(start_month, 'may'= paste(seq(YEAR1, YEAR2, 1),'0501', sep=''), 
                           'nov' = paste(seq(YEAR1, YEAR2, 1),'1101', sep='')) 
      
      # Load the Experiment (retrospective seasonal climate predictions)
      savename_exp = paste(var, '_day_', exp,'_', start_month, '_', year1, '-', year2, '.RData', sep='')
      
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
        dim(data_exp) # 1 x 1 x 10 x 35 x 214 x 1 x 43 x 360
        model_data = data_exp[1,1,,,1:212,1,,] # dims = (members, start_dates, forecast_time, lat, lon)
        save(model_data, latitudes, longitudes, file=paste(pathin, savename_exp, sep=''))
      }
      
      # Load the interpolated observations # 
      savename_obs = paste(var, '_day_', obs,'_', year1,'0201-20161231.RData', sep='')
      savename_obs_formatted = paste(var, '_day_', obs, '_', start_month, '_', year1, '-', year2, '_formatted.RData', sep='')

      if (!file.exists(paste(pathin, savename_obs_formatted, sep=''))) {
        if (file.exists(paste(pathin, savename_obs, sep=''))) {
          load(paste(pathin, savename_obs, sep=''))
          print(paste("Loaded ", savename_obs, sep=''))
        } else { 
          # if ()
          nsidc = paste('/archive/obs/daily_mean/', obs, '/', var,'/interpolated/', obs,'_$date$.nc', sep='')
          data_obs = Start(dataset = nsidc,
                            date = '19880201',
                            var = var,
                            time = 'all',
                            time_across = 'date',
                            lat = values(list(47, 90)),
                            lon = 'all',
                            lon_reorder = CircularSort(0, 360),
                            synonims = list(lon = c('XAX', 'lon'), lat = c('YAX', 'lat'), time = c('TIME', 'time')),
                            retrieve = TRUE)
          # dims = (1 x 1 x 1 x 10562 x 43 x 360)
          dim(data_obs)
          save(data_obs, file = paste(pathin, savename_obs, sep=''))
          print(paste("Saved ", savename_obs, sep=''))
          
          OBS2 = data_obs
          dim(OBS2) = c(1, length(data_obs[1,1,1,,1,1]), 43, 360)
          
          source('h2h.R') # Function to convert the continous satellite dataset to the format of seasonal predictions.
          # Typically with multiple dimensions: c(model, experiment, start dates, forecast time, member, latitude, longitude).
          
          s_dates = switch(start_month, 'may'= paste(seq(YEAR1, YEAR2, 1),'-05-01', sep=''),
                           'nov' = paste(seq(YEAR1, YEAR2, 1),'-11-01', sep=''))
          
          nr_years = YEAR2 - YEAR1 + 1
          obs_formatted = array(dim=c(1,nr_years,212,43,360)) 
          for (j in 1:360) {
            for (i in 1:43) {
              variable = OBS2[,,i,j,drop=F]
              dim(variable) = c(1, length(Data_OBS[1,1,1,,1,1])) 
              names(dim(variable)) = c('dat', 'ftime')
              obs_formatted[,,,i,j] = h2h(variable,
                                           sdate = as.POSIXct('1988-02-01'),
                                           sdatesout = as.POSIXct(s_dates),
                                           n_ftimes = 212,
                                           freq = as.difftime(1, units = 'days'),
                                           calendar_in ='leap', calendar_out = 'leap')
            }
          }
          
          rm(variable)
          save(obs_formatted, file=paste(pathin, savename_obs_formatted, sep=''))
          print(paste("Saved ", savename_obs_formatted, sep=''))
        }
      } else {
        load(paste(pathin, savename_obs_formatted, sep=''))
        print(paste("Loaded ", savename_obs_formatted, sep=''))
      }

      # Bias calculation: EXP minus OBS
      ens_mean_exp = Mean1Dim(model_data, 1)
      exp_date_mean = Mean1Dim(ens_mean_exp, 1) # c(212, 43, 360)
      dim(exp_date_mean) = c(1, 212, 43, 360)
      obs_date_mean = Mean1Dim(obs_formatted, 2) # c(212, 43, 360)
      
      bias_exp_obs = exp_date_mean - obs_date_mean
      
      savename_bias_exp_obs = paste(var, '_day_', exp,'-', obs, '_', start_month, '_', year1, '_', year2,'_bias.RData', sep='')
      if (file.exists(paste(pathin, savename_bias_a1q0_NSIDC0051, sep=''))) {
        load(file = paste(pathin, savename_bias_a1q0_NSIDC0051, sep=''))
      } else {
        save(bias_exp_obs, file = paste(pathin, savename_bias_a1q0_NSIDC0051, sep=''))
      }
      

    } # Close loops
  }
}



