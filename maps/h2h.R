h2h <- function(var, sdate, sdatesout, n_ftimes, 
                ftime_dim = NULL, sdate_dim = NULL, 
                freq = 'month') {
  # Check var
  if (!is.array(var) || !is.numeric(var)) {
    stop("Parameter 'var' must be a numeric array.")
  }

  # Check sdate
  if (!('POSIXct' %in% class(sdate))) {
    stop("Parameter 'sdate' must be a POSIXct date.")
  }
  sdate <- sdate[1]

  # Check sdatesout
  if (!('POSIXct' %in% class(sdatesout))) {
    stop("Parameter 'sdatesout' must be a POSIXct date.")
  }

  # Check n_ftimes
  if (!is.numeric(n_ftimes)) {
    stop("Parameter 'n_ftimes' must be numeric.")
  }
  n_ftimes <- round(n_ftimes[1])

  # Check ftime_dim
  dims <- dim(var)
  if (is.null(ftime_dim)) {
    if (!is.null(names(dims))) {
      if ('ftime' %in% names(dims)) {
        ftime_dim <- which(names(dims) == 'ftime')
      } else {
        stop("Parameter 'var' must have a dimension named 'ftime' if 'ftime_dim = NULL'.")
      }
    } else {
      stop("Parameter 'var' must have dimension names if 'ftime_dim = NULL'.")
    }
  }
  if (is.character(ftime_dim)) {
    ftime_dim <- which(names(dims) == ftime_dim)
    if (length(ftime_dim) == 0) {
      stop("Could not find the specified 'ftime_dim' in 'var' dimension names.")
    }
  } else if (!is.numeric(ftime_dim)) {
    stop("Parameter 'ftime_dim' must be numeric.")
  }
  ftime_dim <- round(ftime_dim[1])
  # Check sdate_dim
  # A 0 will be assigned when the sdate_dim is not present and needs to be added later
  if (is.null(sdate_dim)) {
    if (!is.null(names(dims))) {
      if ('sdate' %in% names(dims)) {
        sdate_dim <- which(names(dims) == 'sdate')
      } else {
        sdate_dim <- 0
      }
    } else {
      sdate_dim <- 0
    }
  }
  if (is.character(sdate_dim)) {
    sdate_dim <- which(names(dims) == sdate_dim)
    if (length(sdate_dim) == 0) {
      stop("Could not find the specified 'sdate_dim' in 'var' dimension names.")
    }
  } else if (!is.numeric(sdate_dim)) {
    stop("Parameter 'sdate_dim' must be numeric.")
  }
  sdate_dim <- round(sdate_dim[1])

  # Check freq
  if (!('difftime' %in% class(freq))) {
    if (freq == 'min') {
      freq <- as.difftime(1, units = 'mins')
    } else if (freq == 'day') {
      freq <- as.difftime(1, units = 'days')
    } else if (freq %in% c('month', 'year')) {
    } else {
      stop("Parameter 'freq' must be either of the class 'difftime' or one ",
           " of 'month' or 'year'.")
    }
  }

  # Create the list_of_dates, containing a list of vectors each
  # with the dates of the values of each sdate
  # Create the dates_in, a vector of the dates of the input data
  # Create tolerance according to freq
  dates_in <- NULL
  if ('difftime' %in% class(freq)) {
    list_of_dates <- lapply(sdatesout, 
                            function(x) {
                              seq(x, by = freq, length.out = n_ftimes)
                            })
    dates_in <- seq(sdate, by = freq, length.out = dims[ftime_dim])
    tolerance <- 0.5 * freq
  } else if (freq == 'month') {
    # Monthly series
    # x has to be 'YYYYMMDD'
    # n_ftimes number of dates to generate
    generate_dates <- function(x, n_ftimes) {
      syear <- as.integer(substr(as.character(x), 1, 4))
      smonth <- as.integer(substr(as.character(x), 6, 7))
      sday <- as.integer(substr(as.character(x), 9, 10))
      months_seq <- (((1:12 + smonth - 1) - 1) %% 12) + 1
      month_inds <- ((1:n_ftimes - 1) %% length(months_seq)) + 1
      months_seq <- months_seq[month_inds]
      year_changes <- which(months_seq == 12)
      as.POSIXct(
        sapply(1:n_ftimes, 
               function(y) {
                 offset_years <- sum(year_changes < y)
                 paste0(syear + offset_years, '-', months_seq[y], '-', sday)
               })
      )
    }
    list_of_dates <- lapply(sdatesout, generate_dates, n_ftimes)
    dates_in <- generate_dates(sdate, dims[ftime_dim])
    tolerance <- as.difftime(15, units = 'days')
  } else if (freq == 'year') {
    stop("Feature 'freq = \"year\"' still to be implemented.")
  }

  # Work out the dimensions of the output array
  new_dims <- dims
  new_dims[ftime_dim] <- n_ftimes
  sdate_dim_out <- 0
  if (sdate_dim > 0) {
    new_dims[sdate_dim] <- length(sdatesout)
  } else {
    new_dims_2 <- NULL
    if (ftime_dim > 1) {
      new_dims_2 <- new_dims[1:(ftime_dim - 1)]
    }
    sdate_dim_out <- length(new_dims_2) + 1
    new_dims_2 <- c(new_dims_2, sdate = length(sdatesout), new_dims[ftime_dim:length(new_dims)])
    new_dims <- new_dims_2
  }

  # Create output array
  var_out <- array(dim = new_dims)

  # Populate output array with corresponding values
  for (i in 1:length(sdatesout)) {
    # First select the values to take for each start date
    indices <- as.list(rep(TRUE, length(dims)))
    indices[[ftime_dim]] <- sapply(1:n_ftimes, 
      function(x) {
        diffs <- abs(dates_in - list_of_dates[[i]][x])
        match <- which.min(diffs)
        if (diffs[match] <= tolerance) {
          match[1]
        } else {
          NA
        }
      })
    if (all(is.na(indices[[ftime_dim]]))) {
      value <- rep(NA, n_ftimes)
    } else {
      value <- do.call('[', c(list(x = var), indices, list(drop = FALSE)))
    }
    # Then put into output array
    indices <- as.list(rep(TRUE, length(new_dims)))
    indices[[sdate_dim_out]] <- i
    var_out <- do.call('[<-', c(list(x = var_out), indices, list(value = value)))
  }

  var_out
}
