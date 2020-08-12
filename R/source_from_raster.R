
#' @export
source_from_raster <- function(rasters, 
                               date = NULL, 
                               timestep = NULL, 
                               unit = NULL,
                               aoi = NULL, 
                               names = NULL,
                               verbose = FALSE) {
    
    . <- NULL
    Date <- NULL
    zoneID <- NULL
    
    # -------------------------------------------------------------------------
    # TEST
    
    # test rasters input
    test <- is.list(rasters)
    if(!test) {
        test2 <- is.character(rasters)
        if(test2) {
            rasters <- as.list(rasters)
        } else {
            test3 <- inherits(rasters,  
                              c("RasterStack", "RasterBrick", "RasterLayer"))
            if(test3) {
                rasters <- list(rasters)
            } else {
                stop("rasters input must be either a raster or an ",
                     "URI to a raster")
            }
        }
    }
    
    # test date input
    test <- is.null(date)
    if(test) {
        is_timeseries <- FALSE
    } else {
        is_timeseries <- TRUE
        # test date and timestep object
        test <- length(date) == 1 && is.null(timestep)
        if(test) stop("Please provide timestep")
        
        test <- timestep %in% c("year", "month", "week", "day", "hour")
        if(!test) stop("Unsupported timestep. Supported timesteps are: ",
                       "'year', 'month', 'week', 'day', 'hour'")
        
        test <- length(date) == 1 && !inherits(date, "Date")
        if(test) stop("date input should be a 'Date' object (use e.g. 
                  date('2000/01/01') ), or a vector of dates with 
                  length equal to number of layers in the rasters")
        
        test <- !is.list(date) && length(date) == 1
        if(test) date <- as.list(rep(date, length(rasters)))
        
        test <- !is.list(date) && length(date) != 1
        if(test) date <- as.list(date)
        
        test <- length(date) == length(rasters)
        if(!test) stop("length(date) != length(rasters)")
    }
    
    # test names
    nrasters <- length(rasters)
    test <- is.null(names) 
    if(test) {
        names <- paste0("variable_", seq(1,nrasters,by=1))
    } else {
        test2 <- length(names) == nrasters
        if(!test2) stop("length(names) != length(rasters)")
    }
    
    # test units
    test <- is.null(units)
    if(test) {
        unit <- "1"
        warning("No unit provided - assuming a unitless quantity.")
    }
   
    
    
   
    
    #---------------------------------------------------------------------------
    # PROCESS DATA
    
    if (verbose) {
        message(paste0("Converting ", nrasters, " raster(s) to source zones.."))
        pb <- txtProgressBar(min = 0, max = nrasters, style = 3) 
    } 
    
    # process all rasters
    for(rast in seq_along(rasters)) {
        
        # load the raster in question
        test <- inherits(rasters[[rast]], 
                         c("RasterStack", "RasterBrick", "RasterLayer"))
        if(!test) {
            raster <- raster::brick(rasters[[rast]])
        } else {
            raster <- rasters[[rast]]
        }
        
        # see if aoi exists, whether it is an 'sf' or 'sp', and crop
        if (!is.null(aoi)) {
            
            # if aoi is an 'sf' object, else if its an 'sp' object, else stop
            accepted <- c("sf", "sfc", "sfg")
            if( any(class(aoi) %in% accepted) ) {
                aoi <- methods::as(aoi, "Spatial")
                raster <- raster::crop(raster, aoi, snap="out")
                aoi <- sf::st_as_sf(aoi)
            } else if ( grepl("Spatial", class(aoi), fixed=TRUE) ) {
                raster <- raster::crop(raster, aoi)
                aoi <- sf::st_as_sf(aoi)
            } else {
                stop("Input area of interest should be an object of spatial",
                     " class from either 'sf' or 'sp' packages")
            }
        }
        
        # polygonize
        grid <- raster::rasterToPolygons(raster)
        
        
        # change class to 'sf', intersect grid to aoi, if exists
        if(!is.null(aoi)) {
            grid <- suppressWarnings(
                suppressMessages(
                    methods::as(grid, "sf") %>%
                        sf::st_intersection(., sf::st_union(aoi))
                )
            )
        } else {
            grid <- methods::as(grid, "sf")
        }
        
        # extract only polygons - points or lines are irrelevant for areal
        # interpolation
        grid <- sf::st_collection_extract(grid, "POLYGON")
        
        
        # ----------------------------------------------------------------------
        # create the output
        grid$sourceID <- 1:nrow(grid)

        # if the raster is a timeseries (has more than one layer)
        if(is_timeseries) {
            
            data <- dplyr::select(grid, -sourceID) %>% 
                sf::st_set_geometry(NULL) %>% 
                t() %>% 
                data.frame() 
            colnames(data) <- grid$sourceID
            rownames(data) <- NULL
            
            # process dates
            dates <- date[[rast]]
            
            grid <- dplyr::select(grid, sourceID)
            
            if(length(dates) == 1) {
                if(timestep == "month") {
                    enddate <- dates %m+% months(raster::nlayers(raster) -1)
                } else if(timestep == "day") {
                    enddate <- dates %m+% 
                        lubridate::days(raster::nlayers(raster) -1)
                } else if(timestep == "hour") {
                    enddate <- dates %m+% 
                        lubridate::hours(raster::nlayers(raster) -1)
                } else if(timestep == "year") {
                    enddate <- dates %m+% 
                        lubridate::years(raster::nlayers(raster) -1)
                } else if(timestep == "week") {
                    enddate <- dates %m+% 
                        lubridate::weeks(raster::nlayers(raster) -1)
                } 
                dates <- seq(dates, enddate, by = timestep)
            }  else {
                test <- length(dates) == nlayers(raster)
                if(test) stop(paste0("length(dates) != nlayers(raster) for raster",
                                     " number ",rast))
            }
            data$Date <- dates
            
            if(rast == 1) {
                output <- create_source(grid, ID = "sourceID") %>% 
                    add_timeseries(data, 
                                   name = names[[rast]],
                                   unit = unit)
            } else {
                output <- create_source(grid, ID = "sourceID") %>% 
                    add_timeseries(data, 
                                   name = names[[rast]],
                                   unit = unit)
                output <- combine_zones(output, add)
            }
            
        } else {
            # if the raster has only one layer (= it is not timeseries)
            
            data <- grid %>% 
                sf::st_set_geometry(NULL) %>% 
                data.frame() 
            colnames(data)[1] <- names[[rast]]
            
            grid <- dplyr::select(grid, sourceID)
            
            if(rast == 1) {
                output <- create_source(grid, ID = "sourceID") %>% 
                    dplyr::left_join(data, by="sourceID")
                
            } else {
                output <- dplyr::left_join(output, data, by="sourceID")
            }
            
        }
        
        if (verbose) setTxtProgressBar(pb, rast)
    }
    if(verbose) close(pb)
    
    
    #return
    return(output)
}

#' @export
target_from_raster <- function(rasters, 
                               date = NULL, 
                               timestep = NULL, 
                               unit = NULL,
                               aoi = NULL, 
                               names = NULL,
                               verbose = FALSE) {
    
    target <- source_from_raster(rasters, 
                                 date, 
                                 timestep, 
                                 unit,
                                 aoi, 
                                 names,
                                 verbose)
    
    target <- dplyr::rename(target, targetID = sourceID)
    
    return(target)
}