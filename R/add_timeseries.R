#' Adds observation timeseries to a 'HS' object
#' 
#' Adds observation timeseries to a HS object. This is needed in order to 
#' evaluate performance of downscaled timeseries, or to perform data assimilation
#' combining several downscaled times.
#' 
#' @param zones An \code{zones} object.
#' @param timeseries a data.frame with observations. Must contain column 
#'   \code{Date}.
#' @param unit The unit of values in \code{timeseries}.
#' @param name Name of the variable
#' @param IDs A vector of riverID of the river segments of the columns
#'   in timeseries. If not provided, IDs will be taken from the column names in
#'   \code{timeseries}.
#'  
#' @return Returns the \code{HS} object with added list column 
#'   \code{observation_ts} containing a timeseries, and column 
#'   \code{observation_station} containing the name of observation stations.
#' 
#' @export
add_timeseries <- function(zones, 
                           timeseries,
                           name,
                           unit = NULL,
                           IDs = NULL) {
    
    # --------------------------------------------------------------------------
    # test inputs
    
    
    if (!any(c("Date") %in% colnames(timeseries))) {
        stop("Observations do not include column 'Date', or 'Month'.")
    } 
    
    test <- inherits(zones, "zones")
    if(!test) {
        stop("First input must be of class 'zones'.")
    }
    
    test <- inherits(timeseries, "data.frame")
    if(!test) {
        stop("timeseries is not a data.frame.")
    }
    
    # check that date-column is available in timeseries
    test <- sapply(timeseries, function(x) {inherits(x, "Date")})
    if(sum(test) == 1) {
        ind <- which(test)
        colnames(timeseries)[ind] <- "Date"
        
    } else if(sum(test) > 1) {
        stop("There are more than 1 column with type 'Date' in the timeseries.")

    } else if(sum(test) == 0) {
        tsnames <- colnames(timeseries) 
        test <- any(tolower(tsnames) == "date")
        if(!test) stop("Did not find a 'Date' column in timeseries.")
        
        ind <- which(tolower(tsnames) == "date")
        timeseries[,ind] <- lubridate::as_date(unlist(timeseries[,ind]))
    }
    

    # test that all IDs are accounted for
    test <- is.null(IDs)
    if(test) {
        IDs <- colnames(timeseries)
        i <- which(tolower(IDs) == "date")
        
        test <- all(zones$sourceID %in% IDs[-i])
        if(!test) stop("timeseries does not include all source zones.")
        
    } else {
        test <- all(zones$sourceID %in% IDs)
        if(!test) stop("timeseries does not include all source zones.")
    }
    
    # reorder columns so that date is first
    ind <- which(tolower(IDs) == "date")
    new_order <- c(ind, c(1:ncol(timeseries))[-ind])
    timeseries <- timeseries[,new_order]
    
    # --------------------------------------------------------------------------
    # process
    
    
    # convert NaNs to NA set unit if provided
    test <- is.null(unit) 
    if(test) {
        for(i in 1:ncol(timeseries)) {
            if( tolower(colnames(timeseries)[i]) == "date") next
            ts <- timeseries[,i]
            ts[is.nan(ts)] <- NA
            timeseries[,i] <- ts
        }
    } else {
        for(i in 1:ncol(timeseries)) {
            if( tolower(colnames(timeseries)[i]) == "date") next
            ts <- timeseries[,i]
            ts[is.nan(ts)] <- NA
            ts <- units::as_units(ts, unit)
            timeseries[,i] <- ts
        }
    }
    
    # test if zones already has some variable_timeseries
    test <- hasName(zones, "variable_ts")
    if(test) {
        variable_ts <- zones$variable_ts
        
        ### test here that variable_ts and timeseries have the same dates
        # and warn if not
        
        variable_ts <- merge(variable_ts, timeseries, by="Date")
        
    } else {
        variable_ts <- timeseries
    }
    

    # --------------------------------------------------------------------------
    # output
    variable_ts <- list(variable_ts)
    names(variable_ts) <- name
    listc <- dasymetric:::spread_listc(variable_ts)
    zones$variable_ts <- listc
    
    # zones <- reorder_cols(HS)
    zones <- assign_class(zones, "zones")
    return(zones)
}


