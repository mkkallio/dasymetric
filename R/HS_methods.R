#' @method print zones
#' @export 
print.zones <- function(x, ...) {
    
    cat("\nHydrostreamer")
    cat("\n")
    cat(paste0("No. objects: ", nrow(x)))
    cat("\n")
    
    if(hasName(x, "runoff_ts")){
        nc <- ncol(x$runoff_ts[[1]])-1
        cat(paste0("No. runoff timeseries: ", nc))
        cat("\n")
        if(nc < 10) {
            cat("  Included runoff timeseries: ")
            cat(colnames(x$runoff_ts[[1]][-1]))
            cat("\n")
        }
    }
    
    if(hasName(x, "discharge_ts")){
        nc <- ncol(x$discharge_ts[[1]])-1
        cat(paste0("No. discharge timeseries: ", nc))
        cat("\n")
        if(nc < 10) {
            cat("  Included discharge timeseries: ")
            cat(colnames(x$discharge_ts[[1]][-1]))
            cat("\n")
        }
    }
    
    if(hasName(x, "observation_station")) {
        stations <- unique(x$observation_station)
        stations <- stations[!is.na(stations)]
        
        cat(paste0("No. observation stations: ", length(stations)))
        cat("\n")
        if(length(stations) < 10) {
            cat("  Stations: ", paste(stations, sep=" "))
            cat("\n")
        }
        
    }
    
    if(hasName(x, "control_ts")) {
        controls <- table(x$control_type)
        
        cat(paste0("No. of flow controls: ", sum(controls)))
        cat("\n")
        if(sum(controls) < 10) {
            cat("  Control types: ")
            cat(controls)
            cat("\n")
        }
    }
    cat("\n")
    NextMethod()
}


#' @method plot zones
#' @export 
plot.zones <- function(x, ...) {
    
    observation_ts <- NULL
    control_ts <- NULL
    
    test <- hasName(x, "PREVIOUS")
    if(test) x$PREVIOUS <- lapply(x$PREVIOUS, 
                                  function(x) {
                                      paste(x, collapse=" ")
                                  }) %>% unlist()
    
    test <- hasName(x, "runoff_ts") 
    if (test) {
        x$runoff_ts <- rep(TRUE, nrow(x))
    }
    
    test <- hasName(x, "discharge_ts") 
    if (test) {
        x$discharge_ts <- rep(TRUE, nrow(x))
    }
    
    test <- hasName(x, "Optimisation_info") 
    if (test) {
        x$Optimisation_info <- !sapply(x$Optimisation_info, is.null)
    }
    
    test <- hasName(x, "observation_ts") 
    if (test) {
        #x$observation_ts <- !sapply(x$observation_ts, is.null)
        x <- dplyr::select(x, -observation_ts)
    }
    
    test <- hasName(x, "control_ts") 
    if (test) {
        #x$control_ts <- !sapply(x$control_ts, is.null)
        x <- dplyr::select(x, -control_ts)
    } 
    
    NextMethod()
}


