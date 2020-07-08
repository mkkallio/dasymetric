#' Combine \code{zones} objects or add new runoff input from raster.
#' 
#' Combines the \code{runoff_ts} columns from two zones objects, or adds new
#' runoff timeseries from a raster. **All** geometries in zones must be present
#' in the object to be added, or combination will not done.
#'
#' @param zones an existing \code{zones} object.
#' @param from A \code{zones} object to add from. Optional.
#' @inheritParams source_from_raster
#' 
#' @return Returns the input \code{zones} object with added columns in 
#'   runoff_ts column.
#' 
#' @export
combine_zones <- function(zones, 
                          from=NULL, 
                          rasters = NULL, 
                          unit = NULL,
                          date = NULL, 
                          timestep = NULL, 
                          aoi = NULL, 
                          names=NULL) {
    
    test <- inherits(zones, "zones")
    if(!test) {
        stop("First input should be of class 'zones'")
    }

    if (is.null(from)) {
        from <- raster_to_HS(rasters, unit, date, timestep, aoi, names)
    }
    
    test <- inherits(from, "zones")
    if(!test) {
        stop("'from' input should be of class 'zones'")
    }
    
    # test which elements of from are included in HS
    common_elements <- sf::st_equals(from, HS, sparse = FALSE)
    
    test <- colSums(common_elements)
    if(!all(test == 1)) {
        stop("Areal units in 'zones' and 'from' do not match. Please make sure",
             " that all units in 'zones' are also included in 'from'.")
    }
    
    # test that both zones and from have variable ts
    test <- hasName(zones, "variable_ts") & hasName(from, "variable_ts")
    
    
    # add timeseries from variable_ts in 'from' to variable_ts in 'zones'
    # for all elements in common (same ID)
    if(test) {
        for(i in 1:nrow(common_elements)) {
            gid <- which(common_elements[i,])
            if (length(gid) == 0) next
            
            fr <- from$variable_ts[[i]]
            variable_names <- colnames(fr)
            
            test <- any(variable_names[-1] %in% colnames(zones$variable_ts[[gid]]))
            if(test) warning("Variable timeseries to be combined contain same
                         names - possible duplicated timeseries.")
            
            variable_ts <- merge(zones$variable_ts[[gid]], 
                                 fr,
                                 by="Date",
                                 all.x=TRUE)
            zones$variable_ts[[gid]] <- variable_ts
            
        } 
    }
    
    variable_ts <- zones$variable_ts
    
    zones <- merge(zones[, -c("variable_ts")],
                   sf::st_set_geometry(from[, -c("variable_ts")], NULL),
                   by="sourceID")
    zones["variable_ts"] <- variable_ts
    
       
    zones <- assign_class(zones, c("zones"))
    return(zones)
}
