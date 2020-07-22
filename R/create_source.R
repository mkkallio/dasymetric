#' Constructs a \code{zones} object from given input
#' 
#' Creates a \code{zones} object from sf polygon representing the spatial units 
#' of data. 
#' 
#' @param zones An \code{sf} object containing polygon representation of the 
#'   spatial units.
#' @param ID Column name with unique IDs.
#'  
#' @return Returns a data.frame with an added class \code{zones}, and column
#'   \code{sourceID}
#' 
#' @export
create_source <- function(zones, 
                          ID = "ID") {
    
    # --------------------------------------------------------------------------
    # TEST INPUT
    
    test <- ID %in% colnames(zones)
    if(!test) stop("ID column ", ID, " not found in zones")
    
    test <- ID == "sourceID"
    if(!test) {
        zones$sourceID <- zones[,ID] 
    }

    test <- length(zones$sourceID) == length(unique(zones$sourceID))
    if(!test) stop ("'sourceID' column contains duplicates. Please ensure ", 
                    "that each ID is unique.")
    
    # --------------------------------------------------------------------------
    # create output
    zones <- sf::st_as_sf(tibble::as_tibble(zones))
    zones <- dasymetric:::assign_class(zones, c("zones"))
    
    return(zones)
}



#' Constructs a \code{zones} object from given input
#' 
#' Creates a \code{zones} object from sf polygon representing the spatial units 
#' of data. 
#' 
#' @param zones An \code{sf} object containing polygon representation of the 
#'   spatial units.
#' @param ID Column name with unique IDs.
#'  
#' @return Returns a data.frame with an added class \code{zones}, and column
#'   \code{targetID}
#' 
#' @export
create_target <- function(zones, 
                          ID) {
    
    # --------------------------------------------------------------------------
    # TEST INPUT
    
    test <- ID %in% colnames(zones)
    if(!test) stop("ID column ", ID, " not found in zones")
    
    
    zones$targetID <- zones$ID 
    test <- length(zones$targetID) == length(unique(zones$targetID))
    if(!test) stop (ID, " column contains duplicates. Please ensure ", 
                    "that each ID is unique.")
    
    # --------------------------------------------------------------------------
    # create output
    
    zones <- sf::st_as_sf(tibble::as_tibble(zones))
    output <- assign_class(output, c("zones"))
    
    return(output)
}
