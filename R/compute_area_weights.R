#' Compute weights for river catchment areas within the runoff area 
#' features. 
#' 
#' Computes weights for each individual river segment specific catchments 
#' falling in the areal units of the runoff \emph{source}. Function first 
#' takes a union between \emph{basins} and \emph{source} (creating new 
#' catchment units which fall inside only one runoff unit), and calculating 
#' the area for each individual catchment unit. The weight is assigned by 
#' dividing the area of sub-catchment with the area of runoff unit.
#'

#' @inheritParams compute_HSweights
#'
#' @return Returns an 'sf' polygon feature (a union of basins, and HS) 
#'   with added attributes (columns):
#'   \itemize{
#'     \item \emph{ID}. Unique ID of the feature.
#'     \item \emph{riverID}. ID of the river segment each sub-catchment is 
#'       associated to.
#'     \item \emph{sourceID}. ID of the runoff unit the sub-catchment 
#'       is contained in.
#'     \item \emph{weights}. Weights computed for each sub-catchment.
#'     \item \emph{target_area}. Area of the sub-catchment (basin) in 
#'       \eqn{m^2}.
#'     \item \emph{source_area}. Area of the runoff unit sub-catchment is 
#'       contained in.
#' }
#' 
compute_area_weights <- function(source, 
                                 target,
                                 pycno = NULL,
                                 dasy = NULL,
                                 weights = NULL,
                                 n = 20,
                                 intensive = TRUE) {
    
    area <- NULL
    weights <- NULL
    ID <- NULL
    target_area <- NULL
    source_area <- NULL
    
    # TEST INPUTS
    #############
    
    test <- is.null(pycno)
    if(test) {
        pycnophylactic <- FALSE
    } else {
        pycnophylactic <- TRUE
        test <- hasName(source, pycno)
        if(!test) stop("No column ", pycno," in target")
        test <- sum(is.null(source[,pycno]))
        test2 <- sum(is.na(source[,pycno]))
        if(test+test2 > 0) stop("Missing values in column ", pycno)
    }
    
    test <- is.null(dasy)
    if(test) {
        dasymetric <- FALSE
    } else {
        dasymetric <- TRUE
        test <- hasName(target, dasy)
        if(!test) stop("No column ", dasy," in target")
        test <- sum(is.null(target[,dasy]))
        test2 <- sum(is.na(target[,dasy]))
        if(test+test2 > 0) stop("Missing values in column ", dasy)
    }
    
    
    # --------------------------------------------------------------------------
    # PROCESS

    

    # intersection between source and target zones
    target <- suppressWarnings(source %>% 
        dplyr::select(sourceID, !!pycno) %>%
        sf::st_intersection(dplyr::select(target, targetID, !!dasy), .) %>% 
        sf::st_collection_extract("POLYGON") %>% 
        dplyr::arrange(targetID) %>% 
        dplyr::mutate(., target_area = sf::st_area(.),
                      target_area = units::set_units(target_area, "km^2")) %>%
        dplyr::filter(target_area != units::set_units(0, "m^2")) %>% 
        dplyr::group_by(sourceID) %>% 
        dplyr::mutate(source_area = sum(units::drop_units(target_area))) %>% 
        dplyr::ungroup() %>% 
        dplyr::mutate(source_area = units::set_units(source_area, "km^2")))
        
    
    
    # --------------------------------------------------------------------------
    # if pycnophylactic variable is provided
   if(pycnophylactic) {
        pycno_var <- dplyr::pull(source, pycno)
        names(pycno_var) <- source$sourceID
        
        #identify neighbours
        touching <- sf::st_touches(target)
        
        #identify boundary
        boundary <- sf::st_touches(target, 
                                   sf::st_union(target) %>%
                                       sf::st_cast("POLYGON") %>%
                                       sf::st_cast("LINESTRING"),
                                   sparse = FALSE) %>%
            as.numeric()
        boundary[boundary == 0] <- NA
        
        test <- hasName(source, "area")
        if(test) {
            gridareas <- source$area
        } else {
            gridareas <- units::set_units(st_area(source), "km2")
        }
        
        if(dasymetric) {
            dasymetric_var <- dplyr::pull(target, dasy)
            pycno_res <- iterate_pycno(target, 
                                       dasy = dasymetric_var,
                                       touching, 
                                       boundary,
                                       pycno_var, 
                                       gridareas, 
                                       n,
                                       convert=!intensive)
            
        } else {
            pycno_res <- iterate_pycno(target, 
                                       dasy = NULL,
                                       touching, 
                                       boundary,
                                       pycno_var,
                                       gridareas, 
                                       n,
                                       convert=!intensive)
        }
        
        pycno_res <- pycno_res * units::drop_units(target$target_area)
        
        
        target <- target %>%
            dplyr::mutate(weights = pycno_res) %>%
            dplyr::group_by(sourceID) %>%
            dplyr::mutate(sum = sum(weights),
                          weights = weights/sum) %>%
            dplyr::ungroup() %>%
            dplyr::select(-sum) 
        
    # --------------------------------------------------------------------------
    # if pycnophylactic variable is not provided
    } else if(!pycnophylactic && dasymetric) { # replicated above
        dasymetric_var <- dplyr::pull(target, dasy)
        
        target <- target %>%
            tibble::add_column(variable = dasymetric_var) %>%
            dplyr::group_by(sourceID) %>%
            dplyr::mutate(bas_dasy = variable*units::drop_units(target_area),
                          denom = sum(bas_dasy),
                          weights = bas_dasy/denom) %>%
            dplyr::ungroup() %>%
            dplyr::select(-variable, -bas_dasy, -denom)
        
    # --------------------------------------------------------------------------
    # if pycnophylacticor dasymetric variables are not provided
    } else if(!pycnophylactic && !dasymetric) {
        target <- dplyr::mutate(target, 
                                weights = target$target_area/target$source_area) 
    } else {
        stop("error")
    }
    
    # reorder and add columns 
    if (!hasName(target, "ID")) target$ID <- 1:nrow(target)

    target <- target %>% 
        dplyr::select(ID, targetID, sourceID, weights,
                                       target_area, source_area)
    
    return(target)
}
