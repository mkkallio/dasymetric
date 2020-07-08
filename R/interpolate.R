#' Estimate runoff in target features
#'   
#' Estimates runoff in either 
#'
#' @param HS A 'HS' object, obtained with \code{\link{raster_to_HS}}.
#' @param river An 'sf' linestring feature representing a river network.
#' @param basins An 'sf' polygon object.
#' @param dasymetric Column name in \code{river} or in \code{basins} to be used 
#'   as the ancillary information in dasymetric mapping. If \code{NULL} 
#'   (default), no dasymetric mapping is performed. See details.
#' @param pycnophylactic Column in \code{HS} to be used as basis in 
#'   pycnophylactic interpolation. If \code{NULL}, or if \code{is.null(basins)},
#'   not performed. See details.
#' @param n Number of iterations when using pycnophylactic interpolation. 
#'   Default \code{25}.
#' @param intensive Whether the pycnophylactic variable is intensive (density,
#'   like runoff in mm), or not (in which case it is extensive, or counts like
#'   runoff in volume). 
#' @param weights Name of a column in \code{river} to be used directly as 
#'   weights. Defaults to \code{NULL}. See Details.
#' @param aoi An area of interest ('sf' polygon object) used to intersect 
#'   \code{river}. Ignored, if basins provided (in which case, aoi is the union
#'   of \code{basins}). Defaults to \code{NULL}.
#' @param riverID A character string which specifies the name of the column in 
#'   \code{river} containing unique river network identifiers. Defaults to 
#'   \code{"riverID"}.
#' @param verbose Whether or not print progress indicators.
#'   
#'
#' @return Returns a list object with class 'HSweights', containing the following 
#'   elements:
#'   \itemize{
#'     \item \code{river}. The supplied river network with routing information. 
#'       See \code{\link{river_network}} for details.
#'     \item \code{weights}. River network lines or catchment polygon network 
#'       which was used as the basis of weighting. See 
#'       \code{\link{compute_area_weights}} or \code{\link{compute_river_weights}} 
#'       for details.
#'     \item \code{HS}. The input HS object containing runoff information. 
#' }
#'  
#' @export
interpolate_aw <- function(source, 
                           target,
                           dasymetric = NULL,
                           pycnophylactic = NULL,
                           n = 20,
                           intensive = NULL,
                           aoi=NULL, 
                           verbose=FALSE) {
    
    # --------------------------------------------------------------------------
    # test

    test <- inherits(source, "zones")
    if(!test) stop("source should be of class 'zones', obtained with function ",
                   "source_from_raster() or create_source()")
    
    test <- inherits(target, "zones")
    if(!test) stop("target should be of class 'zones', obtained with function ",
                   "create_target()")
    
  
    if(verbose) {
        msg <- paste0("Interpolating from ", nrow(source), " source ",
                      "zones to ", nrow(target), " target zones.")
        message(msg)
    }
    
    
    #############
    # interpolate
    
    
    
    # 1. Intersect river network with union of basins, and intersect HS
    # with the union of basins - this is to make sure that the runoff 
    # units is consistent with the basins. If they are not, weights
    # will not equal to 1 for every runoff unit.
    select <- sf::st_intersects(target, 
                                sf::st_geometry(sf::st_union(source)), 
                                sparse=FALSE)
    target <- target[select,]
    
    
    # 2. compute basin weights
    if(verbose) message("Computing weights..")
  
    weights <- dasymetric:::compute_area_weights(source,
                                                 target,
                                                 pycno = pycnophylactic,
                                                 dasy = dasymetric,
                                                 intensive = intensive,
                                                 weights = NULL)
    
    output <- downscale_runoff(source,
                               target,
                               weights,
                               pycno = FALSE,
                               n = 10,
                               dasy = dasymetric)
    
    
    return(output)
}