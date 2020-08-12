
#' @export
interpolate <- function(source, 
                        target,
                        variable,
                        dasymetric = NULL,
                        pycnophylactic = NULL,
                        pycno_iter = 20,
                        pycno_each_ts = FALSE,
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
    
    
    #### TODO: TEST VARIABLE INPUT
    
    test <- hasName(source, variable) 
    if(!test) {
        test <- hasName(source$variable_ts[[1]], variable)
        if(!test) stop("Column named ", variable, " not found in source zones.")
    }
    
    
    if(verbose) {
        msg <- paste0("Interpolating from ", nrow(source), " source ",
                      "zones to ", nrow(target), " target zones.")
        message(msg)
    }
    
    
    # --------------------------------------------------------------------------
    # interpolate
    
    
    # remove target zones not in source
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
                                                 n = pycno_iter,
                                                 weights = NULL)
    
    
    
    output <- dasymetric::downscale_runoff(source,
                               target,
                               weights,
                               variable,
                               pycno = pycno_each_ts,
                               n = pycno_iter,
                               dasy = dasymetric)
    
    
    return(output)
}

#' @export
interpolate_aw <- function(source, 
                           target,
                           variable,
                           std_var = NULL,
                           dasymetric = NULL,
                           verbose=FALSE) {
  
  
  # --------------------------------------------------------------------------
  # test
  
  test <- inherits(source, "zones")
  if(!test) stop("source should be of class 'zones', obtained with function ",
                 "source_from_raster() or create_source()")
  
  test <- inherits(target, "zones")
  if(!test) stop("target should be of class 'zones', obtained with function ",
                 "create_target()")
  
  
  #### TODO: TEST VARIABLE INPUT
  
  test <- hasName(source, variable) 
  if(!test) {
    test <- hasName(source$variable_ts[[1]], variable)
    if(!test) stop("Column named ", variable, " not found in source zones.")
  }
  
  
  if(verbose) message("Interpolating from ", nrow(source), " source ",
                      "zones to ", nrow(target), " target zones.")
  
  # --------------------------------------------------------------------------
  # interpolate
  
  
  # remove target zones not in source
  select <- sf::st_intersects(target, 
                              sf::st_geometry(sf::st_union(source)), 
                              sparse=FALSE)
  target <- target[select,]
  
  
  # 2. compute basin weights

  
  output <- do_interpolation_aw(source,
                                target,
                                variable = variable,
                                std_var = std_var,
                                verbose = verbose)
  
  
  return(output)
}






#' @export
interpolate_dm <- function(source, 
                           target,
                           variable,
                           std_var = NULL,
                           dasymetric = NULL,
                           verbose=FALSE,
                           ...) {
  
  # --------------------------------------------------------------------------
  # test
  
  test <- inherits(source, "zones")
  if(!test) stop("source should be of class 'zones', obtained with function ",
                 "source_from_raster() or create_source()")
  
  test <- inherits(target, "zones")
  if(!test) stop("target should be of class 'zones', obtained with function ",
                 "create_target()")
  
  
  #### TODO: TEST VARIABLE INPUT
  
  test <- hasName(source, variable) 
  if(!test) {
    test <- hasName(source$variable_ts[[1]], variable)
    if(!test) stop("Column named ", variable, " not found in source zones.")
  }
  
  
  if(verbose) message("Interpolating from ", nrow(source), " source ",
                      "zones to ", nrow(target), " target zones.")
  
  
  
  # --------------------------------------------------------------------------
  # interpolate
  
  
  # remove target zones not in source
  select <- sf::st_intersects(target, 
                              sf::st_geometry(sf::st_union(source)), 
                              sparse=FALSE)
  target <- target[select,]
  
  
  # 2. compute basin weights
  
  output <- do_interpolation_dm(source,
                                target,
                                variable = variable,
                                std_var = std_var,
                                dasy = dasymetric,
                                verbose = verbose,
                                ...)
  
  
  return(output)
}



#' @export
interpolate_pp <- function(source, 
                           target,
                           variable,
                           std_var = NULL,
                           n = 10,
                           verbose = FALSE) {
  

  
  output <- interpolate_ppdm(source,
                             target,
                             variable,
                             dasymetric = NULL,
                             std_var,
                             n,
                             verbose)
  return(output)
}


#' @export
interpolate_ppdm <- function(source, 
                           target,
                           variable,
                           dasymetric,
                           std_var = NULL,
                           n = 10,
                           verbose = FALSE) {
  
  ## TODO: should pycnophylactic variable, and variable
  ## to be downscaled be different? yes
  
  ## TODO: pycno var and dasymetric var timeseries and other one not?


  # --------------------------------------------------------------------------
  # test

  test <- inherits(source, "zones")
  if(!test) stop("source should be of class 'zones', obtained with function ",
                 "source_from_raster() or create_source()")

  test <- inherits(target, "zones")
  if(!test) stop("target should be of class 'zones', obtained with function ",
                 "create_target()")


  #### TODO: TEST VARIABLE INPUT

  test <- hasName(source, variable)
  if(!test) {
    test <- hasName(source$variable_ts[[1]], variable)
    if(!test) stop("Column named ", variable, " not found in source zones.")
  }


  if(verbose) message("Interpolating from ", nrow(source), " source ",
                      "zones to ", nrow(target), " target zones.")

  # --------------------------------------------------------------------------
  # interpolate


  # remove target zones not in source
  select <- sf::st_intersects(target,
                              sf::st_geometry(sf::st_union(source)),
                              sparse=FALSE)
  target <- target[select,]


  # 2. compute basin weights

  output <- do_interpolation_ppdm(source,
                                target,
                                variable = variable,
                                dasy = dasymetric,
                                std_var = std_var,
                                n = n,
                                verbose = verbose)

  
  return(output)
}