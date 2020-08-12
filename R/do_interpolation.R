do_interpolation_dm <- function(source, 
                                target,
                                variable,
                                dasy,
                                std_var = NULL,
                                verbose = FALSE,
                                ...) {
  # ----------------------------------------------------------------------------
  # TEST INPUTS
  
  
  
  test <- is.formula(dasy)
  if(test) {
    target <- apply_model(source, target, dasy, 
                                       shift_negative = TRUE, verbose)#, ...)
    dasy <- "dasymetric_model"
  } 
  
  test <- is.null(dasy)
  if(test) {
    stop("Dasymetric variable must be given.")
  } else {
    
    test <- hasName(target, dasy)
    if(test) {
      dasy_ts <- FALSE
      ncol <- ncol(target$variable_ts[[1]])-1
      nrow <- nrow(target$variable_ts[[1]])
      
      dasymetric_var <- matrix(dplyr::pull(target, dasy), nrow = 1)
      dasymetric_var <- dasymetric_var[rep(seq_len(nrow(dasymetric_var)),
                                           each = nrow), ]
      colnames(dasymetric_var) <- target$targetID
      dasymetric_var <- cbind(dplyr::select(target$variable_ts[[1]],Date),
                              dasymetric_var)
      dasymetric_var <- dplyr::as_tibble(dasymetric_var)
      
    } 
    
    if(!test) {
      test <- hasName(target$variable_ts[[1]], dasy)
      if(test) {
        dasy_ts <- TRUE
        dasymetric_var <- lapply(target$variable_ts, function(x) {
          x <- select(x, Date, !!dasy)
        })
        names(dasymetric_var) <- target$targetID
        dasymetric_var <- dasymetric:::spread_listc(dasymetric_var)[[1]]
      } 
      if(!test) stop("No column ", dasy," found in target zones")
    }
  }
  
  
  
  
  
  
  # check standardising variable
  test <- is.null(std_var)
  if(!test) {
    test <- hasName(source, std_var)
    if(test) {
      std_ts <- FALSE
      std_source <- dplyr::pull(source, std_var)
    } else {
      test <- hasName(source$variable_ts[[1]], std_var)
      if(test) {
        std_ts <- TRUE
        std_source <- lapply(source$variable_ts, function(x) {
          x <- select(x, Date, !!std_var)
        })
        std_source <- dasymetric:::spread_listc(std_source)[[1]]
      } else {
        stop("No column ", std_var, " found in source zones.")
      }
    }
    
    test <- hasName(target, std_var)
    if(test) {
      std_ts <- FALSE
      std_target <- dplyr::pull(target, std_var)
    } else {
      test <- hasName(target$variable_ts[[1]], std_var)
      if(test) {
        std_ts <- TRUE
        std_target <- lapply(target$variable_ts, function(x) {
          x <- select(x, Date, !!std_var)
        })
        std_target <- dasymetric:::spread_listc(std_target)[[1]]
      } else {
        stop("No column ", std_var, " found in target zones.")
      }
    }
  } else {
    std_ts <- FALSE
    std_var <- TRUE
    std_source <- sf::st_area(source)
    names(std_source) <- source$sourceID
    std_target <- sf::st_area(target)
    names(std_target) <- target$targetID
  }
  
  # --------------------------------------------------------------------------
  # PROCESS
  
  variable_ts <- lapply(source$variable_ts, function(x) {
    x <- select(x, Date, !!variable)
  }) %>%
    dasymetric:::spread_listc()
  
  
  # intersection between source and target zones
  weights <- suppressWarnings(source %>% 
                                dplyr::select(sourceID) %>%
                                sf::st_intersection(dplyr::select(target, targetID), .) %>% 
                                sf::st_collection_extract("POLYGON") %>% 
                                dplyr::arrange(targetID) %>% 
                                dplyr::mutate(., target_area = sf::st_area(.),
                                              target_area = units::set_units(target_area, "km^2")) %>%
                                dplyr::filter(target_area != units::set_units(0, "m^2")) %>% 
                                dplyr::group_by(sourceID) %>% 
                                dplyr::mutate(source_area = sum(units::drop_units(target_area))) %>% 
                                dplyr::ungroup() %>% 
                                dplyr::mutate(source_area = units::set_units(source_area, "km^2")) %>% 
                                sf::st_set_geometry(NULL))
  weights <- split(weights, factor(weights$sourceID))
  
  
  
  ## COMPUTE DASYMETRIC WEIGHTS
  weightlist <- list()
  for(src in seq_along(weights)) {
    ind <- which(colnames(dasymetric_var) %in% weights[[src]]$targetID)
    
    if(std_ts) {
      dmvar <- dasymetric_var[,ind]
      std <- std_target[which(names(std_target) %in% weights[[src]]$targetID)]
      
      dmvar <- dmvar*std
      dmvar <- dmvar/rowSums(dmvar)
      weightlist[[ src ]] <- dmvar
      
    } else {
      std <- std_target[which(names(std_target) %in% weights[[src]]$targetID)]
      dmvar <- dasymetric_var[,ind]
      dmvar <- sweep(dmvar, 2, std, "*")
      dmvar <- dmvar/rowSums(dmvar)
      weightlist[[ src ]] <- dmvar
    }
    
  }
  
  
  # --------------------------------------------------------------------------
  ## APPLY INTERPOLATION
  output <- list()
  for(var in seq_along(variable_ts)) {
    ts <- variable_ts[[var]][,-1]
    unit <- units::deparse_unit(dplyr::pull(ts, 1))
    
    # apply standardising variable to source
    test <- is.null(std_var)
    if(!test) {
      if(std_ts) {
        ts <- ts * std_source
      } else {
        ts <- sweep(ts, 2, std_source, "*")
      }
    }
    
    
    # create output matrix
    out <- matrix(0, 
                  nrow = nrow(variable_ts[[1]]), 
                  ncol = nrow(target))
    colnames(out) <- target$targetID
    # out <- dasymetric_var 
    
    for(src in seq_along(weights)) {
      ind_trgt <- which(colnames(out) %in% 
                          weights[[src]]$targetID)
      ind_src <- which(colnames(ts) == names(weights)[src])
      
      w <- as.matrix(weightlist[[src]])
      ts_src <- units::drop_units(ts[,ind_src])
      vals <- w * ts_src
      
      out[,ind_trgt] <- out[, ind_trgt] + vals
    }
    
    # give output matrix units and convert to tibble
    out <- units::as_units(out, unit)
    out <- tibble::as_tibble(out) %>% 
      tibble::add_column(Date = variable_ts[[1]]$Date,
                         .before = 1)
    
    # apply standardising variable tom target
    test <- is.null(std_var)
    if(!test) {
      if(std_ts) {
        out[,-1] <- out[,-1] / std_target
      } else {
        out[,-1] <- sweep(out[,-1], 2, std_target, "/")
      }
    }
    output[[var]] <- out
  }
  names(output) <- paste0(names(variable_ts), "_int")
  
  #process output
  output <- dasymetric:::spread_listc(output)
  target$variable_ts <- lapply(seq_along(target$variable_ts), 
                               function(i) {
                                 x <- dplyr::left_join(target$variable_ts[[i]],
                                                       output[[i]],
                                                       by="Date")
                                 return(x)
                               })
  
  return(target)
  
}





## TODO: STD_VAR
do_interpolation_aw <- function(source, 
                                target,
                                variable,
                                std_var = NULL,
                                verbose = FALSE) {
  
  # ----------------------------------------------------------------------------
  # TEST INPUTS
  
  # check standardising variable
  test <- is.null(std_var)
  if(!test) {
    test <- hasName(source, std_var)
    if(test) {
      std_ts <- FALSE
      std_source <- dplyr::pull(source, std_var)
    } else {
      test <- hasName(source$variable_ts[[1]], std_var)
      if(test) {
        std_ts <- TRUE
        std_source <- lapply(source$variable_ts, function(x) {
          x <- select(x, Date, !!std_var)
        })
        std_source <- dasymetric:::spread_listc(std_source)[[1]]
      } else {
        stop("No column ", std_var, " found in source zones.")
      }
    }
    
    test <- hasName(target, std_var)
    if(test) {
      std_ts <- FALSE
      std_target <- dplyr::pull(target, std_var)
    } else {
      test <- hasName(target$variable_ts[[1]], std_var)
      if(test) {
        std_ts <- TRUE
        std_target <- lapply(target$variable_ts, function(x) {
          x <- select(x, Date, !!std_var)
        })
        std_target <- dasymetric:::spread_listc(std_target)[[1]]
      } else {
        stop("No column ", std_var, " found in target zones.")
      }
    }
  } else {
    std_ts <- FALSE
    std_var <- TRUE
    std_source <- sf::st_area(source)
    names(std_source) <- source$sourceID
    std_target <- sf::st_area(target)
    names(std_target) <- target$targetID
  }
    
  
  # ----------------------------------------------------------------------------
  # process
  
  variable_ts <- lapply(source$variable_ts, function(x) {
    x <- select(x, Date, !!variable)
  }) %>%
    dasymetric:::spread_listc()
  
  
  
  # intersection between source and target zones
  weights <- suppressWarnings(source %>% 
          dplyr::select(sourceID) %>%
          sf::st_intersection(dplyr::select(target, 
                                            targetID), .) %>% 
          sf::st_collection_extract("POLYGON") %>% 
          dplyr::arrange(targetID) %>% 
          dplyr::mutate(., target_area = sf::st_area(.),
                        target_area = units::set_units(target_area, "km^2")) %>%
          dplyr::filter(target_area != units::set_units(0, "m^2")) %>% 
          dplyr::group_by(sourceID) %>% 
          dplyr::mutate(source_area = sum(units::drop_units(target_area))) %>% 
          dplyr::ungroup() %>% 
          dplyr::mutate(source_area = units::set_units(source_area, "km^2"))) #%>%
  
  weights <- split(weights, factor(weights$sourceID))
  
  # create weight matrix
  ntstep <- nrow(variable_ts[[1]])
  w <- matrix(1, nrow = ntstep, ncol = nrow(target))
  colnames(w) <- target$targetID
  w <- units::as_units(w, 1)
  w <- tibble::as_tibble(w) %>% 
    tibble::add_column(Date = variable_ts[[1]]$Date, .before=1) 
                   
  
  
  ## COMPUTE WEIGHTS
  weightlist <- list()
  for(src in seq_along(weights)) {
    ind_target <- which(names(std_target) %in% weights[[src]]$targetID)
    ind_source <- which(names(std_source) %in% names(weights)[src])
    
    if(std_ts) {
      trgt_val <- std_target[,ind_target]
      src_val <- std_source[,ind_source]
      
      w_src <- lapply(trgt_val, function(x) x/unlist(src_val)) %>% 
        tibble::as_tibble()
      
      weightlist[[ src ]] <- w_src
        
    } else {
      
      trgt_val <- std_target[ind_target]
      src_val <- std_source[ind_source]
      
      w_src <-  trgt_val/src_val
      
      w_src <- sweep(w[,ind_target+1], 2, w_src, "*")
      
      weightlist[[ src ]] <- w_src
    }
  }
  
  
  
  # --------------------------------------------------------------------------
  ## APPLY INTERPOLATION
  
  output <- list()
  for(var in seq_along(variable_ts)) {
    ts <- variable_ts[[var]][,-1]
    unit <- units::deparse_unit(dplyr::pull(ts, 1))
    
    # if standardised!
    test <- is.null(std_var)
    if(!test) {
      if(std_ts) {
        ts <- ts * std_source
      } else {
        ts <- sweep(ts, 2, std_source, "*")
      }
    }
    
    out <- matrix(0, 
                  nrow = nrow(variable_ts[[1]]), 
                  ncol = nrow(target))
    colnames(out) <- target$targetID
    
    
    
    for(src in seq_along(weights)) {
      ind_trgt <- which(colnames(out) %in% 
                          weights[[src]]$targetID)
      ind_src <- which(colnames(ts) == names(weights)[src])
      
      w <- as.matrix(weightlist[[src]])
      ts_src <- units::drop_units(dplyr::pull(ts, ind_src) )
      vals <- w*ts_src
      
      
      out[,ind_trgt] <- out[, ind_trgt] + vals
      
    }
    
    out <- units::as_units(out, unit)
    
    out <- tibble::as_tibble(out) %>% 
      tibble::add_column(Date = variable_ts[[1]]$Date,
                         .before = 1)
    
    test <- is.null(std_var)
    if(!test) {
      if(std_ts) {
        out[,-1] <- out[,-1] / std_target
      } else {
        out[,-1] <- sweep(out[,-1], 2, std_target, "/")
      }
    }
    output[[var]] <- out
  }
  names(output) <- paste0(names(variable_ts), "_int")

 
  #process output
  output <- dasymetric:::spread_listc(output)
  target$variable_ts <- lapply(seq_along(target$variable_ts), 
                               function(i) {
                                 x <- dplyr::left_join(target$variable_ts[[i]],
                                                       output[[i]],
                                                       by="Date")
                                 return(x)
                               })
  
  return(target)
}



do_interpolation_ppdm <- function(source,
                                  target,
                                  variable,
                                  dasy,
                                  std_var = NULL,
                                  n,
                                  verbose = FALSE) {
  
  # ----------------------------------------------------------------------------
  # test inputs
  
  # check that variable can be found from source zones
  test <- is.null(variable)
  if(test) {
    stop("Variable must be given.")
  } else {
    
    test <- hasName(source, variable)
    if(test) {
      pycno_ts <- FALSE
      
      pycno_var <- dplyr::pull(source, variable)
      names(pycno_var) <- source$sourceID
      
      test <- inherits(pycno_var, "units")
      if(test) {
        unit <- units::deparse_unit(pycno_var)
      } else {
        unit <- 1
      }
    } 
    
    if(!test) {
      test <- hasName(source$variable_ts[[1]], variable)
      if(test) {
        pycno_ts <- TRUE
        pycno_var <- lapply(source$variable_ts, function(x) {
          x <- select(x, Date, !!variable)
        })
        names(pycno_var) <- source$sourceID
        pycno_var <- dasymetric:::spread_listc(pycno_var)[[1]]
        
        test <- inherits(dplyr::pull(pycno_var, 2), "units")
        if(test) {
          unit <- units::deparse_unit(dplyr::pull(pycno_var,2))
        } else {
          unit <- 1
        }
      } 
      if(!test) stop("No column ", variable," found in source zones")
    }
  }
  
  
  # check dasymetric variable
  test <- is.null(dasy)
  if(!test) {
    
    test <- hasName(target, dasy)
    if(test) {
      dasy_ts <- FALSE
      ncol <- ncol(target$variable_ts[[1]])-1
      nrow <- nrow(target$variable_ts[[1]])
      
      dasymetric_var <- matrix(dplyr::pull(target, dasy), nrow = 1)
      dasymetric_var <- dasymetric_var[rep(seq_len(nrow(dasymetric_var)),
                                           each = nrow), ]
      colnames(dasymetric_var) <- target$targetID
      dasymetric_var <- cbind(dplyr::select(target$variable_ts[[1]],Date),
                              dasymetric_var)
      dasymetric_var <- dplyr::as_tibble(dasymetric_var)
      
    } 
    
    if(!test) {
      test <- hasName(target$variable_ts[[1]], dasy)
      if(test) {
        dasy_ts <- TRUE
        dasymetric_var <- lapply(target$variable_ts, function(x) {
          x <- select(x, Date, !!dasy)
        })
        names(dasymetric_var) <- target$targetID
        dasymetric_var <- dasymetric:::spread_listc(dasymetric_var)[[1]]
      } 
      if(!test) stop("No column ", dasy," found in target zones")
    }
    
    # test <- sum(is.null(target[,dasy]))
    # test2 <- sum(is.na(target[,dasy]))
    # if(test+test2 > 0) stop("Missing values in column ", dasy)
  }
  
  
  ## TODO: check that both std_source and std_target are ts or not
  
  # check standardising variable
  test <- is.null(std_var)
  if(!test) {
    test <- hasName(source, std_var)
    if(test) {
      std_ts <- FALSE
      std_source <- dplyr::pull(source, std_var)
    } else {
      test <- hasName(source$variable_ts[[1]], std_var)
      if(test) {
        std_ts <- TRUE
        std_source <- lapply(source$variable_ts, function(x) {
          x <- select(x, Date, !!std_var)
        })
        std_source <- dasymetric:::spread_listc(std_source)[[1]]
      } else {
        stop("No column ", std_var, " found in source zones.")
      }
    }
    
    test <- hasName(target, std_var)
    if(test) {
      std_ts <- FALSE
      std_target <- dplyr::pull(target, std_var)
    } else {
      test <- hasName(target$variable_ts[[1]], std_var)
      if(test) {
        std_ts <- TRUE
        std_target <- lapply(target$variable_ts, function(x) {
          x <- select(x, Date, !!std_var)
        })
        std_target <- dasymetric:::spread_listc(std_target)[[1]]
      } else {
        stop("No column ", std_var, " found in target zones.")
      }
    }
  } else {
    std_ts <- FALSE
    std_var <- TRUE
    std_source <- sf::st_area(source)
    std_target <- sf::st_area(target)
  }
  
  ### n timestep
  
  # ----------------------------------------------------------------------------
  # PROCESS
  
  # intersection between target and source zones
  weights <- suppressWarnings(source %>% 
                                dplyr::select(sourceID) %>%
                                sf::st_intersection(dplyr::select(target, targetID), .) %>% 
                                sf::st_collection_extract("POLYGON") %>% 
                                dplyr::arrange(targetID) %>% 
                                dplyr::mutate(., target_area = sf::st_area(.),
                                              target_area = units::set_units(target_area, "km^2")) %>%
                                dplyr::filter(target_area != units::set_units(0, "m^2")) %>% 
                                dplyr::group_by(sourceID) %>% 
                                dplyr::mutate(source_area = sum(units::drop_units(target_area))) %>% 
                                dplyr::ungroup() %>% 
                                dplyr::mutate(source_area = units::set_units(source_area, "km^2"))) 
  
  weights <- weights %>% 
    tibble::add_column(outID = NA, orig_id = NA) %>%
    dplyr::select(outID, orig_id, targetID, 
                  dplyr::everything()) 
  
  test <- hasName(weights, "geom")
  if(test) weights <- dplyr::rename(weights, geometry = geom)
  
  
  #identify neighbours
  touching <- sf::st_touches(weights)
  
  #identify boundary
  boundary <- sf::st_touches(weights, 
                             sf::st_union(weights) %>%
                               sf::st_cast("POLYGON") %>%
                               sf::st_cast("LINESTRING"),
                             sparse = FALSE) %>%
    as.numeric()
  boundary[boundary == 0] <- NA
  
  # ----------------------------------------------------------------------------
  # prepare for pycnophylactic interpolation
  nstep <- nrow(source$variable_ts[[1]])
  ntarget <- nrow(target)
  nelement <- which(!is.na(weights$targetID))
  unidates <- dplyr::pull(source$variable_ts[[1]], Date) 
  
  if(verbose) message("Performing pycnophylactic interpolation..")
  
  
  # ----------------------------------------------------------------------------
  # do pycno
  if(pycno_ts) {  # if pycnophylactic variable is a timeseries
    
    if(verbose) pb <- txtProgressBar(0, nstep, style = 3)
    
    out <- matrix(NA, nrow = nstep, ncol = ntarget)
    colnames(out) <- target$targetID
    
    # do pp for each timestep
    for(tstep in 1:nstep) {
      
      r_source <- sapply(pycno_var[tstep, -1], units::drop_units)
      
      if(is.null(dasy)) {
        r_pycno <- dasymetric:::iterate_pycno2(weights$sourceID, 
                                               weights$targetID,
                                               std_source,
                                               std_target,
                                               dasy = NULL, 
                                               touching, 
                                               boundary,
                                               r_source, 
                                               n) 
        
      } else {
        r_dasy <- unlist(dasymetric_var[tstep, -1])
        r_pycno <- iterate_pycno2(weights$sourceID, 
                                               weights$targetID,
                                               std_source,
                                               std_target,
                                               dasy = r_dasy, 
                                               touching, 
                                               boundary,
                                               r_source, 
                                               n) 
      }
      
      
      for(i in seq_along(r_pycno)) {
        where <- which(target$targetID == weights$targetID[i])
        if(length(where) == 0) next
        out[tstep, where] <- sum(out[tstep, where], 
                                 r_pycno[i],
                                 na.rm=TRUE) 
      }
      if (verbose) setTxtProgressBar(pb, tstep)
      
    }
    close(pb)
    
    
    
    
    
  } else { # if pycnophylactic weights are static
    
    out <- matrix(NA, nrow = nstep, ncol = ntarget)
    colnames(out) <- target$targetID
    
    r_source <- units::drop_units(pycno_var)
    
    
    r_pycno <- dasymetric:::iterate_pycno2(weights$sourceID, 
                              weights$targetID,
                              std_source,
                              std_target,
                              dasy = NULL, 
                              touching, 
                              boundary,
                              r_source, 
                              n)
    
    
    for(i in seq_along(r_pycno)) {
      where <- which(target$targetID == weights$targetID[i])
      if(length(where) == 0) next
      out[tstep, where] <- sum(out[tstep, where], 
                               r_pycno[i],
                               na.rm=TRUE) 
    }
    
  }
  
  # --------------------------------------------------------------------------
  # prepare output
  out <- units::as_units(out, unit)
  out <- data.frame(out)
  colnames(out) <- target$targetID
  out <- tibble::add_column(dplyr::as_tibble(out), Date = unidates, .before=1)
  
  out <- list(out)
  names(out) <- variable
  
  names(out) <- paste0(names(out), "_int")
  
  #process output
  out <- dasymetric:::spread_listc(out)
  target$variable_ts <- lapply(seq_along(target$variable_ts), 
                               function(i) {
                                 x <- dplyr::left_join(target$variable_ts[[i]],
                                                       out[[i]],
                                                       by="Date")
                                 return(x)
                               })
  
  return(target)
  
}






iterate_pycno2 <- function(sourceID,
                           targetID,
                           std_source,
                           std_target,
                           dasy = NULL, 
                           touching, 
                           boundary, 
                           r_source, 
                           n) {
  
  # remove units
  std_source <- as.numeric(std_source)
  std_target <- as.numeric(std_target)
  
  
  # prepare
  r_orig <- rep(NA, length(touching))
  for(i in seq_along(r_source)) {
    ind <- which(sourceID == as.numeric(names(r_source)[i]))
    r_orig[ind] <- r_source[[i]]
  }
  r_prev <- r_orig
  r_curr <- r_orig
  
  iter <- which(!is.na(targetID))
  remove <- which(is.na(targetID))
  sourceID[remove] <- NA
  
  # iterate
  for (i in 1:n) {
    
    for(j in iter) {
      ind <- c(j, touching[[j]])
      boundary_val <- r_orig[j] * boundary[j]
      vals <- c(r_prev[ind], boundary_val)
      new_value <- mean(vals, na.rm=TRUE)
      r_curr[j] <- new_value
    }
    
    # # rescale
    for(j in seq_along(r_source)) {
      ind <- which(sourceID == j)
      vol_c <- r_curr[ind] * std_target[ind]
      vol_g <- r_source[[j]] * std_source[j]
      bias <- vol_g / sum(vol_c,na.rm = TRUE)
      vol_c <- vol_c * bias
      
      r_curr[ind] <- vol_c / std_target[ind]
    }
    
    r_prev <- r_curr    
  }
  
  # if dasymetric variable is provided
  test <- is.null(dasy)
  if(!test) {
    r_curr <- r_curr * dasy
    
    # same as above
    for(j in seq_along(r_source)) {
      ind <- which(sourceID == j)
      vol_c <- r_curr[ind] * std_target[ind]
      vol_g <- r_source[j]*std_source[j]
      bias <- vol_g / sum(vol_c,na.rm = TRUE)
      vol_c <- vol_c * bias
      
      r_curr[ind] <- vol_c / std_target[ind]
    }
  }
  
  return(r_curr)
} 
