#' @export
do_interpolation_dm <- function(source, 
                                target,
                                variable,
                                dasy = NULL,
                                intensive = TRUE) {
    
    area <- NULL
    weights <- NULL
    ID <- NULL
    target_area <- NULL
    source_area <- NULL
    
    # TEST INPUTS
    #############
    
    test <- is.null(dasy)
    if(test) {
        stop("Dasymetric variable must be given.")
    } else {
        
        test <- hasName(target, dasy)
        if(test) {
            dasy_ts <- FALSE
            dasymetric_var <- dplyr::pull(target, dasy)
        } 
        
        if(!test) {
            test <- hasName(target$variable_ts[[1]], dasy)
            if(test) {
                dasy_ts <- TRUE
                dasymetric_var <- lapply(target$variable_ts, function(x) {
                    x <- select(x, Date, !!dasy)
                })
            } 
            if(!test) stop("No column ", dasy," found in target zones")
        }
        
        # test <- sum(is.null(target[,dasy]))
        # test2 <- sum(is.na(target[,dasy]))
        # if(test+test2 > 0) stop("Missing values in column ", dasy)
    }
    
    
    
    # --------------------------------------------------------------------------
    # PROCESS
    
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
                                    dplyr::mutate(source_area = units::set_units(source_area, "km^2")))
    
    
    if(dasy_ts) {
        
        # variable_ts <- lapply(target$variable_ts, function(x) {
        #     x <- select(x, Date, !!variable)
        # }) %>% 
        #     dasymetric:::spread_listc()
        # 
        # dasymetric_var <- dasymetric:::spread_listc(dasymetric_var)[[1]]
        # 
        # for(src in unique(weights$sourceID)) {
        #     wgt <- dplyr::filter(weights, 
        #                          sourceID == src)
        #     
        #     ind <- which(colnames(dasymetric_var) %in% wgt$targetID)
        #     
        #     dmvar <- dasymetric_var[,ind]
        #     dmvar <- sweep(dmvar, 2, wgt$target_area, "*")
        #     dmvar <- t(apply(dmvar, 1, function(x) {
        #         s <- sum(x)
        #         if(s == 0) {
        #             x <- wgt$target_area/sum(wgt$target_area)  
        #         }  else x <- x/sum(x)
        #         return(x)
        #     }))
        #     
        #     dasymetric_var[,ind] <- set_units(dmvar, 1)
        # }
        # 
        # for(var in seq_along(variable_ts)) {
        #     ts <- variable_ts[[var]][,-1]
        #     ts <- dasymetric:::unit_conversion(ts, "m3/s", areas = target$)
        # }

    } else {
        
        # compute weights
        weights <- weights %>%
            tibble::add_column(variable = dasymetric_var) %>%
            dplyr::group_by(sourceID) %>%
            dplyr::mutate(bas_dasy = variable*units::drop_units(target_area),
                          denom = sum(bas_dasy),
                          weights = bas_dasy/denom) %>%
            dplyr::ungroup() %>%
            dplyr::select(-variable, -bas_dasy, -denom)
        
        # do downscaling
        QTS <- dasymetric:::downscale_with_weights(source,
                                                   target,
                                                   weights, 
                                                   verbose = verbose)
        names(QTS) <- paste0(names(QTS), "_int")
        
        #process output
        listc <- dasymetric:::spread_listc(QTS)
        target$variable_ts <- lapply(seq_along(target$variable_ts), 
                                     function(i) {
                                         x <- dplyr::left_join(target$variable_ts[[i]],
                                                               QTS[[i]],
                                                               by="Date")
                                         return(x)
                                     })
        
    }
    
    # output <- reorder_cols(output)
    # output <- assign_class(output, "zones")
    return(target)
    
}






#' @export
do_interpolation_aw <- function(source, 
                                target,
                                intensive = TRUE) {
    
    area <- NULL
    weights <- NULL
    ID <- NULL
    target_area <- NULL
    source_area <- NULL
    
    # TEST INPUTS
    #############
    
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
           dplyr::mutate(source_area = units::set_units(source_area, "km^2"))) %>% 
           dplyr::mutate(weights, 
                         weights = target$target_area/target$source_area) %>% 
           dplyr::select(targetID, sourceID, weights,
                         target_area, source_area)
    
    QTS <- dasymetric:::downscale_with_weights(source,
                                               target,
                                               weights, 
                                               verbose = verbose)
    names(QTS) <- paste0(names(QTS), "_int")
    
    #process output
    listc <- dasymetric:::spread_listc(QTS)
    target$variable_ts <- lapply(seq_along(target$variable_ts), 
                                 function(i) {
                                     x <- dplyr::left_join(target$variable_ts[[i]],
                                                           QTS[[i]],
                                                           by="Date")
                                     return(x)
                                 })
    
    return(target)
}
