
#' @export
create_model_frame <- function(zones, formula, by_id = FALSE) {
    
    # TODO: allow creating model frames by zone?
    
    # --------------------------------------------------------------------------
    # test input
    test <- inherits(zones, "zones")
    if(!test) stop("Requires input with class 'zones'")
    
    test <- hasName(zones, "sourceID")
    if(test) type <- "sourceID" else type <- "targetID"
    
    # --------------------------------------------------------------------------
    # process
    terms <- all.vars(formula)
    
    ### TODO: handle "." !
    
    
    # which of the variables are not timeseries
    nontsvar <- which(names(zones) %in% terms)
    tsvar <- which(names(zones$variable_ts[[1]]) %in% terms)
    
    # build the model frame
    frame <- lapply(1: nrow(zones), function(i) {
        x <- zones$variable_ts[[i]][,tsvar]
        y <-  sf::st_set_geometry(zones[i,nontsvar], NULL)
        
        for(ii in seq_along(y)) {
            name <- names(y)[ii]
            x <- tibble::add_column(x, !!name := dplyr::pull(y, ii))
        }
        
        return(x)
    })
    
    if(by_id) {
        names(frame) <- dplyr::pull(zones, type)
        
        # test that all the variables are included
        test <- all(terms %in% names(frame[[1]]))
        if(!test) stop("couldn't find all formula variables in 'zones'")
        
        # drop units because standard model engines dont like them
        
        frame <- lapply(frame, function(x) {
            x <- lapply(x, function(xx) {
                test <- inherits(xx, "units")
                if(test) xx <- units::drop_units(xx)
                return(xx)
            }) %>% as.data.frame()
        }) 
        
    } else {
        frame <- do.call(rbind, frame)
        
        # test that all the variables are included
        test <- all(terms %in% names(frame))
        if(!test) stop("couldn't find all formula variables in 'zones'")
        
        # drop units because standard model engines dont like them
        frame <- lapply(frame, function(x) {
            test <- inherits(x, "units")
            if(test) x <- units::drop_units(x)
            return(x)
            
        }) %>% as.data.frame()
    }
    
    return(frame)
}


#' modified lag function from {dynlm}
#' @export
L <- function(x, k = 1) {
    if (length(k) > 1) {
        rval <- lapply(k, function(i) lag(x, n = i))
        rval <- do.call(cbind, rval)
        colnames(rval) <- k
    }
    else {
        rval <- lag(x, k = -k)
    }
    return(rval)
}
