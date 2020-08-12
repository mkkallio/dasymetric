
#' @export
apply_model <- function(source, target, formula, 
                        shift_negative = FALSE, 
                        verbose = FALSE, ...) {
    
    
    # --------------------------------------------------------------------------
    # test
    test <- hasArg("engine")
    if(!test) {
        engine <- lm
    }
    
    if(verbose) message("Training given dasymetric model using ", 
                        "source zones, and evaluating at target zones.")
    
    
    # get unit of dependent variable
    varname <- all.vars(formula)[1]
    
    test <- hasName(source, varname)
    if(test) {
        unit <- units::deparse_unit(dplyr::pull(source, varname))
    } else {
        test <- hasName(source$variable_ts[[1]], varname)
        if(test) {
            unit <- units::deparse_unit(dplyr::pull(source$variable_ts[[1]], 
                                                    varname))
        } else {
            stop("Couldn't find dependent variable in source")
        }
    }
    
    # ----------------------------------------------------------------------
    # train model with source
    data <- create_model_frame(source, formula, by_id = FALSE)
    
    
    model <- engine(formula, data)#, ...)
    
    # ----------------------------------------------------------------------
    # apply model to target
    data <- create_model_frame(target, formula, by_id = TRUE)
    
    v_ts <- lapply(seq_along(data), function(i) {
        dat <- data[[i]]
        vts <- target$variable_ts[[i]]
        pr <- predict(model, newdata = dat)
        
        if(shift_negative) {
            minpr <- min(pr, na.rm=TRUE)
            
            test <- minpr < 0
            if(test) {
                pr <- pr + -minpr
            } 
        }
        
        pr[is.na(pr)] <- 0
        
        pr <- tibble(Date = vts$Date,
                     dasymetric_model = units::as_units(pr, unit))
        
        vts <- dplyr::left_join(vts, pr, by="Date")
        
        return(vts)
    })
    
    target$variable_ts <- v_ts
    
    return(target)
}
