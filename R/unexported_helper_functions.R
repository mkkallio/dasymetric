#helper function to compute weights for line segments
compute_segment_weights <- function(segments, variable) {
    weights <- rep(0, length(segments))
    n <- sum(variable[segments])
    weights[segments] <- variable[segments]/n
    return(weights)
}

compute_dasymetric_weights <- function(segments, arealen, variable) {
    weights <- rep(0, length(variable))
    arealenvar <- arealen*variable
    denom <- sum(arealen[segments]*variable[segments])
    weights[segments] <- arealenvar[segments]/denom
    return(weights)
} 



collect_listc <- function(ts, acc = FALSE) {
    
    Date <- NULL
    
    unit <- units::deparse_unit(dplyr::pull(ts[[1]],2)) 
    unidates <- lapply(ts, function(x) x$Date) %>%
        unlist %>%
        unique %>%
        lubridate::as_date()
    names <- colnames(ts[[1]])[-1]
    nts <- length(ts)
    #n <- ncol(ts[[1]])-1
    n <- lapply(ts, ncol) %>%
        unlist %>%
        max
    n <- n-1
    empty_ts <- matrix(NA, ncol = nts, nrow = length(unidates))
    
    output <- list()
    
    for (tsi in 1:n) {
        act_ts <- empty_ts
        for(i in 1:nts) {
            if(ncol(ts[[i]])-1 < tsi) break
            dates <- unidates %in% ts[[i]]$Date 
            act_ts[dates,i] <-unlist(ts[[i]][,tsi+1])
        }
        output[[ names[tsi] ]] <- units::as_units(act_ts, unit)     
    }
    
    if(acc) {
        for(tsi in seq_along(output)) {
            temp <- cbind(unidates, output[[tsi]]) %>%
                as.data.frame()
            colnames(temp) <- c("Date", names(ts))
            temp <- temp %>% 
                dplyr::mutate(Date = lubridate::as_date(Date)) %>%
                tibble::as_tibble() %>%
                tsibble::as_tsibble(index = "Date")
            output[[tsi]] <- temp
        }
    }
    
    names(output) <- names
    
    return(output)
}

spread_listc <- function(ts) {
    listc <- list()
    names <- names(ts)
    
    listc <- init_ts(ts)
    
    for(seg in seq_along(listc)) {
        for(tsi in seq_along(ts)) {
            listc[[seg]][,tsi+1] <- ts[[tsi]][seg+1]
        }
    }
    return(listc)
}


init_ts <- function(ts) {
    max_ts <- lapply(ts, nrow) %>%
        unlist
    max_ts <- which(max_ts == max(max_ts))
    dates <- ts[[ max_ts[1] ]]$Date
    
    output <- vector("list", ncol(ts[[1]])-1) 
    
    output <- lapply(output, function(x) {
        tsib <- matrix(NA, ncol=length(ts)+1, nrow=length(dates)) 
        colnames(tsib) <- c("Date", names(ts))
        tsib <- tibble::as_tibble(tsib)
        tsib$Date <- dates
        return(tsib)
    })
    names(output) <- colnames(ts[[1]])[-1]
    return(output)
}


assign_class <- function(obj, class) {
    
    original <- class(obj)
    
    HS <- which(original %in% class)
    
    if(HS != 1 || length(HS) == 0) {
        
        if (length(HS) != 0) newclass <- c(class, original[-HS])
        if (length(HS) == 0) newclass <- c(class, original)
        class(obj) <- newclass
        return(obj)
        
    } else {
        
        return(obj)
        
    }
}

# if object has "HS" attributes, edit those without NULL
# if object doesnt have "HS" attributes, initialize it with provided values
mod_HS_attributes <- function(HS, next_col = NA, prev_col = NA, 
                              col = NULL) {
    
    if(is.null(col)) {
        test <- is.null(base::attr(HS, "HS", exact = TRUE))
        if(test) {
            att <- c(next_col = next_col, 
                     prev_col = prev_col)
            base::attr(HS, "HS") <- att
        } else {
            att <- base::attr(HS, "HS", exact = TRUE)
            
            if(!is.na(next_col)) att["next_col"] <- next_col
            if(!is.na(prev_col)) att["prev_col"] <- prev_col
            
            base::attr(HS, "HS") <- att
        }
        return(HS)
    } else {
        test <- hasName(HS, col)
        if(!test) stop("Couldn't find column ", col, " in HS input.")
        
        ind <- which(colnames(HS) == col)
        test <- is.null(base::attr(HS[[ind]], "HS", exact = TRUE))
        if(test) {
            att <- c(next_col = next_col, 
                     prev_col = prev_col)
            base::attr(HS[[ind]], "HS") <- att
        } else {
            att <- base::attr(HS[[ind]], "HS", exact = TRUE)
            
            if(!is.na(next_col)) att["next_col"] <- next_col
            if(!is.na(prev_col)) att["prev_col"] <- prev_col
            
            base::attr(HS[[ind]], "HS") <- att
        }
        return(HS)
    }
    
    
}
# 
get_HS_attr <- function(HS) {
    return(base::attr(HS, "HS", exact = TRUE))
}

# goes through the columns in HS, and checks the value of "HS"
find_attribute <- function(HS, attribute, value) {
    test <- sapply(HS, function(x) {
        att <- attr(x, "HS") 
        test <- att[attribute] == value
        if(length(test) == 0) return(FALSE) else return(test)
    })
    return(which(test))
}



# convert unit 
convert_unit <- function(value, from = NULL, to1, to2, verbose = FALSE) {
    
    test <- inherits(value, "units")
    if(!test) {
        test <- is.null(from)
        if(test) stop("input is not of class 'units', and 'from' not specified")
        value <- units::set_units(value, from, mode="standard")
    }
    
    
    #try converting to first unit
    conv <- try(units::set_units(value, to1, mode="standard"),
                silent = TRUE)
    
    # if first conversion fails, try converting to alternative
    if(class(conv) == "try-error") {
        conv <- try(units::set_units(value, to2, mode="standard"),
                    silent = TRUE)
        
        # stop conversion to alternative didnt work
        if(class(conv) == "try-error") {
            stop(paste0("Couldn't convert units. Are you sure the unit's type is ",
                        "depth per time (e.g. mm/d) or volume per time ",
                        "(e.g. m^3/s)?"))
        }
        
        # return alternative
        if(verbose) message("Unit converted from ", from, " to ", to2)
        return(conv)
    }
    
    # return first option
    if(verbose) message("Unit converted from ", from, " to ", to1)
    return(conv)
}

unit_conversion <- function(obj, unit, areas = NULL) {
    units <- strsplit(unit, " ")[[1]]
    
    # check that all required elements are there and convertible
    depth <- any(units %in% c("mm", "cm", "m", "km"))
    area <- any(units %in% c("m-2", "km-2", "ha"))
    time <- any(units %in% c("s-1", "min-1", "h-1", "d-1", 
                             "week-1", "month-1"))
    volume <- any(units %in% c("m3", "km3"))
    
    if(volume & time) {
        obj <- convert_unit(obj, to1 = "m3/s")
    } else if(volume & time & area) {
        obj <- units::set_units(obj, "m3 m-2 s-1")
        areas <- units::set_units(areas, "m2")
        obj <- obj * areas
    } else if(depth & time & !area) {
        obj <- units::set_units(obj, "m s-1")
        areas <- units::set_units(areas, "m2")
        obj <- obj * areas
    } else if(depth & time & area) {
        obj <- units::set_units(obj, "m m-2 s-1")
        areas <- units::set_units(areas, "m2")
        obj <- obj * areas
        obj <- obj * as_units(1, "m2")
    } 
    return(obj)
}
