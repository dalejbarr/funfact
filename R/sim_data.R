#' Generate population for data simulation
#'
#' @param design_args list specifying experimental design (see \code{\link{stim_lists}}); for best results, n_item should be 2*minimum number of items needed for the design
#' @param fixed_ranges list of 2-element vectors (min, max) defining ranges of fixed effect parameters
#' @param var_range 2-element vector defining range (min, max) of random effect variances
#' @param err_range 2-element vector defining range of error variance
#' @param n_subj number of subjects (for defining random effects structure; for best results, should be 2*number of stimulus lists
#' @return list with parameters for data generation
#' @seealso \code{\link{sim_norm}}
#' @examples
#'
#' dargs <- list(ivs = c(A = 2, B = 2), n_item = 8)
#' gen_pop(dargs, n_subj = 8)
#' @importFrom clusterGeneration genPositiveDefMat
#' @export
gen_pop <- function(design_args,
                    fixed_ranges = NULL,                    
                    var_range = c(0, 3),
                    err_range = c(1, 3),
                    n_subj = NULL) {
    tdat <- trial_lists(design_args, n_subj)
    forms <- design_formula(design_args, n_subj, lme4_format = FALSE)
    tnames <- lapply(forms, function(x) term_names(design_args, x))
    err_var <- runif(1, err_range[1], err_range[2])
    ## generate random effects
    rfx <- lapply(tnames[-1], function(x) {
        mx <- clusterGeneration::genPositiveDefMat(length(x),
                                                   covMethod = "onion",
                                                   rangeVar = var_range)$Sigma
        dimnames(mx) <- list(x, x)
        return(mx)
    })
    ## generate fixed effects
    if (!is.null(fixed_ranges)) {
        if (length(fixed_ranges) != length(tnames[["fixed"]])) {
            stop("fixed_ranges must have ", length(tnames[["fixed"]]),
                 " elements, corresponding to variables ",
                 paste(tnames[["fixed"]], collapse = ", "))
        } else {}
    } else {
        fixed_ranges <- lapply(seq_along(tnames[["fixed"]]), function(x) {c(0, 3)})
        names(fixed_ranges) <- tnames[["fixed"]]        
    }
    if (!is.null(names(fixed_ranges)) &&
            !identical(names(fixed_ranges), tnames[["fixed"]])) {
        warning("names of 'fixed_ranges' elements do not match variable names: ",
                paste(tnames[["fixed"]], collapse = ", "))
    } else {}
    return(list(fixed = sapply(fixed_ranges, function(x) runif(1, x[1], x[2])),
                subj_rfx = rfx[["subj_id"]],
                item_rfx = rfx[["item_id"]],
                err_var = err_var))
}

#' Sample data from population with normal error variance
#'
#' @param mcr.data list with parameters defining population
#' (\code{fixed}, \code{subj_rfx}, \code{item_rfx}).  Must also
#' contain element \code{design_args}, defining the experiment design,
#' and \code{design_args} must also contain the sub-element
#' \code{n_item} defining the number of stimulus items.  Finally,
#' \code{mcr.data} must contain element \code{n_subj}, defining the
#' number of subjects to be sampled.
#' @return A data frame 
#' @seealso \code{\link{gen_pop}}
#' @examples
#' design_args <- list(ivs = c(A = 2, B = 3), n_item = 18)
#' mcr.data <- gen_pop(design_args, n_subj = 12)
#' mcr.data[["n_subj"]] <- 12
#' mcr.data[["design_args"]] <- design_args
#' mcr.data[["err_var"]] <- 3
#' dat <- sim_norm(mcr.data)
#' @importFrom MASS mvrnorm
#' @export
sim_norm <- function(mcr.data) {
    required_elements <- c("n_subj", "design_args",
                           "fixed", "subj_rfx", "item_rfx", "err_var")
    missing_elements <- setdiff(required_elements, names(mcr.data))
    if (length(missing_elements) > 0) {
        stop("mcr.data was missing element(s): ",
             paste(missing_elements, collapse = ", "))
    } else {}
    design_args <- mcr.data[["design_args"]]
    if (is.null(design_args[["n_item"]])) {
        stop("'n_item' not specified in 'design_args'")
    } else {}
    rfx <- mapply(function(x, n) {
        MASS::mvrnorm(n, mu = rep(0, ncol(x)), x)
    }, mcr.data[c("subj_rfx", "item_rfx")],
           c(mcr.data[["n_subj"]], design_args[["n_item"]]), SIMPLIFY = FALSE)
    dat <- compose_data(design_args,
                 fixed = mcr.data[["fixed"]],
                 subj_rmx = rfx[["subj_rfx"]],
                 item_rmx = rfx[["item_rfx"]])
    dat[["Y"]] <- dat[["Y"]] + rnorm(nrow(dat), sd = sqrt(mcr.data[["err_var"]]))
    return(dat)
}
