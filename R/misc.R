#' Deviation-Coded Contrast Matrices
#'
#' Return a matrix of deviation-coded contrasts.
#'
#' @param n a vector of levels for a factor, or the number of levels.
#' @param base an integer specifying which group is considered the
#' baseline group. Ignored if 'contrasts' is \code{FALSE}.
#' @param contrasts a logical indicating whether contrasts should be computed.
#'
#' @export 
contr.dev <- function(n, base = 1, contrasts = TRUE) {
    if (length(n) <= 1L) {
        if (is.numeric(n) && (length(n) == 1L) && (n > 1L))
            levels <- seq_len(n)
        else stop("not enough degrees of freedom to define contrasts")
    } else {
        levels <- n
    }
    #mx <- apply(contr.treatment(n), 2, function(x) {x-mean(x)})
    ctreat <- contr.treatment(levels, base, contrasts)
    mx <- apply(ctreat, 2, scale, scale = FALSE)
    dimnames(mx) <- dimnames(ctreat)
    mx    
}

#' Calculate marginal and cell counts for factorially-designed data
#'
#' @param iv_names Names of independent variables in data.frame given by \code{dat}.
#' @param dat A data frame
#' @param unit_names Names of the fields containing sampling units (subjects, items)
#' @return a list, the elements of which have the marginal/cell counts for each factor in the design
#' @export
fac_counts <- function(iv_names, dat, unit_names = c("subj_id", "item_id")) {    
    fac_info <- attr(terms(as.formula(paste0("~", paste(iv_names, collapse = "*")))), "factors")
    rfx <- sapply(unit_names, function(this_unit) {
        ## figure out how many things are replicated by unit, how many times
        rep_mx <- xtabs(paste0("~", paste(iv_names, collapse = "+"), "+", this_unit), dat)
        lvec <- lapply(colnames(fac_info), function(cx) {
            x <- fac_info[, cx, drop = FALSE]
            ix <- seq_along(x)[as.logical(x)]
            ## create margin table
            marg_mx <- apply(rep_mx, c(ix, length(dim(rep_mx))), sum)
            mmx <- apply(marg_mx, length(dim(marg_mx)), c)
        })
        names(lvec) <- colnames(fac_info)
        return(lvec)
        ## lvec <- apply(fac_info, 2, function(x) {
        ##     ix <- seq_along(x)[as.logical(x)]
        ##     ## create margin table
        ##     marg_mx <- apply(rep_mx, c(ix, length(dim(rep_mx))), sum)
        ##     mmx <- apply(marg_mx, length(dim(marg_mx)), c)
        ## })
    }, simplify = FALSE)
    return(rfx)
}

#' Generate numerical deviation-coded predictors
#'
#' Add deviation-coded predictors to data frame.
#'
#' @param dat A data frame with columns containing factors to be converted.
#' @param iv_names Names of the variables to be converted.
#'
#' @return A data frame including additional deviation coded predictors.
#'
#' @examples
#' with_dev_pred(stim_lists(list(ivs = c(A = 3))), "A")
#' @export
with_dev_pred <- function(dat, iv_names = NULL) {
    if (is.null(iv_names)) {
        iv_names <- names(dat)
    } else {}
    mform <- as.formula(paste0("~", paste(iv_names, collapse = "+")))
    cont <- as.list(rep("contr.dev", length(iv_names)))
    names(cont) <- iv_names
    cbind(dat, model.matrix(mform, dat, contrasts.arg = cont)[, -1])
}

check_design_args <- function(design_args) {
    ## TODO check integrity of design args
    required_elements <- c("ivs")
    missing_elements <- setdiff(required_elements, names(design_args))
    if (length(missing_elements) > 0) {
        stop("'design_args' missing element(s): ",
             paste(missing_elements, collapse = ", "))
    } else {}
    return(TRUE)
}

#' Get names for predictors in factorial design
#'
#' Get the names for the numerical predictors corresponding to all
#' main effects and interactions of categorical IVs in a factorial
#' design.
#'
#' @param design_args A list with experimental design information (see \code{link{stim_lists}})
#' @param design_formula A formula (default NULL, constructs from \code{design_args})
#' @param contr_type Name of formula for generating contrasts (default "contr.dev")
#' @return A character vector with names of all the terms
#' @export 
term_names <- function(design_args,
                       design_formula = NULL,
                       contr_type = "contr.dev") {
    check_design_args(design_args)
    plists <- stim_lists(design_args)
    cont <- as.list(rep(contr_type, length(design_args[["ivs"]])))
    names(cont) <- names(design_args[["ivs"]])
    if (is.null(design_formula)) design_formula <- as.formula(paste0("~",
                                                               paste(names(design_args[["ivs"]]),
                                                                     collapse = " * ")))
    suppressWarnings(mmx <- model.matrix(design_formula, plists, contrasts.arg = cont))
    return(colnames(mmx))
}

#' Get the GLM formula for a factorially-designed experiment
#'
#' Get the formula corresponding to the general linear model for a
#' factorially designed experiment, with maximal random effects.
#' 
#' @param design_args A list with experimental design information (see \code{link{stim_lists}})
#' @param n_subj Number of subjects
#' @param dv_name Name of dependent variable; \code{NULL} (default) for one-sided formula
#' @param lme4_format Do you want the results combined as the model formula for a \code{lme4} model? (default \code{TRUE})
#' @return A formula, character string, or list, depending
#' @export
design_formula <- function(design_args,
                           n_subj = NULL,
                           dv_name = NULL,
                           lme4_format = TRUE) {
    iv_names <- names(design_args[["ivs"]])

    fixed <- paste(iv_names, collapse = " * ")

    fac_cnts <- fac_counts(iv_names, trial_lists(design_args, subjects = n_subj))
    fac_info <- attr(terms(as.formula(paste0("~", paste(iv_names, collapse = "*")))), "factors")

    rfx <- lapply(fac_cnts, function(lx) {
        lvec <- sapply(lx, function(mx) {
            as.logical(prod(apply(mx, 2, function(xx) all(xx > 1))))
        })
        res <- fac_info[, lvec, drop = FALSE]        
        keep_term <- rep(TRUE, ncol(res))
        ## try to simplify the formula
        for (cx in rev(seq_len(ncol(res))[-1])) {
            drop_term <- sapply(seq_len(cx - 1), function(ccx) {
                identical(as.logical(res[, ccx, drop = FALSE]) | as.logical(res[, cx, drop = FALSE]),
                          as.logical(res[, cx, drop = FALSE]))
            })
            keep_term[seq_len(cx - 1)] <- keep_term[seq_len(cx - 1)] & (!drop_term)
        }
        fterms <- apply(res[, keep_term, drop = FALSE], 2, function(llx) {
            paste(names(llx)[as.logical(llx)], collapse = " * ")
        })
        need_int <- any(apply(lx[[1]], 2, sum) > 1)
        fterms2 <- if (need_int) c("1", fterms) else fterms
        paste(fterms2, collapse = " + ")
    })

    form_list <- c(list(fixed = fixed), rfx)

    if (lme4_format) {
        form_str <- paste0(dv_name, " ~ ", form_list[["fixed"]], " + ",
               paste(sapply(names(form_list[-1]),
                            function(nx) paste0("(", rfx[[nx]], " | ", nx, ")")),
                     collapse = " + "))
        result <- as.formula(form_str)
    } else {
        result <- lapply(form_list, function(x) formula(paste0("~", x)))
    }

    return(result)
}
