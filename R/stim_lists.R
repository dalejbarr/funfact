#' Generate stimulus presentation lists
#'
#' Generates counterbalanced presentation lists for factorially
#' designed experiments involving stimulus presentation
#'
#' @param design_args A list describing the experimental design, which
#' must have an element \code{ivs}, giving a named list of independent
#' variables, with each list element a vector giving the levels of
#' that IV, or a single number stating the number of desired levels.
#' If any IVs are administered between-subjects or between-items,
#' these should be named in list elements \code{between_subj} and
#' \code{between_item} respectively.  The argument \code{design_args}
#' also can optionally include the following two elements:
#' \code{n_item}, the desired number of stimulus items, which if
#' unspecified, will result in lists with the minimum possible number
#' of items; and \code{n_rep}, the number of repetitions of each
#' stimulus item for each participant (default 1).
#' @param as_one boolean (default \code{TRUE}) specifying whether the
#' presentation lists are to be returned as a single data frame or as
#' elements in a list object
#'
#' @return a single \code{data.frame} (default) with each list
#' identified by \code{list_id} or a \code{list} of dataframes,
#' depending on the value of \code{as_one}
#'
#' @examples
#' stim_lists(list(ivs = c(A = 2, B = 2))) # 2x2 within-subjects within-item
#' 
#' stim_lists(list(ivs = c(A = 2, B = 2, n_item = 16))) # same but w/more items
#'
#' stim_lists(list(ivs = c(A = 2, B = 2, n_item = 16, n_rep = 3)))
#'
#' # mixed by subject, fully within by item
#' stim_lists(list(ivs = list(group = c("adult", "child"),
#'                            task = c("easy", "hard")),
#'                 between_subj = "group",
#'                 n_item = 12))
#'
#' # mixed by subject, mixed by item
#' stim_lists(list(ivs = c(A = 2, B = 2),
#'            between_subj = "A",
#'            between_item = "B"))
#' @export
stim_lists <- function(design_args, 
                       as_one = TRUE) {
    fac_combine_levels <- function(vars, iv_list, dframe = TRUE) {
        row_indices <- rev(do.call("expand.grid",
                                   lapply(rev(vars),
                                          function(x) seq_along(iv_list[[x]]))))
        res <- mapply(function(x, y) x[y], iv_list[vars], row_indices)
        if (dframe) {
            as.data.frame(res, stringsAsFactors = FALSE)
        } else {
            res
        }
    }

    rotate_cells <- function(x, combine = FALSE) {
        res <- lapply(seq_len(nrow(x)),
                      function(ix) x[c(ix:nrow(x), seq_len(ix - 1)), , drop = FALSE])
        if (combine) {
            do.call("rbind", res)
        } else {
            res
        }
    }

    bs_combine <- function(dat, plists) {
        ## factorially combine across lists for between subject variables
        if (nrow(dat) == 0) {
            return(plists)
        } else {}
        res <- c(lapply(seq_len(nrow(dat)), function(rx) {
            if (length(plists) > 0) {
                lapply(plists, function(lx) {
                    cbind(dat[rep(rx, nrow(lx)), , drop = FALSE], lx)
                })
            } else {
                dat[rx, , drop = FALSE]
            }
        }))
        if (length(plists) > 0) {
            do.call("c", res)
        } else {
            res
        }
    }

    check_design_args(design_args)
    iv_names <- names(design_args[["ivs"]])
    ## check whether elements of 'ivs' are numbers and convert to char vector
    ivs2 <- lapply(iv_names, function(nx) {
        x <- design_args[["ivs"]][[nx]]
        if ((length(x) == 1) && is.numeric(x)) {
            paste0(nx, seq_len(x))
        } else {x}
    })
    names(ivs2) <- iv_names

    item_within <- setdiff(names(ivs2), design_args[["between_item"]])
    subj_within <- setdiff(names(ivs2), design_args[["between_subj"]])
    ## iv_levels <- sapply(ivs2, length) # IS THIS NEEDED?

    ww_fac <- intersect(item_within, subj_within)

    ww_chunks <- rotate_cells(fac_combine_levels(intersect(item_within, subj_within),
                                                 ivs2))

    wb_chunks <- fac_combine_levels(intersect(subj_within, design_args[["between_item"]]), ivs2)

    ## combine the WSWI and WSBI chunks to create the base presentation lists
    if (nrow(wb_chunks) > 0) {
        if (length(ww_chunks) > 0) {
            base_plists <- lapply(ww_chunks, function(ww) {
                cbind(wb_chunks[rep(seq_len(nrow(wb_chunks)), each = nrow(ww)), , drop = FALSE], ww)
            })
        } else {
            base_plists <- list(wb_chunks)
        }
    } else {
        base_plists <- ww_chunks
    }

    ## handle BSWI
    bswi <- fac_combine_levels(intersect(design_args[["between_subj"]],
                                         item_within), ivs2)
    bswi_lists <- bs_combine(bswi, base_plists)

    ## handle bsbi factors (if they exist)
    bsbi <- fac_combine_levels(intersect(design_args[["between_subj"]],
                                         design_args[["between_item"]]), ivs2)
    bsbi_lists <- bs_combine(bsbi, bswi_lists)
    div_fac <- if (nrow(bsbi)) nrow(bsbi) else 1
    n_item <- design_args[["n_item"]]
    if (is.null(design_args[["n_item"]])) { # dynamically choose minimum n_item
        if (length(bswi_lists) > 0) {
            n_item <- nrow(bswi_lists[[1]]) * div_fac
        } else {
            n_item <- div_fac
        }
    } else {}
    if (length(bswi_lists) > 0) {
        item_fac <- div_fac * nrow(bswi_lists[[1]])
        if ((n_item %% item_fac) != 0) {
            stop("n_item must be a factor of ", item_fac)
        } else {}
    } else {}

    if (length(bsbi_lists) == 0) {
        bsbi_lists <- bswi_lists
    } else {}

    if ((n_item %% div_fac) != 0) stop("n_item must be a multiple of ", div_fac)

    rep_times <- if (length(bswi_lists) > 0) length(bswi_lists) else 1
    it_chunks <- rep(seq_len(div_fac), each = rep_times)
    it_lists <- split(seq_len(n_item),
                      rep(seq_len(div_fac), each = n_item / div_fac))[it_chunks]

    plists <- mapply(function(x, y) {
        ix <- rep(seq_len(nrow(y)), each = length(x) / nrow(y))
        cbind(item_id = x, y[ix, , drop = FALSE])
    },
                     it_lists, bsbi_lists, SIMPLIFY = FALSE)

    n_rep <- design_args[["n_rep"]]
    if (is.null(design_args[["n_rep"]])) n_rep <- 1

    if (n_rep > 1) {
        plists <- lapply(plists, function(x) {
                             data.frame(n_rep = as.character(paste0("r", rep(seq_len(n_rep), each = nrow(x)))),
                                        x[rep(seq_len(nrow(x)), n_rep), , drop = FALSE],
                                        check.names = FALSE, stringsAsFactors = FALSE)
                         })
    } else {}

    if (as_one) {
        res <- mapply(function(x, y) {
            cbind(list_id = x, y)
        },
                      seq_along(plists), plists, SIMPLIFY = FALSE)
        final_lists <- do.call("rbind", res)
    } else {
        final_lists <- plists
    }
    rownames(final_lists) <- NULL
    return(final_lists)
}

#' Generate trial lists from a stimulus presentation lists
#'
#' Merge stimulus presentation lists with subject data to create a
#' trial list.
#'
#' @param design_args Stimulus presentation lists (see \code{\link{stim_lists}}).
#' @param subjects One of the following three: (1) an integer
#' specifying the desired number of subjects (must be a multiple of
#' number of stimulus lists); (2) a data frame with assignment
#' information (must include a column \code{subj_id}); or (3)
#' \code{NULL}, in which case there will be one subject per list.
#' @param seq_assign If TRUE, assignment of subjects to lists will
#' be sequential rather than random (default is FALSE)
#'
#' @return A data frame containing all trial information.
#' @export
trial_lists <- function(design_args,
                        subjects = NULL, seq_assign = FALSE) {
    sp_lists <- stim_lists(design_args)
    sp2 <- split(sp_lists, sp_lists[["list_id"]])
    if (is.null(subjects)) {
        subjects <- length(sp2)
    } else {}
    if (is.numeric(subjects)) {
        if ((subjects %% length(sp2)) != 0) {
            stop("'subjects' must be a multiple of number of lists (",
                 length(sp2), ")")
        } else {}
        list_ord = rep(seq_along(sp2), subjects / length(sp2))
        if (!seq_assign) {
            list_ord = sample(list_ord) # randomize assignment to lists
        } else {}
        subj_dat <- data.frame(subj_id = seq_len(subjects),
                               list_id = list_ord)
    } else {
        if (!is.data.frame(subjects)) {
            stop("'subjects' must be an integer, data.frame, or NULL")
        } else {}
        subj_dat <- subjects
        if (any(!("subj_id" %in% names(subj_dat)),
                !("list_id" %in% names(subj_dat)))) {
            stop("'subjects' must contain fields 'subj_id', 'list_id'")
        } else {}
    }
    res <- lapply(seq_len(nrow(subj_dat)), function(rx) {
               cbind(subj_id = subj_dat[rx, "subj_id"],
                     sp2[[subj_dat[rx, "list_id"]]])
           })
    res2 <- do.call("rbind", res)
    rownames(res2) <- NULL
    return(res2)
}

#' Compose response data from fixed and random effects
#'
#' @param design_args List containing information about the experiment
#' design; see \code{\link{stim_lists}}
#' @param fixed vector of fixed effects
#' @param subj_rmx matrix of by-subject random effects
#' @param item_rmx matrix of by-item random effects
#' @param verbose give debugging info (default = \code{FALSE})
#'
#' @details This will add together all of the fixed and random effects
#' according to the linear model specified by the design.  Note,
#' however, that it does not add in any residual noise; for that, use
#' the function \code{\link{sim_norm}}.
#' 
#' @return a data frame containing response variable \code{Y}, the
#' linear sum of all fixed and random effects.
#' @export
compose_data <- function(design_args,
                         fixed = NULL,
                         subj_rmx = NULL,
                         item_rmx = NULL,
                         verbose = FALSE) {
    ## utility function for doing matrix multiplication
    multiply_mx <- function(des_mx, rfx, row_ix, design_args) {
        ## make sure all cols in rfx are represented in des_mx
        diff_cols <- setdiff(colnames(rfx), colnames(des_mx))
        if (length(diff_cols) != 0) {
            stop("column(s) '", paste(diff_cols, collapse = ", "),
                 "' not represented in terms '",
                 paste(term_names(design_args[["ivs"]],
                                  design_args[["between_subj"]],
                                  design_args[["between_item"]]),
                       collapse = ", "), "'")
        } else {}

        reduced_des <- des_mx[, colnames(rfx), drop = FALSE]

        t_rfx <- t(rfx)
        res_vec <- vector("numeric", length(row_ix))
        for (ix in unique(row_ix)) {
            lvec <- row_ix == ix
            res_vec[lvec] <- c(reduced_des[lvec, , drop = FALSE] %*%
                                   t_rfx[, ix, drop = FALSE])
        }
        res_vec
    }

    ivs_nrep <- design_args[["ivs"]]
    if (!is.null(design_args[["n_rep"]])) {
        if (design_args[["n_rep"]] > 1) {
            ivs_nrep <- c(as.list(design_args[["ivs"]]),
                          list(n_rep = paste0("r", seq_len(design_args[["n_rep"]]))))
        } else {}
    } else {}
    iv_names <- names(ivs_nrep)
    tlists <- trial_lists(design_args, subjects = nrow(subj_rmx))

    cont <- as.list(rep("contr.dev", length(ivs_nrep)))
    names(cont) <- iv_names

    mmx <- model.matrix(as.formula(paste0("~", paste(iv_names, collapse = "*"))),
                        tlists,
                        contrasts.arg = cont)

    if (is.null(fixed)) {
        fixed <- runif(ncol(mmx), -3, 3)
        names(fixed) <- colnames(mmx)
    } else {}

    ## fixed component of Y
    fix_y <- c(mmx %*% fixed) # fixed component of Y

    if (is.null(subj_rmx)) {
        stop("Autogeneration of subj_rmx not implemented yet; please define 'subj_rmx'")
    } else {}
    if (nrow(subj_rmx) != length(unique(tlists[["subj_id"]]))) {
        stop("Argument 'subj_rmx' has ", nrow(subj_rmx), " rows; needs ",
             length(unique(tlists[["subj_id"]])))
    } else {}
    sre <- multiply_mx(mmx, subj_rmx, tlists[["subj_id"]], design_args)

    if (is.null(item_rmx)) {
        stop("Autogeneration of item_rmx not implemented yet; please define 'item_rmx'")
    } else {}
    if (nrow(item_rmx) != length(unique(tlists[["item_id"]]))) {
        stop("Argument 'item_rmx' has ", nrow(item_rmx), " rows; needs ",
             length(unique(tlists[["item_id"]])))
    } else {}
    ire <- multiply_mx(mmx, item_rmx, tlists[["item_id"]], design_args)
    ## err <- rnorm(nrow(tlists), sd = sqrt(err_var))
    comb_mx <- matrix(nrow = nrow(tlists), ncol = 0)
    if (verbose) {
        ## comb_mx <- cbind(fix_y = fix_y, sre = sre, ire = ire, err = err)
        comb_mx <- cbind(fix_y = fix_y, sre = sre, ire = ire)
    } else {}
    ## cbind(tlists, Y = fix_y + sre + ire + err, comb_mx)
    cbind(tlists, Y = fix_y + sre + ire, comb_mx)
}
