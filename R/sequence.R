#' Generate pseudorandom minimum distance sequence for factors
#' 
#' @export
prmd_factor <- function(n, levels) {
  if (is.numeric(levels) && (length(levels) == 1L)) {
    levels <- as.character(seq_len(levels))
  }
}
