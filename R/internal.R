get_all_treatment_conditions <- function(treatments) {
  stopifnot(is.factor(treatments) || is.integer(treatments))
  if (is.factor(treatments)) {
    out_conditions <- levels(treatments)
  } else if (is.integer(treatments)) {
    out_conditions <- sort(unique(treatments))
  }
  out_conditions
}

get_treatment_indicators <- function(targets,
                                     treatments) {
  stopifnot(is.factor(treatments) || is.integer(treatments))
  if (is.factor(treatments)) {
    stopifnot(all(as.character(targets) %in% levels(treatments)))
    out_indicators <- rep(FALSE, nlevels(treatments))
    names(out_indicators) <- levels(treatments)
    out_indicators[as.character(targets)] <- TRUE
    out_indicators <- c(FALSE, out_indicators)
  } else if (is.integer(treatments)) {
    max_label <- max(treatments)
    stopifnot(all(as.integer(targets) %in% 0L:max_label))
    out_indicators <- rep(FALSE, max_label + 1L)
    names(out_indicators) <- as.character(0L:max_label)
    out_indicators[as.character(as.integer(targets))] <- TRUE
  }
  out_indicators
}
