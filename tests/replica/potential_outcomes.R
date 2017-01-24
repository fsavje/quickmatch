replica_potential_outcomes <- function(outcomes,
                                       treatments,
                                       matching,
                                       targets = NULL,
                                       subset = NULL) {
  coerce_double(outcomes)
  num_observations <- length(outcomes)
  treatments <- Rscclust:::coerce_type_labels(treatments, num_observations)
  all_treatment_conditions <- get_all_treatment_conditions(treatments)
  Rscclust:::ensure_Rscc_clustering(matching, num_observations)

  if (is.null(targets)) {
    targets <- all_treatment_conditions
  }
  ensure_treatment_label_indicators(targets, all_treatment_conditions)

  if (is.character(subset)) {
    ensure_treatment_label_indicators(subset, all_treatment_conditions)
    subset <- Rscclust:::make_type_indicators(subset, treatments)
    subset <- translate_targets(subset, treatments)
  }
  subset <- Rscclust:::coerce_data_point_indices(subset, num_observations)
  if (is.null(subset)) {
    subset <- rep(TRUE, length(outcomes))
  } else if (is.integer(subset)) {
    subset_tmp <- rep(FALSE, num_observations)
    subset_tmp[subset] <- TRUE
    subset <- subset_tmp
  }

  targets <- Rscclust:::make_type_indicators(targets, treatments)

  treatment_mean <- aggregate(list(outcomes = outcomes),
                              list(
                                "treatments" = as.integer(treatments),
                                "matching" = as.integer(matching)
                              ),
                              mean)
  weight_count <- aggregate(list(weight = subset),
                            list("matching" = as.integer(matching)),
                            sum)

  ave_pot_outcomes <- as.numeric(rep(NA, length(targets)))
  for (t in which(targets)) {
    tmp <- treatment_mean[treatment_mean$treatments == (t - 1), c("matching", "outcomes")]
    if (all(weight_count$matching[weight_count$weight > 0] %in% tmp$matching)) {
      tmp <- merge(weight_count, tmp)
      ave_pot_outcomes[t] <- sum(tmp$weight * tmp$outcomes) / sum(tmp$weight)
    }
  }

  ave_pot_outcomes <- ave_pot_outcomes[targets]
  names(ave_pot_outcomes) <- names(targets)[targets]
  ave_pot_outcomes
}
