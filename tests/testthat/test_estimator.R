library(quickmatch)
suppressPackageStartupMessages(library(Rscclust))
context("estimator.R")

source("../replica/estimator.R", local = TRUE)

sound_outcomes <- 1:10 / 2
unsound_outcomes <- factor(1:10)
sound_matching <- Rscc_clustering(c(rep(0, 5), rep(1, 5)))
unsound_matching <- factor(1:10)
sound_treatments <- factor(rep(1:2, 5))
unsound_treatments <- "unsound"
sound_estimands <- NULL
unsound_estimands <- "T"
sound_subset <- NULL
unsound_subset <- "T"

test_that("potential_outcomes checks input.", {
  expect_type(potential_outcomes(sound_outcomes,
                                 sound_treatments,
                                 sound_matching,
                                 sound_estimands,
                                 sound_subset), "double")
  expect_error(potential_outcomes(unsound_outcomes,
                                  sound_treatments,
                                  sound_matching,
                                  sound_estimands,
                                  sound_subset))
  expect_error(potential_outcomes(sound_outcomes,
                                  unsound_treatments,
                                  sound_matching,
                                  sound_estimands,
                                  sound_subset))
  expect_error(potential_outcomes(sound_outcomes,
                                  sound_treatments,
                                  unsound_matching,
                                  sound_estimands,
                                  sound_subset))
  expect_error(potential_outcomes(sound_outcomes,
                                  sound_treatments,
                                  sound_matching,
                                  unsound_estimands,
                                  sound_subset))
  expect_error(potential_outcomes(sound_outcomes,
                                  sound_treatments,
                                  sound_matching,
                                  sound_estimands,
                                  unsound_subset))
})

test_outcome <- c(5, 3, 4, 2, 5, 7, 9, 4, 2, 2)
test_treatment1 <- factor(rep(1:2, 5))
test_treatment2 <- rep(1:2, 5)
test_matching1 <- Rscc_clustering(c(rep(0, 5), rep(1, 5)))
test_matching2 <- Rscc_clustering(c(1, 1, 2, 2, 3, 3, 3, 4, 4, 4))
test_subset <- c(TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE)

ref1 <- c(
  "1" = (
    sum(test_matching1 == 0) * mean(test_outcome[test_matching1 == 0 & test_treatment1 == 1]) +
      sum(test_matching1 == 1) * mean(test_outcome[test_matching1 == 1 & test_treatment1 == 1])
  ) / 10,
  "2" = (
    sum(test_matching1 == 0) * mean(test_outcome[test_matching1 == 0 & test_treatment1 == 2]) +
      sum(test_matching1 == 1) * mean(test_outcome[test_matching1 == 1 & test_treatment1 == 2])
  ) / 10
)

ref1a <- c(
  "1" = (
    sum(test_matching1 == 0) * mean(test_outcome[test_matching1 == 0 & test_treatment1 == 1]) +
      sum(test_matching1 == 1) * mean(test_outcome[test_matching1 == 1 & test_treatment1 == 1])
  ) / 10
)

ref2 <- c(
  "1" = (
    sum(test_matching2 == 0) * mean(test_outcome[test_matching2 == 0 & test_treatment1 == 1]) +
      sum(test_matching2 == 1) * mean(test_outcome[test_matching2 == 1 & test_treatment1 == 1]) +
      sum(test_matching2 == 2) * mean(test_outcome[test_matching2 == 2 & test_treatment1 == 1]) +
      sum(test_matching2 == 3) * mean(test_outcome[test_matching2 == 3 & test_treatment1 == 1])
  ) / 10,
  "2" = (
    sum(test_matching2 == 0) * mean(test_outcome[test_matching2 == 0 & test_treatment1 == 2]) +
      sum(test_matching2 == 1) * mean(test_outcome[test_matching2 == 1 & test_treatment1 == 2]) +
      sum(test_matching2 == 2) * mean(test_outcome[test_matching2 == 2 & test_treatment1 == 2]) +
      sum(test_matching2 == 3) * mean(test_outcome[test_matching2 == 3 & test_treatment1 == 2])
  ) / 10
)

ref2a <- c(
  "1" = (
    sum(test_matching2 == 0) * mean(test_outcome[test_matching2 == 0 & test_treatment1 == 1]) +
      sum(test_matching2 == 1) * mean(test_outcome[test_matching2 == 1 & test_treatment1 == 1]) +
      sum(test_matching2 == 2) * mean(test_outcome[test_matching2 == 2 & test_treatment1 == 1]) +
      sum(test_matching2 == 3) * mean(test_outcome[test_matching2 == 3 & test_treatment1 == 1])
  ) / 10
)

ref3 <- c(
  "1" = (
    sum(test_matching1 == 0 & test_treatment1 == 1) *
      mean(test_outcome[test_matching1 == 0 & test_treatment1 == 1]) +
      sum(test_matching1 == 1 & test_treatment1 == 1) *
      mean(test_outcome[test_matching1 == 1 & test_treatment1 == 1])
  ) / sum(test_treatment1 == 1),
  "2" = (
    sum(test_matching1 == 0 & test_treatment1 == 1) *
      mean(test_outcome[test_matching1 == 0 & test_treatment1 == 2]) +
      sum(test_matching1 == 1 & test_treatment1 == 1) *
      mean(test_outcome[test_matching1 == 1 & test_treatment1 == 2])
  ) / sum(test_treatment1 == 1)
)

ref3a <- c(
  "1" = (
    sum(test_matching1 == 0 & test_treatment1 == 1) *
      mean(test_outcome[test_matching1 == 0 & test_treatment1 == 1]) +
      sum(test_matching1 == 1 & test_treatment1 == 1) *
      mean(test_outcome[test_matching1 == 1 & test_treatment1 == 1])
  ) / sum(test_treatment1 == 1)
)

ref4 <- c(
  "1" = (
    sum(test_matching1 == 0 & test_subset) * mean(test_outcome[test_matching1 == 0 & test_treatment1 == 1]) +
      sum(test_matching1 == 1 & test_subset) * mean(test_outcome[test_matching1 == 1 & test_treatment1 == 1])
  ) / sum(test_subset),
  "2" = (
    sum(test_matching1 == 0 & test_subset) * mean(test_outcome[test_matching1 == 0 & test_treatment1 == 2]) +
      sum(test_matching1 == 1 & test_subset) * mean(test_outcome[test_matching1 == 1 & test_treatment1 == 2])
  ) / sum(test_subset)
)

ref4a <- c(
  "1" = (
    sum(test_matching1 == 0 & test_subset) * mean(test_outcome[test_matching1 == 0 & test_treatment1 == 1]) +
      sum(test_matching1 == 1 & test_subset) * mean(test_outcome[test_matching1 == 1 & test_treatment1 == 1])
  ) / sum(test_subset)
)


test_that("potential_outcomes produces correct output.", {
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment1,
                                  test_matching1), ref1)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment1,
                                  test_matching1,
                                  estimands = "1"), ref1a)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment2,
                                  test_matching1), ref1)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment2,
                                  test_matching1,
                                  estimands = 1), ref1a)

  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment1,
                                  test_matching2), ref2)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment1,
                                  test_matching2,
                                  estimands = "1"), ref2a)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment2,
                                  test_matching2), ref2)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment2,
                                  test_matching2,
                                  estimands = 1), ref2a)

  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment1,
                                  test_matching1,
                                  subset = 1), ref3)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment1,
                                  test_matching1,
                                  estimands = "1",
                                  subset = 1), ref3a)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment2,
                                  test_matching1,
                                  subset = 1), ref3)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment2,
                                  test_matching1,
                                  estimands = 1,
                                  subset = 1), ref3a)

  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment1,
                                  test_matching1,
                                  subset = test_subset), ref4)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment1,
                                  test_matching1,
                                  estimands = "1",
                                  subset = test_subset), ref4a)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment2,
                                  test_matching1,
                                  subset = test_subset), ref4)
  expect_equal(potential_outcomes(test_outcome,
                                  test_treatment2,
                                  test_matching1,
                                  estimands = 1,
                                  subset = test_subset), ref4a)
})


outcome1 <- c(0.405525727430359, 0.812848279485479, 0.751701522618532, 0.369859978090972, 0.0828832359984517, 0.0581807587295771, 0.0755079395603389, 0.462264972738922, 0.883537364890799, 0.184049236821011, 0.677623615832999, 0.866342591354623, 0.190257553709671, 0.839609482092783, 0.6510562999174, 0.406047733966261, 0.737749932100996, 0.182497988687828, 0.0939545268192887, 0.0681507710833102, 0.438144408399239, 0.395082984119654, 0.293674960965291, 0.0621271352283657, 0.173682052642107, 0.23951577488333, 0.0109507569577545, 0.091836221748963, 0.824933222960681, 0.16530729434453, 0.620773951523006, 0.958596014184877, 0.536811362253502, 0.708960501709953, 0.168887306703255, 0.149347468977794, 0.043619975913316, 0.862376977223903, 0.892320192651823, 0.89996472094208, 0.523822985356674, 0.449652388459072, 0.129791148705408, 0.72630649805069, 0.997333243722096, 0.660752305295318, 0.892215670319274, 0.268775291508064, 0.999640412628651, 0.995845924364403, 0.508668508147821, 0.438040748471394, 0.205370124196634, 0.878601222997531, 0.603225022321567, 0.280399607494473, 0.00127401761710644, 0.653314489172772, 0.332287714118138, 0.0459039981942624, 0.0401524326298386, 0.739893379621208, 0.387678750790656, 0.522295238915831, 0.641945166978985, 0.378986078780144, 0.687857278622687, 0.616588046308607, 0.767046783817932, 0.845434815622866, 0.781069684075192, 0.937888963147998, 0.327032405184582, 0.772445913869888, 0.301286236615852, 0.641092204721645, 0.322475777473301, 0.153312802081928, 0.150135542266071, 0.327025729930028, 0.920420436421409, 0.348250638926402, 0.72784737800248, 0.756019879831001, 0.167359388200566, 0.990394459571689, 0.336550832726061, 0.419682690408081, 0.455126746324822, 0.694204999366775, 0.814578007673845, 0.812302863923833, 0.726125823799521, 0.218008558498695, 0.919011298799887, 0.990311983739957, 0.533001429401338, 0.432248338125646, 0.0948147349990904, 0.896156438160688)
outcome2 <- c(0.419541977345943, 0.330326879397035, 0.406650325050578, 0.962376165436581, 0.435986848548055, 0.679863432655111, 0.0871483124792576, 0.508654234930873, 0.705143598839641, 0.598956637782976, 0.920526552246884, 0.653517587343231, 0.0953848212957382, 0.0679926499724388, 0.778072308050469, 0.580764731857926, 0.15788002521731, 0.698253975016996, 0.201664328807965, 0.155539465369657, 0.57892510201782, 0.115163152338937, 0.472972247982398, 0.677076234016567, 0.718570912955329, 0.66102970787324, 0.586311777122319, 0.976821342017502, 0.912178367609158, 0.969796306220815, 0.452542269602418, 0.8005081852898, 0.610480771167204, 0.363242165418342, 0.79808733612299, 0.279592148493975, 0.544926539529115, 0.283695676131174, 0.226647106464952, 0.116181207820773, 0.995180279947817, 0.738292481983081, 0.901764570269734, 0.443145213183016, 0.50750860478729, 0.388973765540868, 0.623749679652974, 0.41152072395198, 0.677472063107416, 0.263802072266117, 0.816315291449428, 0.413551086559892, 0.928417247487232, 0.0677707095164806, 0.704385258955881, 0.432431754656136, 0.295586160384119, 0.51029753498733, 0.96862543723546, 0.318614579970017, 0.847224656259641, 0.847046320559457, 0.0702245079446584, 0.546595833031461, 0.5279052965343, 0.308200541185215, 0.573142700362951, 0.997842114185914, 0.155169836943969, 0.151510094525293, 0.386162474285811, 0.393618390196934, 0.959006758639589, 0.160810516448691, 0.45279016229324, 0.923675600904971, 0.698369024088606, 0.405743609182537, 0.915793015155941, 0.0908478312194347, 0.597003715112805, 0.412287131883204, 0.221393809653819, 0.975412162719294, 0.30523572047241, 0.420510626863688, 0.447349149733782, 0.0543892264831811, 0.592279243981466, 0.144155940506607, 0.437688226811588, 0.247154575306922, 0.70068475487642, 0.668919923482463, 0.338778334669769, 0.689953552791849, 0.519251861143857, 0.819856939138845, 0.951092007569969, 0.0146876031067222)
treatment1 <- factor(c(rep("T", 50), rep("C", 50)))
treatment2 <- factor(c("T", "T", "O", "C", "O", "C", "O", "T", "C", "T", "T", "C", "T", "T", "O", "C", "C", "C", "C", "C", "C", "C", "O", "O", "T", "C", "T", "C", "T", "C", "T", "C", "O", "O", "T", "O", "T", "O", "C", "O", "O", "O", "T", "T", "T", "O", "T", "C", "T", "O", "C", "C", "O", "T", "O", "O", "O", "T", "T", "O", "T", "T", "T", "C", "O", "C", "O", "T", "C", "O", "C", "C", "T", "T", "C", "O", "C", "O", "C", "T", "C", "C", "O", "C", "C", "C", "O", "O", "C", "T", "C", "C", "C", "C", "O", "O", "C", "C", "T", "C"))
matching1 <- Rscc_clustering(c(8, 1, 7, 5, 14, 4, 15, 2, 11, 3, 10, 6, 20, 8, 19, 7, 13, 15, 15, 3, 4, 14, 16, 6, 5, 17, 13, 17, 12, 4, 9, 14, 18, 10, 9, 8, 19, 4, 15, 2, 17, 10, 19, 16, 6, 3, 12, 19, 8, 18, 6, 19, 10, 7, 12, 7, 2, 16, 11, 18, 1, 12, 11, 10, 14, 4, 13, 13, 5, 15, 3, 18, 1, 20, 11, 1, 5, 14, 5, 11, 16, 6, 1, 2, 8, 9, 18, 17, 20, 7, 3, 17, 12, 9, 20, 13, 16, 2, 20, 9))
matching2 <- Rscc_clustering(c(1, 1, 0, 1, 0, 2, 2, 1, 1, 0, 1, 0, 0, 1, 2, 1, 1, 1, 2, 1, 0, 3, 1, 0, 2, 0, 0, 1, 2, 0, 1, 1, 2, 1, 1, 1, 0, 1, 1, 0, 0, 2, 1, 2, 1, 2, 0, 1, 1, 1, 3, 2, 1, 0, 0, 0, 1, 1, 2, 1, 3, 1, 1, 1, 1, 0, 0, 1, 1, 1, 2, 1, 0, 0, 2, 0, 0, 3, 1, 0, 2, 0, 0, 1, 2, 2, 2, 0, 2, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1))
matching3 <- Rscc_clustering(c(1, 1, 2, 5, 1, 4, 3, 4, 2, 3, 5, 3, 2, 2, 3, 1, 5, 5, 1, 2, 1, 1, 1, 3, 5, 1, 2, 1, 1, 5, 2, 4, 3, 4, 1, 4, 3, 4, 4, 1, 5, 3, 3, 4, 2, 5, 5, 5, 1, 2, 3, 5, 1, 4, 4, 3, 3, 2, 2, 1, 5, 4, 5, 3, 4, 5, 3, 3, 2, 2, 1, 3, 2, 4, 1, 4, 5, 4, 2, 3, 4, 3, 4, 1, 5, 2, 2, 4, 3, 2, 3, 1, 5, 5, 4, 2, 5, 5, 4, 2))

test_against_replica <- function(outcomes,
                                 treatments,
                                 matching,
                                 estimands,
                                 subset) {
  eval(bquote(expect_equal(potential_outcomes(outcomes,
                                              treatments,
                                              matching,
                                              estimands,
                                              subset),
                           replica_potential_outcomes(outcomes,
                                                      treatments,
                                                      matching,
                                                      estimands,
                                                      subset))))
}


outcome1 <- c(0.405525727430359, 0.812848279485479, 0.751701522618532, 0.369859978090972, 0.0828832359984517, 0.0581807587295771, 0.0755079395603389, 0.462264972738922, 0.883537364890799, 0.184049236821011, 0.677623615832999, 0.866342591354623, 0.190257553709671, 0.839609482092783, 0.6510562999174, 0.406047733966261, 0.737749932100996, 0.182497988687828, 0.0939545268192887, 0.0681507710833102, 0.438144408399239, 0.395082984119654, 0.293674960965291, 0.0621271352283657, 0.173682052642107, 0.23951577488333, 0.0109507569577545, 0.091836221748963, 0.824933222960681, 0.16530729434453, 0.620773951523006, 0.958596014184877, 0.536811362253502, 0.708960501709953, 0.168887306703255, 0.149347468977794, 0.043619975913316, 0.862376977223903, 0.892320192651823, 0.89996472094208, 0.523822985356674, 0.449652388459072, 0.129791148705408, 0.72630649805069, 0.997333243722096, 0.660752305295318, 0.892215670319274, 0.268775291508064, 0.999640412628651, 0.995845924364403, 0.508668508147821, 0.438040748471394, 0.205370124196634, 0.878601222997531, 0.603225022321567, 0.280399607494473, 0.00127401761710644, 0.653314489172772, 0.332287714118138, 0.0459039981942624, 0.0401524326298386, 0.739893379621208, 0.387678750790656, 0.522295238915831, 0.641945166978985, 0.378986078780144, 0.687857278622687, 0.616588046308607, 0.767046783817932, 0.845434815622866, 0.781069684075192, 0.937888963147998, 0.327032405184582, 0.772445913869888, 0.301286236615852, 0.641092204721645, 0.322475777473301, 0.153312802081928, 0.150135542266071, 0.327025729930028, 0.920420436421409, 0.348250638926402, 0.72784737800248, 0.756019879831001, 0.167359388200566, 0.990394459571689, 0.336550832726061, 0.419682690408081, 0.455126746324822, 0.694204999366775, 0.814578007673845, 0.812302863923833, 0.726125823799521, 0.218008558498695, 0.919011298799887, 0.990311983739957, 0.533001429401338, 0.432248338125646, 0.0948147349990904, 0.896156438160688)
outcome2 <- c(0.419541977345943, 0.330326879397035, 0.406650325050578, 0.962376165436581, 0.435986848548055, 0.679863432655111, 0.0871483124792576, 0.508654234930873, 0.705143598839641, 0.598956637782976, 0.920526552246884, 0.653517587343231, 0.0953848212957382, 0.0679926499724388, 0.778072308050469, 0.580764731857926, 0.15788002521731, 0.698253975016996, 0.201664328807965, 0.155539465369657, 0.57892510201782, 0.115163152338937, 0.472972247982398, 0.677076234016567, 0.718570912955329, 0.66102970787324, 0.586311777122319, 0.976821342017502, 0.912178367609158, 0.969796306220815, 0.452542269602418, 0.8005081852898, 0.610480771167204, 0.363242165418342, 0.79808733612299, 0.279592148493975, 0.544926539529115, 0.283695676131174, 0.226647106464952, 0.116181207820773, 0.995180279947817, 0.738292481983081, 0.901764570269734, 0.443145213183016, 0.50750860478729, 0.388973765540868, 0.623749679652974, 0.41152072395198, 0.677472063107416, 0.263802072266117, 0.816315291449428, 0.413551086559892, 0.928417247487232, 0.0677707095164806, 0.704385258955881, 0.432431754656136, 0.295586160384119, 0.51029753498733, 0.96862543723546, 0.318614579970017, 0.847224656259641, 0.847046320559457, 0.0702245079446584, 0.546595833031461, 0.5279052965343, 0.308200541185215, 0.573142700362951, 0.997842114185914, 0.155169836943969, 0.151510094525293, 0.386162474285811, 0.393618390196934, 0.959006758639589, 0.160810516448691, 0.45279016229324, 0.923675600904971, 0.698369024088606, 0.405743609182537, 0.915793015155941, 0.0908478312194347, 0.597003715112805, 0.412287131883204, 0.221393809653819, 0.975412162719294, 0.30523572047241, 0.420510626863688, 0.447349149733782, 0.0543892264831811, 0.592279243981466, 0.144155940506607, 0.437688226811588, 0.247154575306922, 0.70068475487642, 0.668919923482463, 0.338778334669769, 0.689953552791849, 0.519251861143857, 0.819856939138845, 0.951092007569969, 0.0146876031067222)
treatment1 <- factor(c(rep("T", 50), rep("C", 50)))
treatment2 <- factor(c("T", "T", "O", "C", "O", "C", "O", "T", "C", "T", "T", "C", "T", "T", "O", "C", "C", "C", "C", "C", "C", "C", "O", "O", "T", "C", "T", "C", "T", "C", "T", "C", "O", "O", "T", "O", "T", "O", "C", "O", "O", "O", "T", "T", "T", "O", "T", "C", "T", "O", "C", "C", "O", "T", "O", "O", "O", "T", "T", "O", "T", "T", "T", "C", "O", "C", "O", "T", "C", "O", "C", "C", "T", "T", "C", "O", "C", "O", "C", "T", "C", "C", "O", "C", "C", "C", "O", "O", "C", "T", "C", "C", "C", "C", "O", "O", "C", "C", "T", "C"))
matching1 <- Rscc_clustering(c(8, 1, 7, 5, 14, 4, 15, 2, 11, 3, 10, 6, 20, 8, 19, 7, 13, 15, 15, 3, 4, 14, 16, 6, 5, 17, 13, 17, 12, 4, 9, 14, 18, 10, 9, 8, 19, 4, 15, 2, 17, 10, 19, 16, 6, 3, 12, 19, 8, 18, 6, 19, 10, 7, 12, 7, 2, 16, 11, 18, 1, 12, 11, 10, 14, 4, 13, 13, 5, 15, 3, 18, 1, 20, 11, 1, 5, 14, 5, 11, 16, 6, 1, 2, 8, 9, 18, 17, 20, 7, 3, 17, 12, 9, 20, 13, 16, 2, 20, 9))
matching2 <- Rscc_clustering(c(1, 1, 0, 1, 0, 2, 2, 1, 1, 0, 1, 0, 0, 1, 2, 1, 1, 1, 2, 1, 0, 3, 1, 0, 2, 0, 0, 1, 2, 0, 1, 1, 2, 1, 1, 1, 0, 1, 1, 0, 0, 2, 1, 2, 1, 2, 0, 1, 1, 1, 3, 2, 1, 0, 0, 0, 1, 1, 2, 1, 3, 1, 1, 1, 1, 0, 0, 1, 1, 1, 2, 1, 0, 0, 2, 0, 0, 3, 1, 0, 2, 0, 0, 1, 2, 2, 2, 0, 2, 0, 1, 1, 1, 1, 0, 0, 0, 1, 0, 1))
matching3 <- Rscc_clustering(c(1, 1, 2, 5, 1, 4, 3, 4, 2, 3, 5, 3, 2, 2, 3, 1, 5, 5, 1, 2, 1, 1, 1, 3, 5, 1, 2, 1, 1, 5, 2, 4, 3, 4, 1, 4, 3, 4, 4, 1, 5, 3, 3, 4, 2, 5, 5, 5, 1, 2, 3, 5, 1, 4, 4, 3, 3, 2, 2, 1, 5, 4, 5, 3, 4, 5, 3, 3, 2, 2, 1, 3, 2, 4, 1, 4, 5, 4, 2, 3, 4, 3, 4, 1, 5, 2, 2, 4, 3, 2, 3, 1, 5, 5, 4, 2, 5, 5, 4, 2))
subset1 <- c(TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE)
subset2 <- c(FALSE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE)
subset3 <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, TRUE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, FALSE)

test_that("potential_outcomes is identical to replica.", {
  test_against_replica(outcome1, treatment1, matching1, NULL, NULL)
  test_against_replica(outcome1, treatment1, matching2, NULL, NULL)
  test_against_replica(outcome1, treatment1, matching3, NULL, NULL)
  test_against_replica(outcome1, treatment2, matching1, NULL, NULL)
  test_against_replica(outcome1, treatment2, matching2, NULL, NULL)
  test_against_replica(outcome1, treatment2, matching3, NULL, NULL)
  test_against_replica(outcome2, treatment1, matching1, NULL, NULL)
  test_against_replica(outcome2, treatment1, matching2, NULL, NULL)
  test_against_replica(outcome2, treatment1, matching3, NULL, NULL)
  test_against_replica(outcome2, treatment2, matching1, NULL, NULL)
  test_against_replica(outcome2, treatment2, matching2, NULL, NULL)
  test_against_replica(outcome2, treatment2, matching3, NULL, NULL)

  test_against_replica(outcome1, treatment1, matching1, "T", NULL)
  test_against_replica(outcome1, treatment1, matching2, "C", NULL)
  test_against_replica(outcome1, treatment1, matching3, "T", NULL)
  test_against_replica(outcome1, treatment2, matching1, "T", NULL)
  test_against_replica(outcome1, treatment2, matching2, "C", NULL)
  test_against_replica(outcome1, treatment2, matching3, "O", NULL)
  test_against_replica(outcome2, treatment1, matching1, "C", NULL)
  test_against_replica(outcome2, treatment1, matching2, "T", NULL)
  test_against_replica(outcome2, treatment1, matching3, "C", NULL)
  test_against_replica(outcome2, treatment2, matching1, c("T", "C"), NULL)
  test_against_replica(outcome2, treatment2, matching2, c("T", "O"), NULL)
  test_against_replica(outcome2, treatment2, matching3, c("C", "O"), NULL)

  test_against_replica(outcome1, treatment1, matching1, NULL, "T")
  test_against_replica(outcome1, treatment1, matching2, NULL, "C")
  test_against_replica(outcome1, treatment1, matching3, NULL, subset1)
  test_against_replica(outcome1, treatment2, matching1, NULL, "T")
  test_against_replica(outcome1, treatment2, matching2, NULL, c("T", "C"))
  test_against_replica(outcome1, treatment2, matching3, NULL, "C")
  test_against_replica(outcome2, treatment1, matching1, NULL, "T")
  test_against_replica(outcome2, treatment1, matching2, NULL, subset2)
  test_against_replica(outcome2, treatment1, matching3, NULL, "C")
  test_against_replica(outcome2, treatment2, matching1, NULL, subset2)
  test_against_replica(outcome2, treatment2, matching2, NULL, subset1)
  test_against_replica(outcome2, treatment2, matching3, NULL, subset3)

  test_against_replica(outcome1, treatment1, matching1, "T", subset1)
  test_against_replica(outcome1, treatment1, matching2, "C", "T")
  test_against_replica(outcome1, treatment1, matching3, "T", subset3)
  test_against_replica(outcome1, treatment2, matching1, "T", c("C", "O"))
  test_against_replica(outcome1, treatment2, matching2, "C", "T")
  test_against_replica(outcome1, treatment2, matching3, "O", "C")
  test_against_replica(outcome2, treatment1, matching1, "C", subset2)
  test_against_replica(outcome2, treatment1, matching2, "T", "C")
  test_against_replica(outcome2, treatment1, matching3, "C", "T")
  test_against_replica(outcome2, treatment2, matching1, c("T", "C"), subset3)
  test_against_replica(outcome2, treatment2, matching2, c("T", "O"), c("O", "T"))
  test_against_replica(outcome2, treatment2, matching3, c("C", "O"), "T")
})


