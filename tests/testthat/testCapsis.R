#'
#' Example of meta-model for OSM and an FMU in Bas-Saint-Laurent
#'

rm(list=ls())

library(CapsisWebAPI4R)

variantList <- CapsisGetVariantList()

test_that("Check if variant list is populated", {
  expect_equal("ARTEMIS" %in% variantList, T)
})

variant <- "ARTEMIS2014"

speciesConiferous <- CapsisGetVariantSpecies(variant, "Coniferous")
speciesBroadleaved <- CapsisGetVariantSpecies(variant, "Broadleaved")
test_that("Check if coniferous and broadleaved species lists", {
  expect_equal(length(speciesBroadleaved), 22)
  expect_equal(length(speciesConiferous), 8)
})

variantFields <- CapsisGetVariantFields(variant)
test_that("Check variant fields", {
  expect_equal(nrow(variantFields) > 20, T)
})

outputRequestTypes <- CapsisGetOutputRequestTypes(variant)
test_that("Check output request type", {
  expect_equal("AliveVolume" %in% outputRequestTypes, T)
})

scope <- CapsisGetVariantScope(variant)
test_that("Check output request type", {
  expect_equal("VEG_POT" %in% names(scope), T)
})

outputRequestList <- CFSCommonGYModelWebAPI4R::new_OutputRequestList()
outputRequestList$addOutputRequest("AliveVolume", list(Coniferous = speciesConiferous, Broadleaved = speciesBroadleaved))

dataStratum <- CapsisWebAPI4R::STR_RE2_70

colnames(dataStratum) <- c("ManagStr", "PLOT", "LATITUDE", "LONGITUDE", "ALTITUDE", "ECOREGION", "TYPEECO", "SLOPE_CLASS",
                       "DRAINAGE_CLASS", "NO_ARBRE", "SPECIES", "TREESTATUS", "TREEDHPCM", "TREEHEIGHT", "TREEFREQ", "ANNEE_SOND")

df <- CapsisSimulate(dataStratum, outputRequestList, variant, 60, 2015, T, 100, "NoChange", "FMU")
ds <- df$dataSet
test_that("Check simulation results", {
  expect_equal(df$nbPlots, 40)
  expect_equal(nrow(ds), 14)
  expect_equal(max(ds$timeSinceInitialDateYr), 60)
})

