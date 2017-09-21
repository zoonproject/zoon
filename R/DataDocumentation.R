
#' @name AplumbeusOcc
#' @title UK occurrence data for Anopheles plumbeus as taken from GBIF
#' @description This is an example occurrence only data for the species
#'    Anopheles plumbeus. The data are taken from GBIF and restricted to the
#'    UK. These data are used in the module UKAnophelesPlumbeus which makes for
#'    a quick running occurrence module for testing and playing with zoon.
#' @docType data
#' @format data.frame with five columns, longitude, latitude, value (1 for
#'    presence), type (presence) and a column of 1s indicating this is
#'    training data not external validation data.
#' @source GBIF
#' @author Tim Lucas September 2014


NULL


#' @name UKAirRas
#' @title UK Air temperature raster layer.
#' @description This is an example environmental covariate raster layer. It is
#'   surface temperatures for the UK taken from NCEP
#' @docType data
#' @format Raster layer
#' @source NCEP
#' @author Tim Lucas September 2014


NULL



#' @name CWBZim
#' @title Presence/absence of the coffee white stem borer in Zimbabwe 2003
#' @description This is an example presence/absence dataset for the
#' coffee white stem borer \emph{Monochamus leuconotus} P. taken from an
#' open access dataset on the Dryad data repository.
#' The data are made available by those authors under a Creative Commons CC0
#' These data are used in the module CWBZimbabwe which can be used for running
#' toy presence/absence species distribution models.
#' @docType data
#' @format data.frame with five columns, longitude, latitude, value (1 for
#'    presence), type (presence) and a column of 1s indicating this is
#'    training data not external validation data.
#' @source Original publication:
#' Kutywayo D, Chemura A, Kusena W, Chidoko P, Mahoya C (2013) The impact of
#'  climate change on the potential distribution of agricultural pests: the case
#'   of the coffee white stem borer (\emph{Monochamus leuconotus} P.) in
#'    Zimbabwe. PLoS ONE 8(8): e73432.
#'    Dryad data package:
#'    Kutywayo D, Chemura A, Kusena W, Chidoko P, Mahoya C (2013) Data from:
#'    The impact of climate change on the potential distribution of agricultural
#'     pests: the case of the coffee white stem borer (\emph{Monochamus
#'      leuconotus} P.) in Zimbabwe. Dryad Digital Repository.
#'    \url{http://dx.doi.org/10.1371/journal.pone.0073432}
#'
#' @author Nick Golding August 2015

NULL


#' @name FrescaloBias
#' @title Example bias raster for plants in England
#' @description This is an example bias raster giving a modelled estimate of the
#' relative recording effort for plants in England using the Frescalo function in the R package sparta.
#' @docType data
#' @format RasterLayer with extent: c(-7.083, 2.167, 49.83, 55.83) and values ranging between 0 and 1.
#'
#' @author Tom August & Nick Golding September 2015

NULL
