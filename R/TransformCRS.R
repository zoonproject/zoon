#' Change the CRS of occurrence data
#'
#' Takes a dataframe returned by an occurrence module or a raster object from a
#' covariate module and converts the CRS to lat/long so that everything works
#' together.
#'
#' @param occurrence The output of an occurrence module
#' @param ras_projection The projection of a covariate layer as a character
#'   (from projection())
#' @return The same object as in occurrence, but with CRS changed as needed
#'
#' @name TransformCRS
#' @import rgdal
#' @import sp
#' @importFrom stats na.omit
#' @export

TransformCRS <- function(occurrence, ras_projection) {

  # Sense checks #
  if (!inherits(what = "data.frame", x = occurrence))
    stop("occurrence must be a data.frame")
  if (!inherits(what = "character", x = ras_projection))
    stop("ras_projection must be a character")
  tryCatch(
    expr = {
      CRS(ras_projection)
    },
    error = function(e) {
      stop (paste0("CRS provided in covariate data [",
                   ras_projection,
                   "] is not a recognised CRS. ",
                   e))
    }
  )
  if (!"crs" %in% tolower(colnames(occurrence)))
    stop('Transform CRS expects occurrence data to have a "crs" column')

  col <- grep("crs", tolower(names(occurrence)))
  crs_current <- as.character(unique(occurrence[, col]))

  if (length(crs_current) > 1) {
    stop ("In occurrence module: There is more than one CRS type specified ",
          "in the CRS column currently we zoon support one")
  }

  tryCatch(
    expr = {
      CRS(crs_current)
    },
    error = function(e) {
      stop( paste0("CRS provided in occurrence data [",
                   crs_current,
                   "] is not a recognised CRS. ",
                   e))
    }
  )

  if (ras_projection != crs_current) {
    message(paste(
      "Occurrence data will be transformed from",
      crs_current, "to", ras_projection,
      "to match covariate data"
    ))

    occ_cords <- data.frame(
      X = as.numeric(occurrence$longitude),
      Y = as.numeric(occurrence$latitude)
    )

    occ_cords_omit <- na.omit(occ_cords)

    LL_points <- SpatialPoints(coords = occ_cords_omit,
                               proj4string = CRS(crs_current))

    LL_new <- spTransform(x = LL_points,
                          CRSobj = CRS(ras_projection))

    XY_new <- coordinates(LL_new)

    # merge back to orignals (containing NAs)
    XY_NAs <- occ_cords

    if ("na.action" %in% names(attributes(occ_cords_omit))) {
      XY_NAs[-attr(occ_cords_omit, "na.action"), ] <- XY_new
      XY_NAs[attr(occ_cords_omit, "na.action"), ] <- NA
    } else {
      XY_NAs <- XY_new
    }

    occurrence$longitude <- XY_NAs[, 1]
    occurrence$latitude <- XY_NAs[, 2]

    return(occurrence)
  } else if (ras_projection == crs_current) {

    # No transformation needed
    return(occurrence)
  }
}
