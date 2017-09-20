#' Combine rasters
#'
#' A generalised function to combine rasters of differing CRS and resolution.
#' The function takes and returns a list of Raster* objects. The final CRS is
#' the one most frequent in the rasters being combined, or lat/long if there is
#' no CRS is most frequent
#'
#' @param rasters A list of Raster* objects to be converted to a common CRS and
#'   resolution
#' @param method The method used in raster::projectRaster. Either 'ngb' (nearest
#'   neighbor), which is useful for categorical variables, or 'bilinear'
#'   (bilinear interpolation; the default value), which is appropriate for
#'   continuous variables
#' @return A list of rasters
#'
#' @name CombineRasters
#' @import raster
#' @export

CombineRasters <- function(rasters, method = "ngb") {

  # This is the function used for combining
  combine <- function(rasters, method = method, crs) {
    tocrs <- unlist(lapply(
      rasters,
      function(x, crs) {
        projection(x) != crs
      },
      crs = crs
    ))

    rasters[tocrs] <- lapply(
      rasters[tocrs],
      FUN = function(x) {
        projectRaster(x, crs = crs, method = method)
      }
    )
    
    edge <- function (x, i, j) {
      bbox(x)[i, j]
    }
    
    # Manually compute the minimal extent
    xmin <- max(vapply(rasters,
                       edge, 1, 1,
                       FUN.VALUE = 1))
    xmax <- min(vapply(rasters,
                       edge, 1, 2,
                       FUN.VALUE = 1))
    ymin <- max(vapply(rasters,
                       edge, 2, 1,
                       FUN.VALUE = 1))
    ymax <- min(vapply(rasters,
                       edge, 2, 2,
                       FUN.VALUE = 1))

    # error is the extents dont match
    if (xmin >= xmax | ymin >= ymax) {
      stop ('Rasters in covariates modules do not overlap. ",
            "Review their extents.')
    }
    
    # Get the minimum resolution of these
    resolutions <- vapply(rasters,
                          raster::res,
                          FUN.VALUE = c(1, 1))
    x_min <- min(resolutions[1, ])
    y_min <- min(resolutions[2, ])

    tores <- apply(
      MARGIN = 2,
      X = resolutions,
      FUN = function(x) {
        !(x_min == x[1] & y_min == x[2])
      }
    )

    # use the same resolution across all rasters
    rasters[tores] <- lapply(
      rasters[tores],
      FUN = function(x) {
        projectRaster(x, res = c(x_min, y_min), crs = crs, method = method)
      }
    )

    # Now crop these rasters to the minimal common extent
    newextent <- c(xmin, xmax, ymin, ymax)

    # Which raster need to be cropped?
    ras_crop <- vapply(rasters,
                       FUN = function(x) {
                         extent(x) != extent(newextent)
                       },
                       FUN.VALUE = FALSE)
    
    # Crop the ones that need to be cropped (and report cropping)
    if (any(ras_crop)) {
      message(
        'Not all rasters have the same extent, they will be cropped ",
              "to the minimal area covered by all rasters: ',
        paste(newextent, collapse = ", ")
      )

      project_to <- raster(
        ext = extent(newextent),
        resolution = c(x_min, y_min),
        crs = crs
      )

      rasters[ras_crop] <- lapply(
        rasters[ras_crop],
        FUN = function(x) {
          projectRaster(from = x, to = project_to)
        }
      )
    }

    return(rasters)
  }

  if (any(is.na(lapply(rasters, FUN = raster::projection)))) {
    stop(paste(
      sum(is.na(lapply(rasters, FUN = raster::projection))),
      "covariate rasters do not have a projection",
      "(see ?raster::projection). Covariate rasters",
      "need a projection so that they can be correctly",
      "combined with occurrence data and other rasters."
    ))
  }

  # Get frequency of projections
  projections <- table(unlist(lapply(rasters, FUN = raster::projection)))

  if (sum(projections) > 1) {
    if (length(grep(max(projections), projections)) > 1) {
      newProjection <- paste(
        "+init=epsg:4326 +proj=longlat +datum=WGS84",
        "+no_defs +ellps=WGS84 +towgs84=0,0,0"
      )

      message(
        "Covariate rasters have been converted to WGS84 ",
        "latitude/longitude in order to be combined. ",
        "To save time make sure all your covariates ",
        "have the same projection."
      )

      return(combine(rasters = rasters, method = method, crs = newProjection))
    } else if (length(projections) == 1) {
      newProjection <- names(sort(projections, decreasing = TRUE))[[1]]

      return(combine(rasters = rasters, method = method, crs = newProjection))
    } else {
      newProjection <- names(sort(projections, decreasing = TRUE))[[1]]

      message(
        "Covariate layers have been converted to a new projection ",
        "to be combined: ", newProjection, " To save time ",
        "make sure all your covariates have the same projection."
      )

      return(combine(rasters = rasters, method = method, crs = newProjection))
    }
  } else {
    return(rasters)
  }
}
