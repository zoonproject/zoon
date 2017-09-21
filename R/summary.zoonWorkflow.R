#' A function to summarize the output of a zoon workflow
#'
#' Renders and opens an HTML report
#'
#' @param object A zoonWorkflow object
#' @param \dots currently ignored
#' @return Path to HTML file. Associated images will be in the same directory.
#' @method summary zoonWorkflow
#' @name summary.zoonWorkflow
#' @export

summary.zoonWorkflow <- function(object, ...) {

  # write a tempoary HTML file
  htmlPath <- tempfile(fileext = ".html")

  html <- summaryHTML(object)

  writeLines(
    text = html,
    con = htmlPath
  )

  browseURL(url = htmlPath)

  return(htmlPath)
}

summaryHTML <- function(workflow) {
  top <- "<!DOCTYPE html>
          <html>
          <body>"

  zoonLogo <- paste0('<img src="https://github.com/zoonproject/blog/raw/',
                     'master/zoon_top.png" alt="Zoon Logo", style="max-height:',
                     ' 170px; max-width: 100%;">')

  callHeader <- paste(
    "<h1>Zoon Workflow summary</h1><br>",
    "<p>Call:", workflow$call, "<p/>"
  )

  occurrenceHeader <- "<hr><h2>Occurrence modules</h2>"

  occurrenceCall <- paste(
    "<p>",
    "Parameterisation: ",
    substr(
      workflow$call,
      start = regexpr(pattern = "occurrence = ", workflow$call)[1],
      stop = regexpr(pattern = ", covariate = ", workflow$call)[1] - 1
    ),
    "</p>", sep = ""
  )

  occurrenceHTML <- occurrenceSummary(Occurrence(workflow))

  covariateHeader <- '<br><hr style="clear: both;"><h2>Covariate modules</h2>'

  covariateCall <- paste(
    "<p>",
    "Parameterisation: ",
    substr(
      workflow$call,
      start = regexpr(pattern = "covariate = ", workflow$call)[1],
      stop = regexpr(pattern = ", process =", workflow$call)[1] - 1
    ),
    "</p>", sep = ""
  )

  covariateHTML <- covariateSummary(Covariate(workflow))

  processHeader <- '<br><hr style="clear: both;"><h2>Process modules</h2>'

  processCall <- paste(
    "<p>",
    "Parameterisation: ",
    substr(
      workflow$call,
      start = regexpr(pattern = "process =", workflow$call)[1],
      stop = regexpr(pattern = ", model =", workflow$call)[1] - 1
    ),
    "</p>", sep = ""
  )

  processHTML <- processSummary(Process(workflow))

  modelHeader <- '<br><hr style="clear: both;"><h2>Model modules</h2>'

  modelCall <- paste(
    "<p>",
    "Parameterisation: ",
    substr(
      workflow$call,
      start = regexpr(pattern = "model =", workflow$call)[1],
      stop = regexpr(pattern = ", output =", workflow$call)[1] - 1
    ),
    "</p>", sep = ""
  )

  modelHTML <- modelSummary(Model(workflow))

  outputHeader <- '<br><hr style="clear: both;"><h2>Output modules</h2>'

  outputCall <- paste(
    "<p>",
    "Parameterisation: ",
    substr(
      workflow$call,
      start = regexpr(pattern = "output =", workflow$call)[1],
      stop = regexpr(pattern = ", forceReproducible =", workflow$call)[1] - 1
    ),
    "</p>", sep = ""
  )

  outputHTML <- outputSummary(workflow$report)

  bottom <- "</body>
             </html>"

  html <- paste(
    top,
    zoonLogo,
    callHeader,
    occurrenceHeader,
    occurrenceCall,
    occurrenceHTML,
    covariateHeader,
    covariateCall,
    covariateHTML,
    processHeader,
    processCall,
    processHTML,
    modelHeader,
    modelCall,
    modelHTML,
    outputHeader,
    outputCall,
    outputHTML,
    bottom,
    sep = "\n"
  )

  return(html)
}

occurrenceSummary <- function(occ) {
  if (is.null(occ)) {
    return("<p>No output was returned from the occurrence module</p>")
  } else if (is.data.frame(occ)) {
    html <- occHTML(occ)
  } else {
    listHTML <- lapply(occ, occHTML)

    paste(unlist(listHTML), collapse = "")
  }
}

#' @importFrom grDevices dev.off png
#' @importFrom utils methods

occHTML <- function(occ) {
  nrows <- nrow(occ)
  minLat <- round(min(occ$latitude), 3)
  maxLat <- round(max(occ$latitude), 3)
  minLong <- round(min(occ$longitude), 3)
  maxLong <- round(max(occ$longitude), 3)

  HTML <- paste(
    "<h3>Module:", attr(occ, "call_path"), "</h3>",
    '<div style="clear: both; float: left; width: 200px;">',
    "<h4>Occurrence data</h4>",
    "<span>Number of rows:", nrows, "</span><br>",
    "<span>Min Latitude:", minLat, "</span><br>",
    "<span>Max Latitude:", maxLat, "</span><br>",
    "<span>Min Longitude:", minLong, "</span><br>",
    "<span>Max Longitude:", maxLong, "</span>",
    "</div>"
  )

  HTML <- paste(
    HTML,
    "<h4>Preview</h4>",
    htmlTable(as.data.frame(head(occ, 50)))
  )

  if (nrow(occ) > 0 & !all(is.na(occ$latitude)) & !all(is.na(occ$longitude))) {
    temppngOcc <- tempfile(fileext = ".png")

    png(filename = temppngOcc)
    plot(
      occ$longitude,
      pch = 16,
      col = "red",
      occ$latitude,
      xlab = "",
      ylab = ""
    )
    dev.off()

    HTML <- paste(
      HTML,
      '<h4 style="clear: both;">Plot</h4>',
      '<img style="clear: both;"',
      paste0('src="', basename(temppngOcc), '"'),
      'alt="Occurrence points">'
    )
  }

  return(HTML)
}

covariateSummary <- function(cov) {
  if (is.null(cov)) {
    return("<p>No output was returned from the covariate module</p>")
  } else if (inherits(cov, c("RasterLayer", "RasterStack", "RasterBrick"))) {
    html <- covHTML(cov)
  } else {
    listHTML <- lapply(cov, covHTML)

    paste(unlist(listHTML), collapse = "")
  }
}

#' @importFrom grDevices dev.off png
#' @importFrom utils methods

covHTML <- function(cov) {
  Nlayers <- length(names(cov))
  minLat <- extent(cov)@ymin
  maxLat <- extent(cov)@ymax
  minLong <- extent(cov)@xmin
  maxLong <- extent(cov)@xmax

  HTML <- paste(
    '<h3 style="clear: both;">Module:', attr(cov, "call_path"), "</h3>",
    '<div style="clear: both; float: left; width: 200px;">',
    "<span>Number of layers:", Nlayers, "</span><br>",
    "<span>Min Latitude:", minLat, "</span><br>",
    "<span>Max Latitude:", maxLat, "</span><br>",
    "<span>Min Longitude:", minLong, "</span><br>",
    "<span>Max Longitude:", maxLong, "</span>",
    "</div>"
  )

  layersFromStack <- function(x) {
    nam <- names(cov[[x]])
    dat <- summary(cov[[x]]@data@values)
    if (!"NA's" %in% names(dat)) dat["NA's"] <- 0
    temp_frame <- data.frame(temp = as.numeric(dat), row.names = names(dat))
    colnames(temp_frame) <- nam
    return(temp_frame)
  }

  tabs <- lapply(names(cov), FUN = layersFromStack)

  summaryTab <- do.call(cbind, tabs)

  HTML <- paste(HTML, htmlTable(summaryTab))

  # If a plot method exists create a plot
  if ("plot" %in% attr(methods(class = class(cov)), "info")$generic) {
    pngtemp <- tempfile(fileext = ".png")
    png(filename = pngtemp)
    plot(cov)
    dev.off()

    nlayerWARN <- NULL
    if (length(names(cov)) > 16)
      nlayerWARN <- "<span>Only the first 16 layers are shown</span><br>"

    HTML <- paste(
      HTML,
      '<h4 style="clear: both;">Plot</h4>',
      nlayerWARN,
      '<img style="clear: both;"',
      paste0('src="', basename(pngtemp), '"'),
      'alt="Covariate raster">'
    )
  }

  return(HTML)
}

htmlTable <- function(dataFrame, maxWidth = 600, maxHeight = 300) {
  colheaders <- colnames(dataFrame)
  rowNames <- row.names(dataFrame)

  tableHTML <- paste('<div style="float: left; max-width: ',
                     maxWidth, "px; max-height: ", maxHeight,
                     'px; overflow: scroll;>">', "<TABLE ><TR><TH></TH><TH>",
                     paste(colheaders, collapse = "</TH><TH>"), "</TH></TR>",
                     sep = "")

  for (i in rowNames) {
    tableHTML <- paste(tableHTML, "<TR><TD>", i, "</TD><TD>",
                       paste(dataFrame[i, ], collapse = "</TD><TD>"),
                       "</TD></TR>", sep = "")
  }

  tableHTML <- paste(tableHTML, "</TABLE>", "</div>", sep = "")

  return(tableHTML)
}

processSummary <- function(pro) {
  if (is.null(pro)) {
    return("<p>No output was returned from the process module</p>")
  } else if (is.data.frame(pro[[1]])) {
    html <- proHTML(pro)
  } else {
    listHTML <- lapply(pro, proHTML)

    paste(unlist(listHTML), collapse = "")
  }
}

proHTML <- function(pro) {
  df <- pro$df
  ras <- pro$ras

  nrows <- nrow(df)
  minLat <- round(min(df$latitude), 3)
  maxLat <- round(max(df$latitude), 3)
  minLong <- round(min(df$longitude), 3)
  maxLong <- round(max(df$longitude), 3)

  HTML <- paste('<h3 style="clear: both;">Module:',
                attr(pro, "call_path")$process, "</h3>",
                "<span>Call path:",
                paste(unlist(attr(pro, "call_path")), collapse = " > "),
                "</span><br>",
                '<div style="clear: both; float: left; width: 200px;">',
                "<h4>Occurrence data</h4>",
                "<span>Number of rows:", nrows, "</span><br>",
                "<span>Min Latitude:", minLat, "</span><br>",
                "<span>Max Latitude:", maxLat, "</span><br>",
                "<span>Min Longitude:", minLong, "</span><br>",
                "<span>Max Longitude:", maxLong, "</span>",
                "</div>"
  )
  
  HTML <- paste(
    HTML,
    "<h4>Preview</h4>",
    htmlTable(as.data.frame(head(df, 50)), maxHeight = 200)
  )

  Nlayers <- length(names(ras))
  minLat <- extent(ras)@ymin
  maxLat <- extent(ras)@ymax
  minLong <- extent(ras)@xmin
  maxLong <- extent(ras)@xmax

  HTML <- paste(
    HTML,
    '<div style="clear: both; float: left; width: 200px;">',
    "<h4>Raster</h4>",
    "<span>Number of layers:", Nlayers, "</span><br>",
    "<span>Min Latitude:", minLat, "</span><br>",
    "<span>Max Latitude:", maxLat, "</span><br>",
    "<span>Min Longitude:", minLong, "</span><br>",
    "<span>Max Longitude:", maxLong, "</span>",
    "</div>"
  )

  layersFromStack <- function(x) {
    nam <- names(ras[[x]])
    dat <- summary(ras[[x]]@data@values)
    if (!"NA's" %in% names(dat)) dat["NA's"] <- 0
    temp_frame <- data.frame(temp = as.numeric(dat), row.names = names(dat))
    colnames(temp_frame) <- nam
    return(temp_frame)
  }

  tabs <- lapply(names(ras), FUN = layersFromStack)

  summaryTab <- do.call(cbind, tabs)

  HTML <- paste(HTML, htmlTable(summaryTab))

  return(HTML)
}

modelSummary <- function(mod) {
  if (is.null(mod)) {
    return("<p>No output was returned from the process module</p>")
  } else if (!is.null(names(mod))) {
    html <- modHTML(mod)
  } else {
    listHTML <- lapply(mod, modHTML)

    paste(unlist(listHTML), collapse = "")
  }
}

modHTML <- function(mod) {
  df <- mod$data
  prediction_code <- paste(capture.output(cat(mod$model$code)),
                           collapse = "<br>")
  model_summary <- paste(capture.output(print(mod$model$model)),
                         collapse = "<br>")

  nrows <- nrow(df)
  minLat <- round(min(df$latitude), 3)
  maxLat <- round(max(df$latitude), 3)
  minLong <- round(min(df$longitude), 3)
  maxLong <- round(max(df$longitude), 3)

  HTML <- paste('<h3 style="clear: both;">Module:',
                attr(mod, "call_path")$model, "</h3>",
                "<span>Call path:",
                paste(unlist(attr(mod, "call_path")), collapse = " > "),
                "</span><br>",
                '<div style="clear: both; float: left; width: 200px;">',
                "<h4>Occurrence data</h4>",
                "<span>Number of rows:", nrows, "</span><br>",
                "<span>Min Latitude:", minLat, "</span><br>",
                "<span>Max Latitude:", maxLat, "</span><br>",
                "<span>Min Longitude:", minLong, "</span><br>",
                "<span>Max Longitude:", maxLong, "</span>",
                "</div>"
  )
  
  HTML <- paste(
    HTML,
    "<h4>Preview</h4>",
    htmlTable(as.data.frame(head(df, 50)), maxHeight = 200)
  )

  HTML <- paste(
    HTML,
    '<div style="clear: both; float: left; width: 600px;">',
    "<p><h4>Model summary:</h4>", model_summary, "</p><br>",
    "<p><h4>Prediction code:</h4>", prediction_code, "</p><br>",
    "</div>"
  )

  return(HTML)
}

outputSummary <- function(out) {
  if (is.null(out)) {
    return("<p>No output was returned from the output module</p>")
  } else {
    listHTML <- lapply(out, outHTML)

    paste(unlist(listHTML), collapse = "")
  }
}

#' @importFrom grDevices dev.off png
#' @importFrom utils methods

outHTML <- function(out) {
  HTML <- paste(
    '<h3 style="clear: both;">Module:',
    attr(out, "call_path")$output,
    "</h3>",
    "<span>Call path:",
    paste(unlist(attr(out, "call_path")), collapse = " > "),
    "</span><br>"
  )

  if ("plot" %in% suppressWarnings({
    attr(methods(class = class(out)), "info")$generic
  })) {
    pngtemp <- tempfile(fileext = ".png")
    png(filename = pngtemp)
    plot(out)
    dev.off()

    HTML <- paste(
      HTML,
      '<h4 style="clear: both;">Plot</h4>',
      '<img style="clear: both;"',
      paste0('src="', basename(pngtemp), '"'),
      'alt="Covariate raster">'
    )
  } else if (inherits(out, what = "data.frame")) {
    HTML <- paste(
      HTML,
      "<h4>Preview</h4>",
      htmlTable(as.data.frame(head(out, 50)), maxHeight = 200)
    )
  } else if ("summary" %in% suppressWarnings({
    attr(methods(class = class(out)), "info")$generic
  })) {
    HTML <- paste(
      HTML,
      "<h4>Summary</h4>",
      paste(capture.output(summary(out)), collapse = "<br>")
    )
  } else {
    HTML <- paste(
      HTML,
      "<h4>Print of output</h4>",
      paste(capture.output(print(out)), collapse = "<br>")
    )
  }

  return(HTML)
}
