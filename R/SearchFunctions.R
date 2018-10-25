#' GetModuleList
#'
#' Get a list of all the modules available on the github repo.
#'
#' @return A list with all module names.
#' @name GetModuleList
#' @param type The subset of zoon modules you want to return. Defaults to 'all', but you can
#' select any of the zoon workflow steps: 'occurrence', 'covariate', 'process',
#' 'model', or 'output'.
#' @param renew Download from github even if we already have a module list.
#' @details This function will only work on a platform that supports the
#' method 'libcurl' in the function url. This can be tested using the function
#' \code{capabilities} (see example).
#'
#' @export
#' @examples
#' # GetModuleList requires libcurl to be supported
#' if(capabilities('libcurl')) GetModuleList()
GetModuleList <- function(type = c(
                          "all", "occurrence", "covariate", "process",
                          "model", "output"
                        ),
                        renew = FALSE) {

  # Define desired subset
  subset <- match.arg(type)

  if (subset == "all") { # return all available modules under sub-headings

    # If we've already downloaded a module list, print that.
    # Otherwise download a list from github
    # If renew is TRUE download from github even if we have a list already.
    if (exists("moduleList", envir = .zoonHidden) & !renew) {
      moduleNames <- .zoonHidden$moduleList
      return(moduleNames)
    } else {

      # modules we know belong to each type (doesn't matter which they are,
      # but these should be fairly stable)
      canaries <- c(
        occurrence = "SpOcc",
        covariate = "Bioclim",
        process = "NoProcess",
        model = "LogisticRegression",
        output = "InteractiveMap"
      )

      # get URLs for these modules
      canary_urls <- helpURL <- paste0(
        "https://raw.githubusercontent.com/zoonproject/modules/master/man/",
        canaries,
        ".Rd"
      )

      # empty list for results
      moduleNames <- list()

      for (i in 1:5) {

        # grab the rd file
        con <- url(canary_urls[i], method = "libcurl")
        rd_i <- readLines(con)
        close(con)

        # get the expected module type
        type <- names(canaries)[i]

        # find the starting line
        start_line <- grep(sprintf("^Other %s: ", type), rd_i)

        # find all section -closing parentheses
        end_lines <- grep("^}$", rd_i)

        # find the end of this section
        end_line <- min(end_lines[end_lines > start_line])

        # grab the required lines and combine
        text <- paste(
          rd_i[start_line:end_line],
          collapse = ""
        )

        # remove formatting we don't want
        start_text <- sprintf("^Other %s: ", type)
        text <- gsub(start_text, "", text)
        text <- gsub("\\\\code\\{\\\\link\\{", "", text)
        text <- gsub("\\}", "", text)

        # split into vector
        text <- strsplit(text, c(","))[[1]]

        # add the target module in
        text <- sort(c(canaries[i], text))

        # remove any names
        names(text) <- NULL

        # remove spaces
        text <- gsub(" ", "", text)

        # add to results
        moduleNames[[i]] <- text
      }

      names(moduleNames) <- names(canaries)

      .zoonHidden$moduleList <- moduleNames

      return(moduleNames)
    }
  } else { # return only subset of modules

    # If we've already downloaded a module list, print that.
    # Otherwise download a list from github
    # If renew is TRUE download from github even if we have a list already.
    if (exists("moduleList", envir = .zoonHidden) & !renew) {
      moduleNames <- .zoonHidden$moduleList
      tmp <- sprintf("moduleNames$%s", subset)
      return(eval(parse(text = tmp)))
    } else {

      # modules we know belong to each type (doesn't matter which they are,
      # but these should be fairly stable)
      canaries <- c(
        occurrence = "SpOcc",
        covariate = "Bioclim",
        process = "NoProcess",
        model = "LogisticRegression",
        output = "InteractiveMap"
      )

      # get URLs for these modules
      canary_urls <- helpURL <- paste0(
        "https://raw.githubusercontent.com/zoonproject/modules/master/man/",
        canaries,
        ".Rd"
      )

      # empty list for results
      moduleNames <- list()

      for (i in 1:5) {

        # grab the rd file
        con <- url(canary_urls[i], method = "libcurl")
        rd_i <- readLines(con)
        close(con)

        # get the expected module type
        type <- names(canaries)[i]

        # find the starting line
        start_line <- grep(sprintf("^Other %s: ", type), rd_i)

        # find all section -closing parentheses
        end_lines <- grep("^}$", rd_i)

        # find the end of this section
        end_line <- min(end_lines[end_lines > start_line])

        # grab the required lines and combine
        text <- paste(
          rd_i[start_line:end_line],
          collapse = ""
        )

        # remove formatting we don't want
        start_text <- sprintf("^Other %s: ", type)
        text <- gsub(start_text, "", text)
        text <- gsub("\\\\code\\{\\\\link\\{", "", text)
        text <- gsub("\\}", "", text)

        # split into vector
        text <- strsplit(text, c(","))[[1]]

        # add the target module in
        text <- sort(c(canaries[i], text))

        # remove any names
        names(text) <- NULL

        # remove spaces
        text <- gsub(" ", "", text)

        # add to results
        moduleNames[[i]] <- text
      }

      names(moduleNames) <- names(canaries)

      .zoonHidden$moduleList <- moduleNames

      tmp2 <- sprintf("moduleNames$%s", subset)
      return(eval(parse(text = tmp2)))
    }
  }
}

# create .zoonHidden in the package namespace
.zoonHidden <- new.env()
