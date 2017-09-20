#' @importFrom graphics strwidth

GetCex <- function(string, width = 9) {
  # given a string (character vector of length one),
  # and a target width on screen, in user coordinates
  # return a value of cex that when passed to text will
  # render with the right width
  cex <- width / strwidth(string, cex = 1)
  return(cex)
}

#' @importFrom graphics strwidth segments

ModuleLabels <- function(colr, IsChain, IsList) {
  ### ___ function for writing the module labels

  # basic defaults for diagram layout.
  
  # lower corner for boxes (Xaxis) (occurrence, covariate, process, model,
  # output)
  x1 <- c(10, 30, 50, 70, 90) 
  x2 <- c(20, 40, 60, 80, 100) # top corner for boxes (Xaxis)
  xG <- x2[1] - x1[1] # gap between boxes (Xaxis)
  yM <- 50 # Y val for middle line
  yL <- 105 # Y val for title labels, e.g. 'x.module'
  yH <- 10 # height of boxes (Y axis)
  yG <- 5 # gap betwen boxes (Yaxis)
  yLG <- 0 # gap between lines (Yaxis)
  # labels options
  moduleLabels <- c(
    "occurrence.module", "covariate.module", "process.module",
    "model.module", "output.module"
  )

  # get cex for module labels
  moduleLabelsCex <- GetCex(
    moduleLabels[which.max(strwidth(moduleLabels))],
    width = 15
  )
  # fonts and text
  fontModules <- 2 ### module labels font size
  fontTitle <- 3 ### main title font size
  cexTitle <- 1.5 ### main title font size
  # mccin function
  segments(x1[1], yL + 8, x2[5] + yG * 0.5, yL + 8, col = colr)
  text(
    x1[1:5], yL,
    moduleLabels,
    col = colr,
    adj = 0,
    font = fontModules,
    cex = moduleLabelsCex
  )

  for (k in 1:5) {
    segments(
      x1[k], yL - 5,
      x2[k] + yG * 0.5, yL - 5,
      col = colr
    )

    if (IsChain[k])
      text(x1[k], yL - 10, "Chain", col = colr, adj = 0, font = 3)

    if (IsList[k])
      text(x1[k], yL - 10, "List", col = colr, adj = 0, font = 3)
  }

  segments(
    x1[1], -10,
    x2[5] + yG * 0.5, -10,
    col = colr
  )
} # END ModuleLabels


#' @importFrom graphics rect segments strwidth
Boxed2 <- function(NoOfModules, InModuleList, IsList, IsChain, ModuleNames) {
  ### ___ function for writing the boxes

  # get cex for module names
  ModuleNamesCex <- GetCex(
    ModuleNames[[which.max(strwidth(ModuleNames))]],
    width = 9
  )

  # reverse the order of module names for chains and lists
  # (Nick added this post-hoc)

  # keep track of the number of names so far
  counter <- 0
  # loop through module types
  for (i in 1:5) {

    # if it's a chain or list
    if (NoOfModules[i] > 1) {

      # get an index for the module names
      idx <- counter + 1:NoOfModules[i]

      # flip the names
      ModuleNames[idx] <- rev(ModuleNames[idx])

      # also flip the InModuleList
      InModuleList[idx] <- rev(InModuleList[idx])
      rm(idx)
    }

    # add to number of modules seen
    counter <- counter + NoOfModules[i]
  }

  # clean up as we don't know what Greg might be using
  # rm(idx)
  rm(counter)
  rm(i)

  # basic defaults for diagram layout.
  
  # lower corner for boxes (Xaxis) (occurrence, covariate, process, model,
  # output)
  x1 <- c(10, 30, 50, 70, 90) 
  x2 <- c(20, 40, 60, 80, 100) # top corner for boxes (Xaxis)
  xG <- x2[1] - x1[1] # gap between boxes (Xaxis)
  yM <- 50 # Y val for middle line
  yL <- 105 # Y val for title labels, e.g. 'x.module'
  yH <- 10 # height of boxes (Y axis)
  yG <- 5 # gap betwen boxes (Yaxis)
  yLG <- 0 # gap between lines (Yaxis)
  # labels options
  moduleLabels <- c(
    "occurrence.module", "covariate.module", "process.module",
    "model.module", "output.module"
  )
  # fonts and text
  fontModules <- 2 ### module labels font size
  fontTitle <- 3 ### main title font size
  cexTitle <- 1.5 ### main title font size
  # graphics formatting
  lW <- 3 ### edges line widths
  clrs <- c("deepskyblue2", "tomato", "deepskyblue4", "tomato3")
  clrs1 <- c("cornsilk", "tomato", "cornsilk4", "tomato3")
  clrs2 <- c("black", NA, "black", NA)
  clrs3 <- c("cornsilk", NA, "cornsilk", NA)

  # main function
  ListAlready <- 0
  ModuleInc <- 0
  ColCode <- 1
  clrsTXT <- "black"
  wInc <- -35

  for (i in 1:5) {
    if (i != 1) {
      segments(
        x1[i] - xG * 0.66, yM,
        x1[i] - xG * 0.33, yM,
        lwd = lW, col = clrs[ColCode]
      )
    }
    yStart <- yM - (yH * 0.5 * NoOfModules[i]) -
      (yG * 0.5 * (NoOfModules[i] - 1))
    yNow <- yStart
    yLineStart <- yM - (yLG * 0.5 * (NoOfModules[i] - 1))
    yLnow <- yLineStart

    for (N in 1:NoOfModules[i]) {
      ModuleInc <- ModuleInc + 1

      if (IsList[i] && ListAlready == 1)
        ListAlready <- 2

      if (ListAlready != 2) {
        if (InModuleList[[ModuleInc]])
          ColCode <- 1
          else
          ColCode <- 2
      }

      if (IsList[i]) {
        segments(
          x1[i] - (xG * 0.33),
          yStart + 0.5 * yH,
          x1[i] - xG * 0.33,
          yStart + (0.5 * yH) + ((NoOfModules[i] - 1) * (yH + yG)),
          lwd = lW, col = clrs[ColCode]
        )

        segments(
          x2[i] + (xG * 0.33),
          yStart + 0.5 * yH,
          x2[i] + xG * 0.33,
          yStart + (0.5 * yH) + ((NoOfModules[i] - 1) * (yH + yG)),
          lwd = lW, col = clrs[ColCode]
        )

        segments(
          x1[i] - xG * 0.33, yNow + 0.5 * yH,
          x1[i], yNow + 0.5 * yH,
          lwd = lW, col = clrs[ColCode]
        )

        segments(
          x2[i], yNow + 0.5 * yH,
          x2[i] + xG * 0.33, yNow + 0.5 * yH,
          lwd = lW, col = clrs[ColCode]
        )
      } else {
        if (IsChain[i]) {
          if (i != 1 && N == NoOfModules[i]) {
            segments(
              x1[i] - xG * 0.33, yLnow,
              x1[i], yNow + 0.5 * yH,
              lwd = lW, col = clrs[ColCode]
            )
          }

          if (i != 5 && N == 1) {
            segments(
              x2[i], yNow + 0.5 * yH,
              x2[i] + xG * 0.33, yLnow,
              lwd = lW, col = clrs[ColCode]
            )
          }

          if (N != NoOfModules[i]) {
            segments(
              0.5 * (x1[i] + x2[i]), yNow + yH,
              0.5 * (x1[i] + x2[i]), yNow + yH + yG,
              lwd = lW, col = clrs[ColCode]
            )
          }
        } else {
          if (i != 1) {
            segments(
              x1[i] - xG * 0.33, yLnow,
              x1[i], yNow + 0.5 * yH,
              lwd = lW, col = clrs[ColCode]
            )
          }

          if (i != 5) {
            segments(
              x2[i], yNow + 0.5 * yH,
              x2[i] + xG * 0.33, yLnow,
              lwd = lW, col = clrs[ColCode]
            )
          }
        }
      }

      rect(
        x1[i], yNow,
        x2[i], yNow + yH,
        col = clrs[ColCode], border = clrs[ColCode],
        lwd = lW
      )

      text(
        x1[i], yNow + 0.5 * yH,
        ModuleNames[[ModuleInc]],
        col = clrsTXT, adj = 0,
        cex = ModuleNamesCex
      )

      if (ColCode %% 2 == 0) {
        text(
          x1[1] + (0.5 * (x2[1] - x1[1])), -18,
          "** Modules not found in the repoistory",
          col = "cornsilk4", adj = 0, cex = 0.8
        )

        rect(
          x1[1] + (0.5 * (x2[1] - x1[1])), wInc,
          x2[1] + (0.5 * (x2[1] - x1[1])), wInc + yH,
          col = clrs[ColCode], border = clrs[ColCode],
          lwd = lW
        )

        text(
          x1[1] + (0.5 * (x2[1] - x1[1])), wInc + 0.5 * yH,
          ModuleNames[[ModuleInc]],
          col = clrsTXT, adj = 0,
          cex = 0.75
        )

        wInc <- wInc - (yH + yG)
      }

      yNow <- yNow + yG + yH
      yLnow <- yLnow + yLG
    } # for N

    if (IsList[i] && ListAlready == 0)
      ListAlready <- 1
  } # for i
} # end function 'Boxed'


CallLister <- function(callList) {
  ### ___ Function to retrieve information from the call.list

  # initialise the lists
  noOfModules <- rep(0, 5)
  inModuleList <- list()
  moduleNames <- list()
  moduleIndex <- c()
  isList <- rep(FALSE, 5)
  isChain <- rep(FALSE, 5)

  # get list of modules in repo
  repoModuleList <- unlist(
    GetModuleList(renew = TRUE),
    use.names = FALSE
  )

  # loop through the module types and
  for (j in 1:5) {
    noOfModules[j] <- length(callList[[j]])

    # check if a list or chain and add to isList/isChain
    if (length(callList [[j]]) > 1) {
      if (is.null(attr(callList[[j]], "chain")))
        isList[j] <- TRUE
        else
        isChain[j] <- TRUE
    }

    for (k in seq_along(callList[[j]])) {
      # add module name to names list and add index (i.e. order)
      modNom <- callList [[j]][[k]]$module
      moduleNames <- c(moduleNames, modNom)
      moduleIndex <- c(moduleIndex, j)
      modOnRepo <- as.character(modNom) %in% repoModuleList
      inModuleList <- c(inModuleList, modOnRepo)
    }
  } # end for j

  moduleIndex <- as.numeric(moduleIndex)

  list(noOfModules,
       moduleIndex,
       isChain,
       isList,
       moduleNames,
       inModuleList)
  
} # END CallLister function

#' @name plot.zoonWorkflow
#'
#' @title Plot a schematic illustration of a zoon workflow structure.
#' @description Opens a graphics device and produces a plot of the workflow
#' structure and modules used.
#'
#' @param x an object of class zoonWorkflow
#' @param \dots currently ignored
#'
#' @method plot zoonWorkflow
#' @importFrom graphics par rect
#' @export
plot.zoonWorkflow <- function(x, ...) {

  # set up new device
  omar <- par()$mar
  par(mar = c(0, 0, 0, 0))
  on.exit({
    par(mar = omar)
  })

  graphics::plot(
    -99, -99,
    xlim = c(0, 110),
    ylim = c(-100, 125),
    xlab = "", ylab = "",
    axes = FALSE
  )

  rect(
    -200, -200, 200, 200,
    col = "cornsilk1",
    border = FALSE
  )

  text(
    10,
    120,
    "My Workflow",
    font = 3,
    cex = 1.5,
    col = "cornsilk4",
    adj = 0
  )

  hg <- CallLister(callList = x$call.list)

  NoOfModules <- hg[[1]]
  ModuleIndex <- hg[[2]]
  IsChain <- hg[[3]]
  IsList <- hg[[4]]
  ModuleNames <- hg[[5]]
  InModuleList <- hg[[6]]

  ModuleLabels(
    colr = "wheat4",
    IsChain,
    IsList
  )

  Boxed2(
    NoOfModules = NoOfModules,
    InModuleList = InModuleList,
    IsList = IsList,
    IsChain = IsChain,
    ModuleNames = ModuleNames
  )
}
