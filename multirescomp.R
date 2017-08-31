##' iNZight Multiple Response Module
##'
##' Opens a UI for visualising multiple-response data.
##' That is, the question form: "Select all that apply".
##' Imagine checking checkboxes (as opposed to a radio button).
##'
##' @title iNZight Multiple Response Module
##'
##' @author Eric Lim
##'
##' @import iNZightMR
##'
##' @export iNZightMultiRes
##' @exportClass iNZightMultiRes
iNZightMultiRes <- setRefClass(
    "iNZightMultiRes",
    fields = list(
        GUI        = "ANY",
        mainGrp    = "ANY",
        activeData = "data.frame",
        vars       = "character",
        binaryVar  = "numeric",
        mid        = "ANY",
        gtab       = "ANY",
        mrObject   = "ANY",
        byMRObject = "ANY",
        plotSet    = "ANY",
        objName    = "character",
        guessName  = "logical"
    ),

    methods = list(
        initialize = function(GUI) {

            initFields(GUI = GUI, plotSet = list(), objName = "response", guessName = TRUE,
                       mrObject = NULL)
            activeData <<- GUI$getActiveData()


            # ==========
            # top panel
            # ==========
            binaryVar <<- getVars(activeData)

            if (length(binaryVar) == 0) {
                gmessage(
                    "Unable to find any binary variables. Code any variables as: ['yes', 'no'] or [0,1] to use this module.",
                    icon = "error", title = "No Binary Variables", parent = GUI$win)
                return(NULL)
            }


            GUI$initializeModuleWindow(.self)
            mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow, expand = TRUE)
            mainGrp$set_borderwidth(5)

            ## --- Plot Toolbar
            aboutBtn <- gimage(stock.id = "about", size = "button")
            addHandlerClicked(aboutBtn, function(h, ...) {

                                  wAb <- gwindow(parent = GUI$win, width = 400, height = 480,
                                                 title = "iNZight Maps Module")

                                  gAb <- gvbox(container = wAb, spacing = 10)
                                  addSpace(gAb, 10)
                                  labAb <- glabel("About the iNZight Maps Module")
                                  font(labAb) <- list(weight = "bold", size = 12)
                                  add(gAb, labAb, anchor = c(0, 0))

                                  aboutText <-
                                      paste("\n\nThe iNZight Multiple Response module allows you to explore data",
                                            "from questions in which respondents can select multiple answers a single",
                                            "question. For example: 'What colours do you like?', and a range of colours",
                                            "are present (for example as checkboxes) to select.",
                                            "\n\nThe data will then be in a form with a column for each option of the question,",
                                            "for example: 'colourred', 'colourblue', 'colourgree', etc.",
                                            "The data is therefore of the form 'yes' or 'no', or in mathematical terms,",
                                            "0 or 1.",
                                            "\n\nThe iNZight Multiple Response module allows you to select related variables ",
                                            "(i.e., all of the 'colour' variables) and investigate the proportion of 'yes's.",
                                            "You can select grouping variables to see how the responses are affected by",
                                            "other covariates, for example gender or age.",
                                            "\n\nThis module is fairly new, and so there may be some issues with it that",
                                            "we aren't aware of. Please report these to us so we can solve them:\n")
                                  txtAb <- gtext(text = aboutText, width = 380, height = NULL)
                                  add(gAb, txtAb, expand = TRUE)

                                  lab <- gbutton("Contact iNZight Support")
                                  font(lab) <- list(color = "navy", weight = "bold")
                                  addHandlerClicked(lab, function(h, ...)
                                      browseURL("https://stat.auckland.ac.nz/~wild/iNZight/support/contact"))
                                  add(gAb, lab, expand = FALSE, fill = FALSE, anchor = c(0, 0))


                                  cls <- gbutton("Close", handler = function(h, ...) dispose(wAb))
                                  add(gAb, cls, anchor = c(0, 1))

                              })
            GUI$plotToolbar$update(NULL, refresh = "updatePlot", extra = list(aboutBtn))

            addSpace(mainGrp, 15)

            lbl1 <- glabel("Multiple Response Module")
            font(lbl1) <- list(weight = "bold",
                               family = "normal",
                               size   = 12)
            add(mainGrp, lbl1, anchor = c(0, 0))
            addSpace(mainGrp, 20)

            top <- gvbox(container = mainGrp)
            mid <<- glayout(container = mainGrp, expand = FALSE)
            addSpring(mainGrp)
            bot <- ggroup(container = mainGrp)

            lab <- glabel("Select related variables:")
            font(lab) <- list(weight = "bold", size = 11)
            add(top, lab, anchor = c(-1, -1))


            vars <<- names(activeData)
            gtab <<- gtable(vars[binaryVar], multiple = TRUE, container = top)
            names(gtab) <<- "VARIABLES"
            size(gtab)  <<- c(-1, 350)

            top.timer <- NULL
            ## For some reason this is firing off twice when selection changes before timer ends ...
            addHandlerSelectionChanged(gtab, handler = function(h, ...) {
                if (!is.null(top.timer))
                    top.timer$stop_timer()
                top.timer <- gtimer(500, function(...) {
                    if (length(svalue(gtab)) >= 2) {
                        visible(G1clearbtn) <- visible(G1box) <- enabled(G1box) <- TRUE
                        visible(G2clearbtn) <- visible(G2box) <- enabled(G2box) <- svalue(G1box, index = TRUE) != 1
                        enabled(sumButton) = TRUE
                        enabled(comButton) = TRUE

                        setMRobj()
                    } else {
                        visible(G1clearbtn) <- visible(G1box) <- enabled(G1box) <- FALSE
                        visible(G2clearbtn) <- visible(G2box) <- enabled(G2box) <- FALSE
                        enabled(sumButton) = FALSE
                        enabled(comButton) = FALSE

                        setMRobj()
                    }
                }, one.shot = TRUE)
            })


            ## summary button
            sumButton = gbutton("Summary", handler = function(h,...) {
                s1 = svalue(G1box, index = TRUE)
                s2 = svalue(G2box, index = TRUE)
                if (s1 == 1) {
                    # summaryWindow(capture.output(summary(mroPara(mrObject))), mode = 1)
                    txt = capture.output(summary(iNZightMR::mroPara(mrObject)))
                    summaryWindow(txt, mode = 1)
                } else if (s1 != 1 & s2 == 1) {
                    txt = capture.output(summary(byMRObject, "within"))
                    summaryWindow(txt, mode = 2)
                } else if (s1 != 1 & s2 != 1) {
                    txt = capture.output(summary(byMRObject, "between"))
                    summaryWindow(txt, mode = 3)
                }
            })
            enabled(sumButton) = FALSE

            ## combinations
            comButton = gbutton("Combinations", handler = function(h,...) {
                s1 = svalue(G1box, index = TRUE)
                s2 = svalue(G2box, index = TRUE)
                if (s1 == 1) {
                    summaryWindow(capture.output(iNZightMR::plotcombn(mrObject)), mode = 3)
                } else if (s1 != 1 & s2 == 1) {
                    gmessage("Not yet supported")
                } else if (s1 != 1 & s2 != 1) {
                    gmessage("Not yet supported")
                }
            })
            enabled(comButton) = FALSE

            btnGrp <- ggroup(container = top)
            add(btnGrp, sumButton, expand = TRUE, fill = TRUE)
            add(btnGrp, comButton, expand = TRUE, fill = TRUE)


            # =============
            # mid panel
            # =============

            ## --------------------------------------------------  SLIDERS
            G1box <- gcombobox(c("Select Subset Variable 1", vars))
            G2box <- gcombobox(c("Select Subset Variable 2", vars))

            mid[1, 1:5, anchor = c(0, 0), expand = TRUE] <<- G1box
            mid[3, 1:5, anchor = c(0, 0), expand = TRUE] <<- G2box

            ## -- Grouping Variable 1
            G1clearbtn <- gbutton("",
                                  handler = function(h,...) {
                                      svalue(G1box, index = TRUE) <- 1
                                      ## change handler will handle the rest
                                  })
            G1clearbtn$set_icon("Cancel")
            mid[1, 7, anchor = c(0, 0)] <<- G1clearbtn

            ## -- Grouping Variable 2
            G2clearbtn <- gbutton("",
                                  handler = function(h,...) {
                                      svalue(G2box, index = TRUE) <- 1
                                  })
            G2clearbtn$set_icon("Cancel")
            mid[3, 7, anchor = c(0, 0)] <<- G2clearbtn

            ## --- check box for Side-by-side Variable 1
            sideChk = gcheckbox("Display subset variable 1 Side-by-side")
            mid[5, 1:5, anchor = c(-1, 0), expand = TRUE] <<- sideChk
            visible(sideChk) <- enabled(sideChk) <- svalue(G1box, index = TRUE) > 1 & svalue(G2box, index = TRUE) > 1
            addHandlerChanged(sideChk, function(h, ...) {
                                  changePlotSettings(list(sidebyside = svalue(sideChk)))
                              })

            ## --- enable/disable appropriately
            visible(G1clearbtn) <- visible(G1box) <- enabled(G1box) <-
                length(svalue(gtab, index = TRUE)) > 1
            visible(G2clearbtn) <- visible(G2box) <- enabled(G2box) <-
                length(svalue(gtab, index = TRUE)) > 1 && svalue(G1box, index = TRUE) != 1

            ## slider 1
            addHandlerChanged(
                G1box,
                handler = function(h, ...) {
                    if (svalue(G1box) == svalue(G2box)) {
                        svalue(G1box, index = TRUE) <- 1
                        gmessage("You cannot use the same variable in both subsetting slots.",
                                 parent = GUI$win)
                    } else {
                        deleteSlider(pos = 2)
                        if (svalue(G1box, index = TRUE) > 1) {
                            val <- svalue(G1box)
                            createSlider(pos = 2, val)
                            changePlotSettings(list(
                                g1 = svalue(G1box),
                                g1.level = "_MULTI",
                                varnames = list(
                                    g1 = val)
                                ))
                        } else {
                            changePlotSettings(list(g1 = NULL,
                                                    g1.level = NULL,
                                                    varnames = list(
                                                        g1 = NULL)
                                                    ), reset = TRUE)
                        }
                    }

                    s1 <- svalue(G1box, index = TRUE) - 1
                    if (s1 == 0) {
                        visible(G2clearbtn) <-visible(G2box) <- enabled(G2box) <- FALSE
                        visible(sideChk) <- enabled(sideChk) <- FALSE
                        svalue(G2box, index = TRUE) <- 1
                        enabled(comButton) <- TRUE
                    } else {
                        visible(G2clearbtn) <- visible(G2box) <- enabled(G2box) <- TRUE
                        visible(sideChk) <- enabled(sideChk) <- svalue(G2box, index = TRUE) > 1
                        enabled(comButton) <- FALSE
                    }

                })

            ## slider 2
            addHandlerChanged(
                G2box,
                handler = function(h, ...) {
                    if (svalue(G2box) == svalue(G1box)) {
                        svalue(G2box, index = TRUE) <- 1
                        gmessage("You cannot use the same variable in both subsetting slots.",
                                 parent = GUI$win)
                    } else {
                        deleteSlider(pos = 4)
                        if (svalue(G2box, index = TRUE) > 1) {
                            val <- svalue(G2box)
                            createSlider(pos = 4, val)
                            changePlotSettings(list(
                                g2 = svalue(G2box),
                                g2.level = "_ALL",
                                varnames = list(
                                    g2 = val)
                                ))
                        } else {
                            changePlotSettings(list(g2 = NULL,
                                                    g2.level = NULL,
                                                    varnames = list(
                                                        g2 = NULL)
                                                    ), reset = TRUE)
                        }
                    }

                    s2 <- svalue(G2box, index = TRUE) - 1
                    if (s2 == 0) {
                        visible(sideChk) <- enabled(sideChk) <- FALSE
                        enabled(comButton) <- svalue(G1box, index = TRUE) == 1
                    } else {
                        visible(sideChk) <- enabled(sideChk) <- svalue(G1box, index = TRUE) > 1
                        enabled(comButton) <- FALSE
                    }
                })


            ## --- Buttons at bottom of window - SUMMARY | COMBINATIONS || HELP | HOME



            helpButton <- gbutton("Help",
                                  handler = function(h, ...) {
                                      browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/?topic=multiple_response")
                                  })
            homeButton <- gbutton("Home",
                                handler = function(h, ...) {
                                    ## delete the module window
                                    delete(GUI$leftMain, GUI$leftMain$children[[2]])
                                    ## display the default view (data, variable, etc.)
                                    GUI$plotToolbar$restore()
                                    visible(GUI$gp1) <<- TRUE
                                })

            add(bot, helpButton, expand = TRUE, fill = TRUE)
            add(bot, homeButton, expand = TRUE, fill = TRUE)


            visible(GUI$moduleWindow) <<- TRUE

        },

        ## isBinary() checks for a single vector.
        isBinary = function(x) {
            ## NAs are ignored as they are handled by MR
            tab = table(x, useNA = "no")[table(x)!=0]
            n   = length(names(tab))
            ## if not binary, return FALSE
            if (n != 2) { return(FALSE) }
            ## regular expressions for "yes, no, 0, 1, true, false"
            re1 = "([Yy][Ee][Ss])|([Nn][Oo])|([Yy])|([Nn])"
            re2 = "(0)|(1)"
            re3 = "([Tt][Rr][Uu][Ee])|([Ff][Aa][Ll][Ss][Ee])|([Tt])|([Ff])"
            re  = paste(re1, re2, re3, sep = "|")
            ## do those patterns match?
            l = grepl(re, names(tab))
            ## do BOTH binary values match the patterns?
            return(all(l))
        },
        ## getVars() checks for every variable in data.
        getVars = function(data) {
            which(apply(data, 2, function(x) isBinary(x)))
        },
        createSlider = function(pos, dropdata) {
            ## not working yet ...
            return(NULL)

            ## make sure there is no slider at the pos
            deleteSlider(pos)

            ## create a ggroup for the slider at the specified
            ## pos in the glayout
            tbl <- mid
            tbl[pos, 1:5, expand = TRUE] <- (hzGrp <- ggroup(fill = "x"))

            sliderGrp <- ggroup(horizontal = FALSE)

            ## build the level names that are used for the slider
            grpData <- activeData[dropdata][[1]]
            grpData <- iNZightPlots:::convert.to.factor(grpData)
            if (pos == 2)
                lev <- c("_MULTI", levels(grpData))
            else
                lev <- c("_ALL", levels(grpData), "_MULTI")
            lev <- factor(lev, levels = lev)
            slider <- gslider(from = lev,
                              value = 1)
            add(sliderGrp, slider, expand = FALSE)
            if (pos == 2)
                grp = "g1"
            else
                grp = "g2"
            ## update the plot settings whenever the slider changes
            addHandlerChanged(slider, handler = function(h, ...) {
                                  lbl <- paste(grp, "level", sep = ".")
                                  changePlotSettings(
                                      structure(list(
                                          as.character(svalue(h$obj))
                                          ), .Names = lbl)
                                      )
                          })
            lbl <- levels(grpData)
            ## if the level names are too long, replace them with nr
            if (sum(nchar(lbl)) > 42)
                lbl <- 1:length(lbl)
            ## add * or _ to beginning of labels
            if (pos == 2)
                lbl <- c("_MULTI", lbl)
            else
                lbl <- c("_ALL", lbl, "_MULTI")
            ## only add label if it is short enough
            if (sum(nchar(lbl)) + 3 * length(lbl) < 50)
                add(sliderGrp, glabel(paste(lbl, collapse = "   ")))

            ## Play button
            ## playBtn <- gbutton("Play", expand = FALSE,
            ##                 handler = function(h, ...) {
            ##                     oldSet <- GUI$getActiveDoc()$getSettings()
            ##                     for (i in 1:length(levels(grpData))) {
            ##                         changePlotSettings(
            ##                             structure(list(i),
            ##                                       .Names = paste(
            ##                                           grp,
            ##                                           "level",
            ##                                           sep = ".")
            ##                                       )
            ##                             )
            ##                       # This effectively freezes the R session,
            ##                       # and therefore iNZight --- so increase with
            ##                       # discression!!!!!
            ##                         Sys.sleep(0.6)
            ##                     }
            ##                     changePlotSettings(oldSet)
            ##                 })
            add(hzGrp, sliderGrp, expand = TRUE)

            ## tbl[pos, 7, anchor = c(0, 0), expand = FALSE] <- playBtn

        },
        deleteSlider = function(pos) {
            ## get the child that is at the specified positions
            childPos <- which(sapply(mid$child_positions,
                                     function(x) x$x == pos))
            while(length(childPos) > 0) {
                ##childPos <- names(ctrlGp$children[[1]]$child_positions)[[childPos]]
                ## delete all the current children of sliderGrp
                try({
                    mid$remove_child(
                        mid$child_positions[[childPos[1]]]$child)
                    childPos <- which(sapply(mid$child_positions,
                                             function(x) x$x == pos))
                }, silent = TRUE)
            }
        },
        changePlotSettings = function(set, reset = FALSE) {
            plotSet <<- iNZight:::modifyList(plotSet, set, keep.null = FALSE)

            setMRobj()
        },
        setMRobj = function() {
            ## Get variables
            responseID <- svalue(gtab, index = TRUE)
            if (length(responseID) == 1) {
                mrObject <<- NULL
                updatePlot()

                return(NULL)
            }

            responseVars <- binaryVar[responseID]

            frm <- as.formula(paste(objName, "~", paste(vars[responseVars], collapse = " + ")))

            mrObject <<- iNZightMR::iNZightMR(frm, data = activeData, Labels = substrsplit)
            if (mrObject$Labels$Commonstr != objName && guessName) {
                if (!(objName == "response" && mrObject$Labels$Commonstr == "")) {
                    objName <<- ifelse(mrObject$Labels$Commonstr == "", "response", mrObject$Labels$Commonstr)
                    setMRobj()
                    return(NULL)
                }
            }

            updatePlot()
        },
        ## create an MR object and plot it
        updatePlot = function() {

            if (is.null(mrObject)) return(NULL)

            if (is.null(plotSet$g1)) {
                mro <- iNZightMR::mroPara(mrObject)
            } else if (is.null(plotSet$g2)) {
                by.formula = paste("~", plotSet$g1)
                mro <- byMRObject <<- iNZightMR::byMRO(mrObject, by.formula, mroPara)
            } else {
                by.formula = paste("~", paste(plotSet$g1, "+", plotSet$g2))
                mro <- byMRObject <<- iNZightMR::byMRO(mrObject, by.formula, mroPara)
                if (!is.null(plotSet$sidebyside))
                    if (plotSet$sidebyside)
                        mro <- iNZightMR::between(mro)
            }

            iNZightMR::barplotMR(mro)

        },
        ## summary window
        summaryWindow = function(text, mode) {
            if (mode == 1) {
                # w = 500
                # h = 350
                w = 680; h = 350
            } else if (mode == 2) {
                # w = 670
                # h = 500
                w = 680; h = 350
            } else if (mode == 3) {
                # w = 300
                # h = 200
                w = 680; h = 350
            }
            text = paste0(text, collapse = "\n")
            win  = gwindow("Summary Output", width = w, height = h)
            gtext(text, font.attr = list(family = "monospace"), container = win)
        }
    )
)