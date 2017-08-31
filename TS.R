library(gWidgets2RGtk2)
library(memoise)

##' iNZight Time Series Module
##'
##' A GUI add-on for visualising and doing basic inference and prediction of time series data.
##'
##' @title iNZight Time Series Module
##'
##' @author Eric Lim
##'
##' @import iNZightTS
##'
##' @export iNZightTSMod
##' @exportClass iNZightTSMod
iNZightTSMod <- setRefClass(
    "iNZightTSMod",
    fields = list(
        GUI         = "ANY",
        mainGrp     = "ANY",
        activeData  = "data.frame",
        timeVar     = "ANY",
        timePeriod = "ANY", timeFreq = "ANY", timeStart = "ANY",
        patternType = "numeric",
        smthSlider  = "ANY", smoothness = "numeric",
        tsObj       = "ANY",
        yLab        = "ANY", xLab = "ANY",
        plottype    = "numeric",
        compare     = "numeric",
        animateBtn  = "ANY", pauseBtn = "ANY",
        recomposeBtn = "ANY", recomposeResBtn = "ANY", decomp = "ANY",
        forecastBtn = "ANY", forecasts   = "ANY"
    ),
    methods = list(
        initialize = function(GUI) {
            initFields(GUI = GUI, patternType = 1, smoothness = 10, tsObj = NULL,
                       plottype = 1, compare = 1, timeFreq = 1, timeStart = c(1, 1))

            dat = GUI$getActiveData()
            activeData <<- tsData(dat)
            timeVar <<- getTime(activeData, index = FALSE)

            GUI$initializeModuleWindow(.self)
            mainGrp <<- gvbox(spacing = 10, container = GUI$moduleWindow, expand = TRUE)
            mainGrp$set_borderwidth(5)

            ## playBtn <- iNZight:::gimagebutton(stock.id = "media-play", handler = function(h, ...) updatePlot(animate = TRUE))
            GUI$plotToolbar$update(NULL, refresh = "updatePlot")#, extra = list(playBtn))

            ################
            ###  fields  ###
            ################
            frameFont = list(weight = "bold")

            #################################
            ###  set up frame containers  ###
            #################################
            g1 = gframe("Time Information", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            g2 = gframe("Model Settings", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)
            addSpring(mainGrp)

            midGrp <- ggroup(container = mainGrp, fill = TRUE)
            g3 = gframe("Series Variables", pos = 0.5, horizontal = FALSE,
                        container = midGrp, fill = TRUE)
            g5 = gframe("Plot Type Options", pos = 0.5, horizontal = FALSE,
                        container = midGrp, fill = TRUE, expand = TRUE)

            g4 = gframe("Customize Labels", pos = 0.5, horizontal = FALSE,
                        container = mainGrp)


            g1$set_borderwidth(8)
            g2$set_borderwidth(8)
            g3$set_borderwidth(8)
            g4$set_borderwidth(8)

            g5$set_borderwidth(8)

            ## bold-faced title for the frames
            frames = getToolkitWidget(mainGrp)$getChildren()
            mainGrp$set_rgtk2_font(frames[[1]]$getChildren()[[2]], frameFont)
            mainGrp$set_rgtk2_font(frames[[2]]$getChildren()[[2]], frameFont)
            midGrp$set_rgtk2_font(getToolkitWidget(midGrp)$getChildren()[[1]]$getChildren()[[2]], frameFont)
            midGrp$set_rgtk2_font(getToolkitWidget(midGrp)$getChildren()[[2]]$getChildren()[[2]], frameFont)
            mainGrp$set_rgtk2_font(frames[[5]]$getChildren()[[2]], frameFont)

            ############
            ###  g1  ###
            ############
            ## FOR MAIN LAYOUT
            g1_layout = glayout(container = g1)
            g1_opt1   = gradio(c("Select time variable", "Provide time manually"),
                               selected = 1, horizontal = FALSE)
            g1_layout[1, 1:2, expand = TRUE] = g1_opt1

            ## FOR LAYOUT A
            g1a_layout = glayout(container = g1)
            ## g1a options

            g1a_opt1   = gcombobox(names(activeData),
                                   selected = match(timeVar, names(activeData)),
                                   handler = function(h, ...) {
                                       timeVar <<- svalue(h$obj)
                                       updatePlot()
                                   })
            ## g1a labels
            g1a_lab1   = glabel("Select time variable:")
            ## g1a layout
            g1a_layout[2, 1, expand = TRUE, anchor = c(-1, 0)] = g1a_lab1
            g1a_layout[2, 2, expand = TRUE]   = g1a_opt1

            ## FOR LAYOUT B
            g1b_layout = glayout(container = g1, spacing = 2)
            visible(g1b_layout) = FALSE
            
            ## g1b options
            ii <- 1

            lbl <- glabel("Period :")
            timePeriodList <- gcombobox(c("Year", "Week", "Day"), selected = 0,
                                        handler = function(h, ...) {
                                            timePeriod <<- svalue(h$obj)
                                            timeFreqList$set_items(c(names(freqOpts[[svalue(h$obj)]]), "Custom"))
                                            svalue(startlbl1) <- "Year"
                                            g3_opt1$invoke_change_handler()
                                        })
            g1b_layout[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            g1b_layout[ii, 2, expand = TRUE, fill = TRUE] <- timePeriodList
            ii <- ii + 1

            lbl <- glabel("Frequency* :")
            freqOpts <- list("Year" = c("Yearly (1)" = 1, "Quarterly (4)" = 4, "Monthly (12)" = 12,
                                        "Weekly (52)" = 52, "Daily (365/366)" = 365.25),
                             "Week" = c("Daily (7)" = 7, "Daily - work week (5)" = 5),
                             "Day"  = c("Hourly (24)" = 24))
            timeFreqList <- gcombobox(character(), selected = 0,
                                      handler = function(h, ...) {
                                          if (svalue(h$obj) == "Custom") {
                                              enabled(timeFreqNum) <- TRUE
                                          } else {
                                              enabled(timeFreqNum) <- FALSE
                                              svalue(timeFreqNum) <- freqOpts[[timePeriod]][svalue(h$obj)]
                                          }
                                          season.name <- svalue(h$obj)
                                          if (season.name == "Custom") {
                                              season.name <- "Season"
                                          } else {
                                              season.name <- gsub("ly$", "", strsplit(season.name, " ")[[1]][1])
                                              if (season.name == "Dai") season.name <- "Day"
                                          }
                                          svalue(startlbl2) <- season.name
                                          g3_opt1$invoke_change_handler()
                                      })
            timeFreqNum <- gspinbutton(1, 1000, by = 1, value = 1,
                                       handler = function(h, ...) {
                                           timeFreq <<- svalue(h$obj)
                                           svalue(timeStartSeason) <- min(svalue(timeStartSeason), timeFreq)
                                           if (svalue(h$obj) == 1) {
                                               enabled(timeStartSeason) <- FALSE
                                               visible(startlbl2) <- FALSE
                                           } else {
                                               enabled(timeStartSeason) <- TRUE
                                               visible(startlbl2) <- TRUE
                                           }
                                           g3_opt1$invoke_change_handler()
                                       })
            g1b_layout[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            g1b_layout[ii, 2, expand = TRUE, fill = TRUE] <- timeFreqList
            g1b_layout[ii, 3, expand = TRUE, fill = TRUE] <- timeFreqNum
            ii <- ii + 1

            lbl <- glabel("*How many observations per period?")
            font(lbl) <- list(size = 9)
            g1b_layout[ii, 2:3, anchor = c(-1, 1), expand = TRUE] <- lbl
            ii <- ii + 1

            ii <- ii + 1

            lbl <- glabel("Start date : ")
            timeStartPeriod <- gspinbutton(0, 1e5, by = 1, value = 1,
                                           handler = function(h, ...) {
                                               timeStart <<- c(svalue(h$obj), svalue(timeStartSeason))
                                               g3_opt1$invoke_change_handler()
                                           })
            timeStartSeason <- gspinbutton(0, 1e5, by = 1, value = 1,
                                           handler = function(h, ...) {
                                               if (svalue(h$obj) > timeFreq) svalue(h$obj) <- timeFreq
                                               timeStart <<- c(svalue(timeStartPeriod), svalue(h$obj))
                                               g3_opt1$invoke_change_handler()
                                           })
            g1b_layout[ii, 1, anchor = c(1, 0), expand = TRUE] <- lbl
            g1b_layout[ii, 2, expand = TRUE, fill = TRUE] <- timeStartPeriod
            g1b_layout[ii, 3, expand = TRUE, fill = TRUE] <- timeStartSeason
            ii <- ii + 1

            startlbl1 <- glabel("Period")
            font(startlbl1) <- list(size = 9)
            startlbl2 <- glabel("Season")
            font(startlbl2) <- list(size = 9)
            g1b_layout[ii, 2, anchor = c(-1, 1), expand = TRUE] <- startlbl1
            g1b_layout[ii, 3, anchor = c(-1, 1), expand = TRUE] <- startlbl2
            ii <- ii + 1

            addHandlerChanged(g1_opt1, handler = function(h,...) {
                if (svalue(h$obj, index = TRUE) == 1) {
                    visible(g1a_layout) = TRUE
                    visible(g1b_layout) = FALSE
                } else {
                    visible(g1a_layout) = FALSE
                    visible(g1b_layout) = TRUE
                }
                g3_opt1$invoke_change_handler()
            })

            ############
            ###  g2  ###
            ############
            g2_layout = glayout(container = g2, spacing = 5)
            g2_opt1   = gradio(c("Multiplicative", "Additive"), selected = patternType,
                               horizontal = TRUE,
                               handler = function(h, ...) {
                                   patternType <<- svalue(h$obj, index = TRUE)
                                   updatePlot()
                               })

            g2_layout[1, 1, anchor = c(1, 0), expand = TRUE] <- glabel("Seasonal pattern :")
            g2_layout[1, 2, expand = TRUE] = g2_opt1

            ## Smoother
            smthSlider <<- gslider(0, 100, by = 0.1, value = smoothness,
                                   handler = function(h, ...) {
                                       smoothness <<- svalue(h$obj)
                                       updatePlot()
                                   })

            g2_layout[2, 1, anchor = c(1, 0), expand = TRUE] <- glabel("Smoothness :")
            g2_layout[2, 2, fill = TRUE, expand = TRUE] <- smthSlider

            ############
            ###  g3  ###
            ############
            ## NOTE:
            ##   need to change the variable selection widget for when there
            ##   are many variables which will expand the widget.
            g3_layout = glayout(container = g3)
            g3_opt1 = gtable(names(activeData)[! names(activeData) %in% timeVar],
                             multiple = TRUE)
            size(g3_opt1) <- c(floor(size(GUI$leftMain)[1] * 0.5), 200)
            g3_layout[1, 1, anchor = c(-1, 0), expand = TRUE] <- glabel("Hold CTRL to select many")
            g3_layout[2, 1, expand = TRUE] = g3_opt1

            addHandlerSelectionChanged(g3_opt1, function(h, ...) {
                if (length(svalue(g3_opt1)) == 0) {
                    visible(novar) <- TRUE
                    return()
                }
                visible(novar) <- FALSE

                ## make dataset an iNZightTS object
                var_ind <- which(names(activeData) %in% svalue(h$obj))
                if (length(var_ind) == 1) {
                    visible(onevar) <- TRUE
                    visible(multivar) <- FALSE
                } else {
                    visible(onevar) <- FALSE
                    visible(multivar) <- TRUE
                }
                
                if (svalue(g1_opt1, TRUE) == 1) {
                    tsObj <<- iNZightTS::iNZightTS(data = activeData, var = var_ind,
                                                   time.col = which(colnames(activeData) == timeVar))
                } else {
                    tsObj <<- iNZightTS::iNZightTS(data = activeData, var = var_ind,
                                                   start = timeStart, freq = timeFreq)
                }
                updatePlot()
            })


            ############
            ###  g5  ###
            ############

            onevar <- gvbox(container = g5)
            addSpring(onevar)
            plotType <- gradio(c("Standard", "Decomposition", "Seasonal", "Forecast"), selected = plottype,
                               container = onevar, expand = TRUE,
                               handler = function(h, ...) {
                                   plottype <<- svalue(h$obj, index = TRUE)
                                   visible(animateBtn) <<- svalue(h$obj, TRUE) == 1
                                   visible(pauseBtn) <<- svalue(h$obj, TRUE) == 1
                                   visible(recomposeBtn) <<- FALSE
                                   visible(recomposeResBtn) <<- FALSE
                                   visible(forecastBtn) <<- FALSE
                                   updatePlot()
                               })

            tsenv <- new.env()
            assign("stopAnimation", FALSE, envir = tsenv)
            runAnimation <- gaction("Animate", icon = "gtk-media-play",
                                    handler = function(h, ...) {
                                        assign("stopAnimation", FALSE, envir = tsenv)
                                        enabled(animateBtn) <<- FALSE
                                        enabled(pauseBtn) <<- TRUE
                                        iNZightTS::rawplot(tsObj, multiplicative = (patternType == 1),
                                                           ylab = svalue(yLab), xlab = svalue(xLab), animate = TRUE, t = smoothness,
                                                           e = tsenv)
                                        enabled(pauseBtn) <<- FALSE
                                        enabled(animateBtn) <<- TRUE
                                    })
            pauseAnimation <- gaction("End Animation", icon = "gtk-media-stop",
                                      handler = function(h, ...) {
                                          assign("stopAnimation", TRUE, envir = tsenv)
                                      })

            animateBtn <<- gbutton(action = runAnimation, container = onevar)
            pauseBtn <<- gbutton(action = pauseAnimation, container = onevar)
            enabled(pauseBtn) <<- FALSE

            recomposeBtn <<- gbutton("Recompose", container = onevar,
                                     handler = function(h, ...) {
                                         assign("stopAnimation", FALSE, envir = tsenv)
                                         decomp <<- decompositionplot(tsObj, multiplicative = (patternType == 1),
                                                                          xlab = svalue(xLab), ylab = svalue(yLab), t = smoothness)
                                         iNZightTS::recompose(decomp, e = tsenv)
                                     })
            visible(recomposeBtn) <<- FALSE
            recomposeResBtn <<- gbutton("Recompose Result", container = onevar)
            addHandlerClicked(recomposeResBtn, function(h, ...) {
                assign("stopAnimation", TRUE, envir = tsenv)
                blockHandlers(h$obj)
                if (svalue(h$obj) == "Re-decompose") {
                    updatePlot()
                    svalue(recomposeResBtn) <<- "Recompose Result"
                } else {
                    iNZightTS::recompose(decomp, animate = FALSE)
                    svalue(recomposeResBtn) <<- "Re-decompose"
                }
                unblockHandlers(h$obj)
            })
            visible(recomposeResBtn) <<- FALSE

            forecastBtn <<- gbutton("Forecasted Values", container = onevar,
                                    handler = function(h, ...) {
                                        w <- gwindow("Time Series Forecasts", parent = GUI$win,
                                                     width = 400, height = 300)
                                        g <- gvbox(container = w)
                                        t <- gtext(text = "", container = g, expand = TRUE,
                                                   wrap = FALSE, font.attr = list(family = "monospace"))
                                        insert(t, capture.output(print(forecasts)))
                                    })
            visible(forecastBtn) <<- FALSE

            multivar <- ggroup(container = g5)
            compareChk <- gradio(c("Single graph",
                                   "Separate graphs"),
                                 checked = compare,
                                 container = multivar,
                                 handler = function(h, ...) {
                                     compare <<- svalue(h$obj, index = TRUE)
                                     updatePlot()
                                 })

            visible(onevar) <- FALSE
            visible(multivar) <- FALSE

            novar <- gvbox(container = g5)
            glabel("Select a Variable.", container = novar)
            lb <- glabel("(Hold CTRL to select multiple)", container = novar)
            font(lb) <- list(size = 8)



            ############
            ###  g4  ###
            ############
            g4_layout = glayout(container = g4)
            g4_lab1   = glabel("x-axis")
            g4_lab2   = glabel("y-axis")
            xLab <<- gedit(timeVar)
            yLab <<- gedit("")

            addHandlerKeystroke(xLab, function(h, ...) updatePlot())
            addHandlerKeystroke(yLab, function(h, ...) updatePlot())

            #size(xLab) <<- c(150, 21)
            #size(yLab) <<- c(150, 21)

            g4_layout[1, 1:2, expand = TRUE, anchor = c(-1, 0)] = g4_lab1
            g4_layout[2, 1:2, expand = TRUE, anchor = c(-1, 0)] = g4_lab2
            g4_layout[1, 3, expand = TRUE] = xLab
            g4_layout[2, 3, expand = TRUE] = yLab

            clearXlab <- iNZight:::gimagebutton(stock.id = "reset", handler = function(h, ...) {
                svalue(xLab) <<- timeVar
            })
            g4_layout[1, 4] <- clearXlab
            clearYlab <- iNZight:::gimagebutton(stock.id = "reset", handler = function(h, ...) {
                svalue(yLab) <<- ""
            })
            g4_layout[2, 4] <- clearYlab


            btmGrp <- ggroup(container = mainGrp)

            helpButton <- gbutton("Help", expand = TRUE, fill = TRUE,
                                  cont = btmGrp,
                                  handler = function(h, ...) {
                                      browseURL("https://www.stat.auckland.ac.nz/~wild/iNZight/user_guides/add_ons/?topic=time_series")
                                  })
            homeButton <- gbutton("Home", expand = TRUE, fill = TRUE,
                                  cont = btmGrp,
                                  handler = function(h, ...) {
                                      ## delete the module window
                                      delete(GUI$leftMain, GUI$leftMain$children[[2]])
                                      ## display the default view (data, variable, etc.)
                                      GUI$plotToolbar$restore()
                                      visible(GUI$gp1) <<- TRUE
                                  })

            ## Make the module visible
            visible(GUI$moduleWindow) <<- TRUE

            ## IF time series variable is chosen, plot first variable.
            svalue(g3_opt1, index = TRUE) <- 1
        },

        # ========
        # METHODS
        # ========
        ## returns the time variable index
        getTime = function(data, index = TRUE) {
            ## look for time or date
            time_re = "([Tt][Ii][Mm][Ee])|([Dd][Aa][Tt][Ee])"
            ind     = grep(time_re, names(data))
            if (index) return(ind)
            else if (length(ind) == 0) return(NA)
            else return(names(data)[ind])
        },

        ## checks for a time variable in dataset
        isTS = function(data) {
            return(length(getTime(data)) != 0)
        },

        ## drops categorical variables (except the time variable)
        tsData = function(data) {
            time_index = getTime(data)

            data[, c(time_index, which(sapply(data, is.numeric)))]
        },

        ## draw the plot, depending on the settings
        updatePlot = function(animate = FALSE) {
            ## plot the TS object setup by the GUI

            if (animate) gmessage("Animation not yet implemented :(")
            animate <- FALSE

            decomp <<- NULL
            forecasts <<- NULL

            can.smooth <- TRUE
            smooth.t <- smoothness

            if (is.null(tsObj)) {
                cat("Nothing to plot ...\n")
                plot.new()
            } else if (inherits(tsObj, "iNZightMTS")) { ## multiple vars
                switch(compare,
                       compareplot(tsObj, multiplicative = (patternType == 1),
                                   xlab = svalue(xLab), ylab = svalue(yLab), t = smooth.t),
                       multiseries(tsObj, multiplicative = (patternType == 1),
                                   xlab = svalue(xLab), ylab = svalue(yLab), t = smooth.t))
            } else { ## single var
                switch(plottype, {
                    ## 1 >> standard plot
                    ## patternType = 1 >> 'multiplicative'; 2 >> 'additive'
                    iNZightTS::rawplot(tsObj, multiplicative = (patternType == 1),
                                       ylab = svalue(yLab), xlab = svalue(xLab), animate = animate, t = smooth.t)
                }, {
                    ## 2 >> decomposed plot
                    decomp <<- iNZightTS::decompositionplot(tsObj, multiplicative = (patternType == 1),
                                                            xlab = svalue(xLab), ylab = svalue(yLab),
                                                            t = smooth.t)
                    visible(recomposeBtn) <<- TRUE
                    visible(recomposeResBtn) <<- TRUE
                }, {
                    ## 3 >> season plot
                    iNZightTS::seasonplot(tsObj, multiplicative = (patternType == 1),
                                          xlab = svalue(xLab), ylab = svalue(yLab), t = smooth.t)
                }, {
                    ## 4 >> forecast plot
                    forecasts <<- iNZightTS::forecastplot(tsObj, multiplicative = (patternType == 1),
                                                          xlab = svalue(xLab), ylab = svalue(yLab))
                    visible(forecastBtn) <<- TRUE
                    can.smooth <- FALSE
                })

            }

            enabled(smthSlider) <<- can.smooth

        }
    )
)

#iNZightTimeSeries()