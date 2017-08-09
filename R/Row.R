
RowDlg <- function(newrow = TRUE)
{
    if(!is.null(DEenv$roww)){
        focus(DEenv$roww) <- TRUE
        return(invisible(NULL))
    }
    onDestroy <- function(...)
    {
        rm(list = "roww", envir = DEenv)
    }

    if(!newrow){
        sid <- as.integer(svalue(DEenv$dfview))
        if(length(sid) == 0){
            gmessage(gettext("No row is selected.", domain = "R-DataEntry"),
                     type = "warning")
            return(invisible(NULL))
        }
        srow <- DEenv$Data[DEenv$Data$id == sid, ]
        if(DEenv$ProjOpt$emptycell)
            srow <- sapply(srow, function(x) ifelse(is.na(x), "", as.character(x)))
        else
            srow <- sapply(srow, function(x) ifelse(is.na(x), DEenv$ProjOpt$missv, as.character(x)))
    }

    DEenv$roww <- gwindow(ifelse(newrow,
                                 gettext("Add row", domain = "R-DataEntry"),
                                 gettext("Replace row", domain = "R-DataEntry")),
                          handler = onDestroy, visible = FALSE)
    g <- ggroup(horizontal = FALSE, container = DEenv$roww, use.scrollwindow = TRUE)
    l <- glayout(container = g, expand = TRUE)
    ilist <- list()
    tlist <- character()

    for(i in 2:ncol(DEenv$Data)){
        icol <- names(DEenv$Data)[i]
        l[i-1, 1, expand = TRUE, anchor = c(-1, 1)] <- glabel(paste0(icol, ":"))
        if(newrow){
            if(DEenv$ProjOpt$droplist && !is.na(DEenv$VarAttr[[i]]$valid.values[1])){
                if(DEenv$ProjOpt$emptycell)
                    ilist[[i-1]] <- gcombobox(c("", DEenv$VarAttr[[i]]$valid.values))
                else
                    ilist[[i-1]] <- gcombobox(c("", DEenv$VarAttr[[i]]$valid.values, DEenv$ProjOpt$missv))
                tlist[i-1] <- "C"
            } else {
                ilist[[i-1]] <- gedit(width = DEenv$ProjOpt$editwidth)
                tlist[i-1] <- "E"
            }
        } else {
            if(DEenv$ProjOpt$droplist && !is.na(DEenv$VarAttr[[i]]$valid.values[1])){
                items <- c("", as.character(DEenv$VarAttr[[i]]$valid.values))
                if(!DEenv$ProjOpt$emptycell)
                    items <- c(items, DEenv$ProjOpt$missv)
                idx <- items %in% srow[icol]
                if(sum(idx) < 1){
                    idx <- 1
                    warning(sprintf(gettext("Error trying to find \"%s\" as valid value of \"%s\".",
                                            domain = "R-DataEntry"), srow[icol], icol))
                } else {
                    idx <- grep(TRUE, idx)
                }
                ilist[[i-1]] <- gcombobox(items, selected = idx)
                tlist[i-1] <- "C"
            } else {
                ilist[[i-1]] <- gedit(srow[icol], width = DEenv$ProjOpt$editwidth)
                tlist[i-1] <- "E"
            }
        }
        l[i-1, 2] <- ilist[[i-1]]
        l[i-1, 3, expand = TRUE, anchor = c(-1, 1)] <- glabel(DEenv$VarAttr[[icol]]$label)
    }
    g1 <- ggroup(container = g)
    addSpring(g1)
    btCancel <- gbutton(gettext("Cancel", domain = "R-DataEntry"), container = g1)
    if(newrow)
        btAdd <- gbutton(gettext("Add", domain = "R-DataEntry"), container = g1)
    else
        btAdd <- gbutton(gettext("Replace", domain = "R-DataEntry"), container = g1)

    onBtAddClick <- function(...)
    {
        ## This code to avoid users adding the same row twice does not work:
        # if(!is.null(DEenv$addingRow) && DEenv$addingRow){
        #     gmessage(gettext("The button was already pressed.", domain = "R-DataEntry"),
        #              type = "warning")
        #     return(invisible(NULL))
        # }
        # on.exit(DEenv$addingRow <- FALSE)
        # DEenv$addingRow <- TRUE

        onerow <- list()
        if(!newrow){
            onerow[1] <- sid
        } else {
            DEenv$id <- as.integer(DEenv$id + 1)
            onerow[1] <- DEenv$id
        }
        names(onerow) <- "id"

        vlist <- sapply(ilist, svalue)
        varattr <- DEenv$VarAttr
        varnames <- names(DEenv$Data)
        for(i in 2:ncol(DEenv$Data)){
            onerow[i] <- vlist[i-1]
            Encoding(onerow[[i]]) <- "UTF-8"
            vattr <- varattr[[varnames[i]]]

            # NA value
            if(onerow[[i]] == DEenv$ProjOpt$missv){
                onerow[[i]] <- NA
            } else if(onerow[i] == ""){
                if(DEenv$ProjOpt$emptycell){
                    onerow[[i]] <- NA
                } else {
                    gmessage(paste0(varnames[i], ": ",
                                    gettext("No cell might be left empty.",
                                            domain = "R-DataEntry")))
                    focus(ilist[[i-1]])
                    return(invisible(NULL))
                }
            }
            if(is.na(onerow[[i]])){
                if(vattr[["class"]] == "factor")
                    onerow[[i]] <- factor(NA,
                                          levels = 1:length(vattr[["valid.values"]]),
                                          labels = vattr[["valid.values"]])
                else if(vattr[["class"]] == "numeric")
                    onerow[[i]] <- as.numeric(NA)
                else if(vattr[["class"]] == "integer")
                    onerow[[i]] <- as.integer(NA)
                else if(vattr[["class"]] == "character")
                    onerow[[i]] <- as.character(NA)
                next
            }

            if(vattr[["class"]] == "integer"){
                if(!IsNumericInt(onerow[i], "integer")){
                    focus(ilist[[i-1]]) <- TRUE
                    return(invisible(NULL))
                }
                onerow[[i]] <- as.integer(onerow[[i]])
            } else if(vattr[["class"]] == "numeric"){
                if(!IsNumericInt(onerow[i], "numeric")){
                    focus(ilist[[i-1]]) <- TRUE
                    return(invisible(NULL))
                }
                onerow[[i]] <- as.numeric(onerow[[i]])
            }
            if(!is.na(vattr[["min"]]) && onerow[[i]] < vattr[["min"]]){
                gmessage(sprintf(gettext("The minimum value of '%s' is '%s'", domain = "R-DataEntry"),
                                 varnames[i], vattr[["min"]]))
                focus(ilist[[i-1]]) <- TRUE
                return(invisible(NULL))
            }
            if(!is.na(vattr[["max"]]) && onerow[[i]] > vattr[["max"]]){
                gmessage(sprintf(gettext("The maximum value of '%s' is '%s'", domain = "R-DataEntry"),
                                 varnames[i], vattr[["max"]]))
                focus(ilist[[i-1]]) <- TRUE
                return(invisible(NULL))
            }
            if(!is.na(vattr[["valid.values"]][1])){
                idx <- 0
                for(idx in 1:length(vattr[["valid.values"]])){
                    if(vattr[["valid.values"]][idx] == onerow[[i]]){
                        break
                    }
                }
                if(idx == 0){
                    gmessage(sprintf(gettext("Invalid value for '%s': '%s'. Valid values are:\n%s",
                                             domain = "R-DataEntry"),
                                     varnames[i], onerow[[i]],
                                     paste(vattr[["valid.values"]], collapse = "\n")))
                    focus(ilist[[i-1]]) <- TRUE
                    return(invisible(NULL))
                }
            }
            if(vattr[["class"]] == "factor"){
                if(length(vattr[["valid.values"]]) > 0){
                    onerow[[i]] <- factor(idx,
                                          levels = 1:length(vattr[["valid.values"]]),
                                          labels = vattr[["valid.values"]])
                }
            }
        }
        names(onerow) <- names(DEenv$Data)
        onerow <- as.data.frame(onerow, stringsAsFactors = FALSE)
        if(newrow)
            DEenv$Data <- rbind(DEenv$Data, onerow)
        else
            DEenv$Data[DEenv$Data$id == sid, ] <- onerow
        UpdateDFView()
        DEenv$ProjOpt$size.roww <- size(DEenv$roww)
        SaveProject()

        if(svalue(btAdd) == gettext("Replace", domain = "R-DataEntry")){
            dispose(DEenv$roww)
        } else {
            for(i in 2:ncol(DEenv$Data)){
                if(tlist[i-1] == "E")
                    svalue(ilist[[i-1]]) <- ""
                else
                    svalue(ilist[[i-1]], index = TRUE) <- 1
            }
            # FIXME: Scroll DEenv$roww
            focus(ilist[[1]]) <- TRUE
        }
    }

    onBtCloseClick <- function(...)
    {
        dispose(DEenv$roww)
    }

    addHandlerClicked(btAdd, onBtAddClick)
    addHandlerClicked(btCancel, onBtCloseClick)
    if(!is.null(DEenv$ProjOpt$size.roww))
        size(DEenv$roww) <- DEenv$ProjOpt$size.roww

    if(!is.null(DEenv$AppOpt$font)){
        fnt <- pangoFontDescriptionFromString(DEenv$AppOpt$font)
        for(i in 2:ncol(DEenv$Data)){
            gtkWidgetModifyFont(l[i-1, 1]@.xData$widget, fnt)
            gtkWidgetModifyFont(l[i-1, 2]@.xData$widget, fnt)
            gtkWidgetModifyFont(l[i-1, 3]@.xData$widget, fnt)
        }
        gtkWidgetModifyFont(btCancel@.xData$widget$getChildren()[[1]], fnt)
        gtkWidgetModifyFont(btAdd@.xData$widget$getChildren()[[1]], fnt)
    }


    visible(DEenv$roww) <- TRUE
    focus(ilist[[1]]) <- TRUE
}
