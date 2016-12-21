
RowDlg <- function(newrow = TRUE)
{
    if(!is.null(DEenv$roww)){
        focus(DEenv$roww)
        return(invisible(NULL))
    }
    onDestroy <- function(...)
    {
        rm(list = "roww", envir = DEenv)
    }

    DEenv$roww <- gwindow(ifelse(newrow,
                                 gettext("Add row", domain = "R-DataEntry"),
                                 gettext("Replace row", domain = "R-DataEntry")),
                          handler = onDestroy, visible = FALSE)
    g <- ggroup(horizontal = FALSE, container = DEenv$roww, use.scrollwindow = TRUE)
    l <- glayout(container = g, expand = TRUE)
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
    for(i in 2:ncol(DEenv$Data)){
        icol <- names(DEenv$Data)[i]
        l[i-1, 1] <- paste0(icol, ":")
        if(newrow){
            if(DEenv$ProjOpt$droplist && !is.na(DEenv$VarAttr[[i]]$valid.values[1])){
                if(DEenv$ProjOpt$emptycell)
                    l[i-1, 2] <- gdroplist(c("", DEenv$VarAttr[[i]]$valid.values))
                else
                    l[i-1, 2] <- gdroplist(c("", DEenv$VarAttr[[i]]$valid.values, DEenv$ProjOpt$missv))
            } else {
                l[i-1, 2] <- gedit(width = 10)
            }
        } else {
            if(DEenv$ProjOpt$droplist && !is.na(DEenv$VarAttr[[i]]$valid.values[1])){
                items <- c("", as.character(DEenv$VarAttr[[i]]$valid.values))
                if(!DEenv$ProjOpt$emptycell)
                    items <- c(items, DEenv$ProjOpt$missv)
                idx <- grep(paste0("^", srow[icol], "$"), items)
                if(idx < 1){
                    idx <- 1
                    warning(sprintf(gettext("Error trying to find \"%s\" as valid value of \"%s\".",
                                            domain = "R-DataEntry"), srow[icol], icol))
                }
                l[i-1, 2] <- gdroplist(items, selected = idx)
            } else {
                l[i-1, 2] <- gedit(srow[icol], width = 10)
            }
        }
        l[i-1, 3] <- DEenv$VarAttr[[icol]]$label
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
        onerow <- list()
        if(!newrow){
            onerow[1] <- sid
        } else {
            DEenv$id <- as.integer(DEenv$id + 1)
            onerow[1] <- DEenv$id
        }
        names(onerow) <- "id"
        varattr <- DEenv$VarAttr
        varnames <- names(DEenv$Data)
        for(i in 2:ncol(DEenv$Data)){
            onerow[i] <- svalue(l[i-1, 2])
            vattr <- varattr[[varnames[i]]]

            # NA value
            if(onerow[i] == ""){
                if(DEenv$ProjOpt$emptycell){
                    onerow[[i]] <- NA
                } else {
                    gmessage(gettext("No cell might be left empty.",
                                     domain = "R-DataEntry"))
                    focus(l[i-1, 2])
                    return(invisible(NULL))
                }
            }
            if(onerow[[i]] == DEenv$ProjOpt$missv){
                onerow[[i]] <- NA
            }
            if(is.na(onerow[[i]])){
                if(vattr[["class"]] == "factor")
                    onerow[[i]] <- factor(NA, levels = 1:length(vattr[["valid.values"]]), labels = vattr[["valid.values"]])
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
                    focus(l[i-1, 2])
                    return(invisible(NULL))
                }
                onerow[[i]] <- as.integer(onerow[[i]])
            } else if(vattr[["class"]] == "numeric"){
                if(!IsNumericInt(onerow[i], "numeric")){
                    focus(l[i-1, 2])
                    return(invisible(NULL))
                }
                onerow[[i]] <- as.numeric(onerow[[i]])
            }
            if(!is.na(vattr[["min"]]) && onerow[[i]] < vattr[["min"]]){
                gmessage(sprintf(gettext("The minimum value of '%s' is '%s'", domain = "R-DataEntry"),
                                 varnames[i], vattr[["min"]]))
                focus(l[i-1, 2])
                return(invisible(NULL))
            }
            if(!is.na(vattr[["max"]]) && onerow[[i]] > vattr[["max"]]){
                gmessage(sprintf(gettext("The maximum value of '%s' is '%s'", domain = "R-DataEntry"),
                                 varnames[i], vattr[["max"]]))
                focus(l[i-1, 2])
                return(invisible(NULL))
            }
            if(!is.na(vattr[["valid.values"]][1])){
                is.valid <- FALSE
                for(v in vattr[["valid.values"]])
                    if(v == onerow[[i]]){
                        is.valid <- TRUE
                        break
                    }
                if(!is.valid){
                    gmessage(sprintf(gettext("Invalid value: '%s'. Valid values are:\n%s", domain = "R-DataEntry"),
                                     onerow[[i]], paste(vattr[["valid.values"]], collapse = "\n")))
                    focus(l[i-1, 2])
                    return(invisible(NULL))
                }
            }
            if(vattr[["class"]] == "factor"){
                lbl <- vattr[["valid.values"]]
                if(length(lbl) > 0){
                    onerow[[i]] <- factor(grep(paste0("^", onerow[[i]], "$"), lbl),
                                          levels = 1:length(lbl),
                                          labels = lbl)
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
        dispose(DEenv$roww)
        ## Instead of destroying the window, it would be better to clean the
        ## edit boxes. However, the code below generates the error:
        ## Gtk-CRITICAL **: IA__gtk_table_attach: assertion 'child->parent == NULL' failed
        # for(i in 2:ncol(DEenv$Data)){
        #     svalue(l[i-1, 2]) <- ""
        # }
        # focus(l[1, 2])
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
            gtkWidgetModifyFont(l[i-1, 1]@widget@widget, fnt)
            gtkWidgetModifyFont(l[i-1, 2]@widget@widget, fnt)
            gtkWidgetModifyFont(l[i-1, 3]@widget@widget, fnt)
        }
        gtkWidgetModifyFont(btCancel@widget@widget$getChildren()[[1]], fnt)
        gtkWidgetModifyFont(btAdd@widget@widget$getChildren()[[1]], fnt)
    }


    visible(DEenv$roww) <- TRUE
    focus(l[1, 2])
}
