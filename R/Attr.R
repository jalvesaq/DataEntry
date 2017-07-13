
AttrDlg <- function(newvar = TRUE)
{
    if(!is.null(DEenv$attrw)){
        focus(DEenv$attrw)
        return(invisible(NULL))
    }
    onDestroy <- function(...)
    {
        rm(list = "attrw", envir = DEenv)
    }

    if(newvar){
        nm <- ""
        lb <- ""
        cl <- "character"
        vv <- ""
        mi <- ""
        ma <- ""
    } else {
        nm <- svalue(DEenv$vlist)
        if(nm == "id"){
            gmessage(gettext("Cannot edit 'id'.", domain = "R-DataEntry"),
                     type = "warning")
            return(invisible(NULL))
        }
        Encoding(nm) <- "UTF-8"
        lb <- DEenv$VarAttr[[nm]]$label
        cl <- DEenv$VarAttr[[nm]]$class
        if(is.na(DEenv$VarAttr[[nm]]$valid.values[1]))
            vv <- ""
        else
            vv <- paste(as.character(DEenv$VarAttr[[nm]]$valid.values),
                        collapse = "\n")
        if(is.na(DEenv$VarAttr[[nm]]$min))
            mi <- ""
        else
            mi <- as.character(DEenv$VarAttr[[nm]]$min)
        if(is.na(DEenv$VarAttr[[nm]]$max))
            ma <- ""
        else
            ma <- as.character(DEenv$VarAttr[[nm]]$max)
    }

    DEenv$attrw <- gwindow(gettext("Attributes", domain = "R-DataEntry"),
                           handler = onDestroy, visible = FALSE)
    vbox <- ggroup(horizontal = FALSE, container = DEenv$attrw)

    lb1 <- glabel(gettext("Name:", domain = "R-DataEntry"), container = vbox, anchor = c(-1, 1))
    edName <- gedit(nm, width = 25, container = vbox, anchor = c(-1, 1))

    lb2 <- glabel(gettext("Label:", domain = "R-DataEntry"), container = vbox, anchor = c(-1, 1))
    edLbl <- gedit(lb, width = 25, container = vbox, anchor = c(-1, 1))

    lb3 <- glabel(gettext("Class:", domain = "R-DataEntry"), container = vbox, anchor = c(-1, 1))
    clls <- c("character", "factor", "integer", "numeric")
    rdClass <- gradio(clls, selected = grep(cl, clls), horizontal = TRUE,
                      container = vbox, anchor = c(-1, 1))
    gseparator(container = vbox)

    intBox <- ggroup(container = vbox)
    lbInt <- glabel(gettext("Set valid:", domain = "R-DataEntry"), container = intBox)
    rdIntVal <- gradio(c(gettext("range", domain = "R-DataEntry"),
                         gettext("values", domain = "R-DataEntry")),
                       selected = ifelse(vv == "", 1, 2), horizontal = TRUE, container = intBox)

    rngBox <- ggroup(container = vbox)
    lbRng <- glabel(gettext("Valid range:", domain = "R-DataEntry"), container = rngBox)
    lbMin <- glabel(gettext("Min:", domain = "R-DataEntry"), container = rngBox, anchor = c(-1, 0))
    edMin <- gedit(mi, width = 6, container = rngBox, anchor = c(-1, 1))
    lbMax <- glabel(gettext("Max:", domain = "R-DataEntry"), container = rngBox, anchor = c(-1, 0))
    edMax <- gedit(ma, width = 6, container = rngBox, anchor = c(-1, 1))

    vvBox <- ggroup(horizontal = FALSE, container = vbox)
    lbVV <- glabel(gettext("Valid values (one per line):", domain = "R-DataEntry"),
                   anchor = c(-1, 1), container = vvBox)
    txVV <- gtext(vv, width = 25, height = 100, anchor = c(-1, 1), container = vvBox)

    visible(vvBox) <- FALSE
    visible(intBox) <- FALSE
    visible(rngBox) <- FALSE

    addSpring(vbox)
    hbox2 <- ggroup(container = vbox)
    addSpring(hbox2)
    btCancel <- gbutton(gettext("Cancel", domain = "R-DataEntry"), container = hbox2)
    btOK <- gbutton(gettext("OK", domain = "R-DataEntry"), container = hbox2)

    onClassChange <- function(...){
        visible(vvBox) <- FALSE
        visible(intBox) <- FALSE
        visible(rngBox) <- FALSE
        if(svalue(rdClass) == "integer")
            visible(intBox) <- TRUE
        if(svalue(rdClass) == "numeric" || (svalue(rdClass) == "integer" && svalue(rdIntVal) == gettext("range", domain = "R-DataEntry")))
            visible(rngBox) <- TRUE
        else if(svalue(rdClass) == "factor" || (svalue(rdClass) == "integer" && svalue(rdIntVal) == gettext("values", domain = "R-DataEntry")))
            visible(vvBox) <- TRUE
    }

    onCancel <- function(...){
        dispose(DEenv$attrw)
    }

    onOK <- function(...){
        # Get clean values
        nm <- sub("^[ \t\r\n]*", "", svalue(edName))
        nm <- sub("[ \t\r\n]*$", "", nm)
        lb <- sub("^[ \t\r\n]*", "", svalue(edLbl))
        lb <- sub("[ \t\r\n]*$", "", lb)
        cl <- svalue(rdClass)
        vv <- svalue(txVV)
        vv <- strsplit(vv, "\n")[[1]]
        vv <- sub("^[ \t\n\r]*", "", vv)
        vv <- sub("[ \t\n\r]*$", "", vv)
        vv <- vv[vv != ""]
        mi <- sub("^[ \t\r\n]*", "", svalue(edMin))
        mi <- sub("[ \t\r\n]*$", "", mi)
        mi <- sub("^NA$", "", mi)
        ma <- sub("^[ \t\r\n]*", "", svalue(edMax))
        ma <- sub("[ \t\r\n]*$", "", ma)
        ma <- sub("^NA$", "", ma)

        Encoding(nm) <- "UTF-8"
        Encoding(lb) <- "UTF-8"
        Encoding(vv) <- "UTF-8"

        # Get "range of value" and "valid values" only when they are meaniful
        if(length(vv) == 0)
            vv <- NA
        if(mi == "")
            mi <- NA
        if(ma == "")
            ma <- NA
        if(cl == "character" || cl == "numeric"){
            vv <- NA
        }
        if(cl == "character" || cl == "factor"){
            mi <- NA
            ma <- NA
        }

        # Check if the name is valid
        if(nchar(nm) == 0){
            gmessage(gettext("The variable name cannot be empty.",
                             domain = "R-DataEntry"), type = "warning")
            focus(edName)
            return(invisible(NULL))
        }
        if(grepl("^[0-9]", nm)){
            gmessage(gettext("The variable name cannot begin with a number.",
                             domain = "R-DataEntry"), type = "warning")
            focus(edName)
            return(invisible(NULL))
        }
        if(grepl(" ", nm)){
            gmessage(gettext("The variable name cannot have empty spaces.",
                             domain = "R-DataEntry"), type = "warning")
            focus(edName)
            return(invisible(NULL))
        }
        if(grepl("[@#$%^&*(){}<>?|/\\+=,;:'\"`~]", nm) || grepl("-", nm) || grepl("\\[", nm) || grepl("\\]", nm)){
            gmessage(gettext("Invalid character in the variable name.",
                             domain = "R-DataEntry"), type = "warning")
            focus(edName)
            return(invisible(NULL))
        }

        # Check if the variable label is too long
        if(nchar(lb) > 80){
            gmessage(gettext("The variable label must have at most 80 characters.",
                             domain = "R-DataEntry"), type = "warning")
            focus(edLbl)
            return(invisible(NULL))
        }

        # Check if the values are valid
        if(cl == "numeric" || (cl == "integer" && svalue(rdIntVal) == gettext("range", domain = "R-DataEntry"))){
            vv <- NA
            if(!is.na(mi) && mi != "" && !IsNumericInt(mi, cl)){
                focus(edMin)
                return(invisible(NULL))
            }
            if(!is.na(ma) && ma != "" && !IsNumericInt(mi, cl)){
                focus(edMin)
                return(invisible(NULL))
            }
            if(cl == "numeric"){
                mi <- as.numeric(mi)
                ma <- as.numeric(ma)
            } else {
                mi <- as.integer(mi)
                ma <- as.integer(ma)
            }
        }
        if(cl == "integer" && svalue(rdIntVal) == gettext("values", domain = "R-DataEntry")){
            mi <- NA
            ma <- NA
            for(v in vv){
                if(!IsNumericInt(v, cl)){
                    focus(txVV)
                    return(invisible(NULL))
                }
            }
            vv <- as.integer(vv)
        }

        # Check for duplicates
        if(sum(duplicated(vv))){
            gmessage(sprintf(ngettext(sum(duplicated(vv)),
                                      "There is a duplicated value:\n%s",
                                      "There are duplicated values:\n%s",
                                      domain = "R-DataEntry"),
                             paste(vv[duplicated(vv)], collapse = "\n")),
                     type = "warning")
            focus(txVV)
            return(invisible(NULL))
        }

        # Factors must have valid values defined:
        if(cl == "factor" && (length(vv) == 0 || is.na(vv[1]))){
            gmessage(gettext("Factors must have valid values defined.",
                             domain = "R-DataEntry"), type = "warning")
            focus(txVV)
            return(invisible(NULL))
        }


        dispose(DEenv$attrw)

        if(!newvar){
            oldname <- svalue(DEenv$vlist)
            if(oldname != nm){
                names(DEenv$Data) <- sub(paste0("^", oldname, "$"), nm,
                                         names(DEenv$Data))
                names(DEenv$VarAttr) <- sub(paste0("^", oldname, "$"), nm,
                                            names(DEenv$VarAttr))
            }
        }

        if(cl == "character"){
            DEenv$Data[, nm] <- rep("", nrow(DEenv$Data))
            Encoding(DEenv$Data[, nm]) <- "UTF-8"
        } else if(cl == "integer"){
            DEenv$Data[, nm] <- as.integer(rep(NA, nrow(DEenv$Data)))
        } else if(cl == "factor"){
            DEenv$Data[, nm] <- factor(rep(NA, nrow(DEenv$Data)),
                                       levels = 1:length(vv), labels = vv)
            Encoding(levels(DEenv$Data[, nm])) <- "UTF-8"
        } else {
            DEenv$Data[, nm] <- as.numeric(rep(NA, nrow(DEenv$Data)))
        }
        DEenv$VarAttr[[nm]] <- list("label" = lb, "class" = cl, "valid.values" = vv, "min" = mi, "max" = ma)

        SaveProject()
        UpdateVarList()
        svalue(DEenv$vlist) <- nm
    }

    addHandlerChanged(rdClass, onClassChange)
    addHandlerChanged(rdIntVal, onClassChange)
    addHandlerClicked(btCancel, onCancel)
    addHandlerClicked(btOK, onOK)

    if(!newvar)
        onClassChange()

    if(!is.null(DEenv$AppOpt$font)){
        fnt <- pangoFontDescriptionFromString(DEenv$AppOpt$font)
        gtkWidgetModifyFont(lb1@.xData$widget, fnt)
        gtkWidgetModifyFont(edName@.xData$widget, fnt)
        gtkWidgetModifyFont(lb2@.xData$widget, fnt)
        gtkWidgetModifyFont(edLbl@.xData$widget, fnt)
        gtkWidgetModifyFont(lb3@.xData$widget, fnt)
        # gtkWidgetModifyFont(rdClass@.xData$widget$getChildren()[[1]], fnt) # FIXME: does not work for radio buttons
        gtkWidgetModifyFont(lbInt@.xData$widget, fnt)
        # gtkWidgetModifyFont(rdIntVal@.xData$widget$getChildren()[[1]], fnt) # FIXME: does not work for radio buttons
        gtkWidgetModifyFont(lbRng@.xData$widget, fnt)
        gtkWidgetModifyFont(lbMin@.xData$widget, fnt)
        gtkWidgetModifyFont(edMin@.xData$widget, fnt)
        gtkWidgetModifyFont(lbMax@.xData$widget, fnt)
        gtkWidgetModifyFont(edMax@.xData$widget, fnt)
        gtkWidgetModifyFont(lbVV@.xData$widget, fnt)
        gtkWidgetModifyFont(txVV@.xData$widget, fnt)
        gtkWidgetModifyFont(btCancel@.xData$widget$getChildren()[[1]], fnt)
        gtkWidgetModifyFont(btOK@.xData$widget$getChildren()[[1]], fnt)
    }
    visible(DEenv$attrw) <- TRUE
    focus(edName)
}
