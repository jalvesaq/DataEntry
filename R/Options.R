
OptionsDlg <- function(...)
{
    if(!is.null(DEenv$optw)){
        focus(DEenv$optw) <- TRUE
        return(invisible(NULL))
    }

    if(is.null(DEenv$AppOpt))
        GetAppOpt()

    onDestroy <- function(...)
    {
        rm(list = "optw", envir = DEenv)
    }


    DEenv$optw <- gwindow(gettext("Options", domain = "R-DataEntry"), handler = onDestroy, visible = FALSE)
    g <- ggroup(horizontal = FALSE, container = DEenv$optw)

    p <- gframe(gettext("Project options", domain = "R-DataEntry"),
                horizontal = FALSE, container = g)
    if(is.null(DEenv$ProjOpt)){
        cbDrop <- gcheckbox(gettext("Put valid values in dropdown list", domain = "R-DataEntry"),
                            checked = FALSE, container = p)
        cbEmpty <- gcheckbox(gettext("Allow blank cells", domain = "R-DataEntry"),
                             checked = FALSE, container = p)
        gm <- ggroup(container = p)
        glabel(gettext("Text representing missing values: ", domain = "R-DataEntry"),
               container = gm, anchor = c(-1, 1))
        edMissV <- gedit("", width = 4, container = gm)
        gw <- ggroup(container = p)
        glabel(gettext("Edit box width (number of characters): ",
                       domain = "R-DataEntry"), container = gw)
        edBoxWd <- gedit("25", width = 3, container = gw)
    } else {
        cbDrop <- gcheckbox(gettext("Put valid values in dropdown list", domain = "R-DataEntry"),
                            checked = DEenv$ProjOpt$droplist, container = p)
        cbEmpty <- gcheckbox(gettext("Allow blank cells", domain = "R-DataEntry"),
                             checked = DEenv$ProjOpt$emptycell, container = p)
        gm <- ggroup(container = p)
        glabel(gettext("Text representing missing values: ", domain = "R-DataEntry"),
               container = gm, anchor = c(-1, 1))
        edMissV <- gedit(DEenv$ProjOpt$missv, width = 4, container = gm)
        gw <- ggroup(container = p)
        glabel(gettext("Edit box width (number of characters): ",
                       domain = "R-DataEntry"), container = gw)
        edBoxWd <- gedit(as.character(DEenv$ProjOpt$editwidth), width = 3, container = gw)
    }

    a <- gframe(gettext("Application options", domain = "R-DataEntry"),
                horizontal = FALSE, container = g)

    gf <- ggroup(container = a)
    glabel(gettext("Set font for entering data:", domain = "R-DataEntry"), container = gf)
    btFont <- gtkFontButtonNew()
    if(!is.null(DEenv$AppOpt$font)){
        gtkFontButtonSetFontName(btFont, DEenv$AppOpt$font)
    }
    oldfont <- btFont$GetFontName()
    add(gf, btFont)

    cbBckOpen <- gcheckbox(gettext("Backup when opening project",
                                   domain = "R-DataEntry"),
                           checked = DEenv$AppOpt$bckopen, container = a)
    cbBckLast <- gcheckbox(gettext("Keep only the last backups",
                                   domain = "R-DataEntry"),
                           checked = DEenv$AppOpt$bcklast, container = a)
    g1 <- ggroup(container = g)
    lbNBck <- glabel(gettext("Number of backups to keep: ", domain = "R-DataEntry"),
                     container = g1, anchor = c(-1, 1))
    edNBcks <- gedit(as.character(DEenv$AppOpt$nbcks), width = 3, container = g1)

    addSpring(g)
    g2 <- ggroup(container = g)
    addSpring(g2)
    btDefault <- gbutton(gettext("Default", domain = "R-DataEntry"), container = g2)
    btCancel <- gbutton(gettext("Cancel", domain = "R-DataEntry"), container = g2)
    btOK <- gbutton(gettext("OK", domain = "R-DataEntry"), container = g2)

    SetDefault <- function(...)
    {
        SetDefaultAppOpt()
        SetDefaultProjOpt()
    }

    SetOptions <- function(...)
    {
        if(!IsNumericInt(svalue(edNBcks), "integer")){
            focus(edNBcks) <- TRUE
            return(invisible(NULL))
        }
        if(!IsNumericInt(svalue(edBoxWd), "integer")){
            focus(edBoxWd) <- TRUE
            return(invisible(NULL))
        } else {
            edbw <- as.integer(svalue(edBoxWd))
            if(is.na(edbw) || edbw < 1 || edbw > 999){
                gmessage(gettext("Please, enter a integer number between 1 and 999.",
                                 domain = "R-DataEntry"), type = "warning")
                focus(edBoxWd) <- TRUE
                return(invisible(NULL))
            }
        }
        if(as.integer(svalue(edNBcks)) < 1){
            gmessage(gettext("Please, enter a positive integer number.",
                             domain = "R-DataEntry"), type = "warning")
            focus(edNBcks) <- TRUE
            return(invisible(NULL))
        }
        if(!svalue(cbEmpty) && svalue(edMissV) == ""){
            gmessage(gettext("You must define the string representing missing values.",
                             domain = "R-DataEntry"), type = "warning")
            focus(edMissV) <- TRUE
            return(invisible(NULL))
        }

        if(oldfont != btFont$GetFontName()){
            DEenv$AppOpt$font <- btFont$GetFontName()
            font <- pangoFontDescriptionFromString(DEenv$AppOpt$font)
            gtkWidgetModifyFont(DEenv$mainw@.xData$widget, font)
        }

        if(!is.null(DEenv$VarAttr)){
            DEenv$ProjOpt$droplist  <- svalue(cbDrop)
            DEenv$ProjOpt$emptycell <- svalue(cbEmpty)
            DEenv$ProjOpt$missv     <- svalue(edMissV)
            DEenv$ProjOpt$editwidth <- edbw
            SaveProject()
        }
        DEenv$AppOpt$bckopen    <- svalue(cbBckOpen)
        DEenv$AppOpt$bcklast    <- svalue(cbBckLast)
        DEenv$AppOpt$nbcks      <- as.integer(svalue(edNBcks))
        SaveAppOpt()
        dispose(DEenv$optw)
    }

    ShowHide <- function(...)
    {
        visible(cbBckLast) <- FALSE
        visible(g1) <- FALSE
        visible(gm) <- FALSE
        if(is.null(DEenv$ProjOpt))
            visible(p) <- FALSE
        else
            visible(p) <- TRUE
        if(!svalue(cbEmpty))
            visible(gm) <- TRUE
        if(svalue(cbBckOpen)){
            visible(cbBckLast) <- TRUE
            if(svalue(cbBckLast))
                visible(g1) <- TRUE
            else
                visible(g1) <- FALSE
        } else {
            visible(cbBckLast) <- FALSE
            visible(g1) <- FALSE
        }
    }

    addHandlerClicked(cbEmpty, ShowHide)
    addHandlerClicked(cbBckOpen, ShowHide)
    addHandlerClicked(cbBckLast, ShowHide)
    addHandlerClicked(btDefault, SetDefault)
    addHandlerClicked(btCancel, function(...) dispose(DEenv$optw))
    addHandlerClicked(btOK, SetOptions)
    ShowHide()
    visible(DEenv$optw) <- TRUE
    focus(btCancel) <- TRUE
}
