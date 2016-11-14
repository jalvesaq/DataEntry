
OptionsDlg <- function(...)
{
    if(!is.null(DEenv$optw)){
        focus(DEenv$optw)
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
                horizontal = FALSE)

    cbDrop <- gcheckbox(gettext("Put valid values in dropdown list", domain = "R-DataEntry"),
                        checked = DEenv$ProjOpt$droplist, container = p)
    cbEmpty <- gcheckbox(gettext("Allow blank cells", domain = "R-DataEntry"),
                         checked = DEenv$ProjOpt$emptycell, container = p)
    gm <- ggroup(container = p)
    glabel(gettext("Text representing missing values: ", domain = "R-DataEntry"),
           container = gm, anchor = c(-1, 1))
    edMissV <- gedit(DEenv$ProjOpt$missv, width = 4, container = gm)

    if(!is.null(DEenv$VarAttr))
        add(g, p)

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
                           checked = DEenv$AppOpt$bcklast)
    g1 <- ggroup()
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
            focus(edNBcks)
            return(invisible(NULL))
        }
        if(as.integer(svalue(edNBcks)) < 1){
            gmessage(gettext("Please, enter a positive integer number.",
                             domain = "R-DataEntry"), type = "warning")
            focus(edNBcks)
            return(invisible(NULL))
        }
        if(!svalue(cbEmpty) && svalue(edMissV) == ""){
            gmessage(gettext("You must define the string representing missing values.",
                             domain = "R-DataEntry"), type = "warning")
            focus(edMissV)
            return(invisible(NULL))
        }

        if(oldfont != btFont$GetFontName()){
            DEenv$AppOpt$font <- btFont$GetFontName()
            font <- pangoFontDescriptionFromString(DEenv$AppOpt$font)
            gtkWidgetModifyFont(DEenv$mainw@widget@widget, font)
        }

        if(!is.null(DEenv$VarAttr)){
            DEenv$ProjOpt$droplist  <- svalue(cbDrop)
            DEenv$ProjOpt$emptycell <- svalue(cbEmpty)
            DEenv$ProjOpt$missv     <- svalue(edMissV)
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
        delete(a, cbBckLast)
        delete(a, g1)
        delete(p, gm)
        if(!svalue(cbEmpty))
            add(p, gm)
        if(svalue(cbBckOpen)){
            add(a, cbBckLast)
            if(svalue(cbBckLast))
                add(a, g1)
            else
                delete(a, g1)
        } else {
            delete(a, cbBckLast)
            delete(a, g1)
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
    focus(btCancel)
}
