
DataEntry <- function()
{
    if("mainw" %in% ls(DEenv)){
        focus(DEenv$mainw)
        return(invisible(NULL))
    }

    onDestroy <- function(...)
    {
        if(!is.null(DEenv$varw))
            dispose(DEenv$varw)
        if(!is.null(DEenv$dataw))
            dispose(DEenv$dataw)
        if(!is.null(DEenv$optw))
            dispose(DEenv$optw)
        if(!is.null(DEenv$expw))
            dispose(DEenv$expw)
        rm(list = ls(DEenv), envir = DEenv)
    }

    options("guiToolkit" = "RGtk2")
    DEenv$mainw <- gwindow("DataEntry", handler = onDestroy, visible = FALSE)
    ggroup(container = DEenv$mainw, expand = TRUE)
    g <- ggroup(horizontal = FALSE, container = DEenv$mainw)
    ggroup(container = DEenv$mainw, expand = TRUE)

    addSpring(g)
    bt1 <- gbutton(gettext("New project", domain = "R-DataEntry"), container = g)
    bt2 <- gbutton(gettext("Open project", domain = "R-DataEntry"), container = g)
    expBt <- gbutton(gettext("Export data", domain = "R-DataEntry"), container = g)
    addSpring(g)
    optBt <- gbutton(gettext("Options", domain = "R-DataEntry"), container = g) 
    clsBt <- gbutton(gettext("Close", domain = "R-DataEntry"), container = g)

    onBt1Click <- function(...)
    {
        if(svalue(bt1) == gettext("New project", domain = "R-DataEntry")){
            if(NewProject()){
                svalue(bt1) <- gettext("Set variables", domain = "R-DataEntry")
                svalue(bt2) <- gettext("Edit data", domain = "R-DataEntry")
                enabled(expBt) <- TRUE
                focus(bt1)
            }
        } else {
            if(svalue(bt1) == gettext("Set variables", domain = "R-DataEntry"))
                VarListDlg()
        }
    }

    onBt2Click <- function(...)
    {
        if(svalue(bt2) == gettext("Open project", domain = "R-DataEntry")){
            if(OpenProject()){
                svalue(bt1) <- gettext("Set variables", domain = "R-DataEntry")
                svalue(bt2) <- gettext("Edit data", domain = "R-DataEntry")
                enabled(expBt) <- TRUE
                if(length(names(DEenv$Data)) < 2)
                    focus(bt1)
            }
        } else {
            if(svalue(bt2) == gettext("Edit data", domain = "R-DataEntry"))
                DataEntryDlg()
        }
    }

    addHandlerChanged(bt1, onBt1Click)
    addHandlerChanged(bt2, onBt2Click)
    addHandlerChanged(expBt, ExportDlg)
    addHandlerChanged(optBt, OptionsDlg)
    addHandlerChanged(clsBt, function(...) dispose(DEenv$mainw))
    enabled(expBt) <- FALSE
    visible(DEenv$mainw) <- TRUE
    return(invisible(NULL))
}
