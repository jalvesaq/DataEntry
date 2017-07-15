
DataEntry <- function()
{
    if(!is.null(DEenv$mainw)){
        focus(DEenv$mainw) <- TRUE
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

    g0 <- ggroup(container = DEenv$mainw, expand = TRUE)

    ggroup(container = g0, expand = TRUE)
    g <- ggroup(horizontal = FALSE, container = g0)
    ggroup(container = g0, expand = TRUE)

    addSpring(g)
    bt1 <- gbutton(gettext("New project", domain = "R-DataEntry"), container = g)
    bt2 <- gbutton(gettext("Open project", domain = "R-DataEntry"), container = g)
    btExp <- gbutton(gettext("Export data", domain = "R-DataEntry"), container = g)
    addSpring(g)
    btOpt <- gbutton(gettext("Options", domain = "R-DataEntry"), container = g)
    btClose <- gbutton(gettext("Close", domain = "R-DataEntry"), container = g)

    # Avoid bug in gWidgets2RGtk2: click event is triggered when the button is clicked
    DEenv$is.changing.label <- 0

    onBt1Click <- function(...)
    {
        if(DEenv$is.changing.label)
            return()
        if(svalue(bt1) == gettext("New project", domain = "R-DataEntry")){
            if(NewProject()){
                DEenv$is.changing.label <- 1
                svalue(bt1) <- gettext("Set variables", domain = "R-DataEntry")
                svalue(bt2) <- gettext("Edit data", domain = "R-DataEntry")
                enabled(btExp) <- TRUE
                focus(bt1) <- TRUE
                DEenv$is.changing.label <- 0
                svalue(DEenv$mainw) <- paste("DataEntry -",
                                             basename(DEenv$fpath))
            }
        } else {
            if(!is.null(DEenv$Data))
                VarListDlg()
        }
    }

    onBt2Click <- function(...)
    {
        if(DEenv$is.changing.label)
            return()
        if(svalue(bt2) == gettext("Open project", domain = "R-DataEntry")){
            if(OpenProject()){
                DEenv$is.changing.label <- 1
                svalue(bt1) <- gettext("Set variables", domain = "R-DataEntry")
                svalue(bt2) <- gettext("Edit data", domain = "R-DataEntry")
                enabled(btExp) <- TRUE
                if(length(names(DEenv$Data)) < 2)
                    focus(bt1) <- TRUE
                DEenv$is.changing.label <- 0
                svalue(DEenv$mainw) <- paste("DataEntry -",
                                             basename(DEenv$fpath))
            }
        } else {
            if(!is.null(DEenv$Data))
                DataEntryDlg()
        }
    }

    addHandlerClicked(bt1, onBt1Click)
    addHandlerClicked(bt2, onBt2Click)
    addHandlerClicked(btExp, ExportDlg)
    addHandlerClicked(btOpt, OptionsDlg)
    addHandlerClicked(btClose, function(...) dispose(DEenv$mainw))
    enabled(btExp) <- FALSE
    visible(DEenv$mainw) <- TRUE
    return(invisible(NULL))
}
