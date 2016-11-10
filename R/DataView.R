
UpdateDFView <- function()
{
    DEenv$dfview[,] <- DEenv$Data
}

DataEntryDlg <- function(...)
{
    if("dataw" %in% ls(DEenv)){
        focus(DEenv$dataw)
        return(invisible(NULL))
    }

    onDestroy <- function(...)
    {
        if("roww" %in% ls(DEenv))
            dispose(DEenv$roww)
        rm(list = "dataw", envir = DEenv)
    }

    DEenv$dataw <- gwindow(gettext("View and edit data", domain = "R-DataEntry"),
                                  handler = onDestroy, visible = FALSE)
    g <- ggroup(horizontal = FALSE, container = DEenv$dataw)
    DEenv$dfview <- gtable(DEenv$Data, container = g, expand = TRUE)
    g1 <- ggroup(container = g)
    addSpring(g1)
    btEdit <- gbutton(gettext("Edit row", domain = "R-DataEntry"), container = g1)
    btDelete <- gbutton(gettext("Delete row", domain = "R-DataEntry"), container = g1)
    btAdd <- gbutton(gettext("Add row", domain = "R-DataEntry"), container = g1)
    addSpring(g1)
    btClose <- gbutton(gettext("Close", domain = "R-DataEntry"), container = g1)

    onBtDeleteClick <- function(...)
    {
        n <- svalue(DEenv$dfview)
        DEenv$Data <- DEenv$Data[DEenv$Data$id != n, ]
        UpdateDFView()
        SaveProject()
    }

    addHandlerChanged(btEdit,   function(...) RowDlg(FALSE))
    addHandlerChanged(btDelete, onBtDeleteClick)
    addHandlerChanged(btAdd,    function(...) RowDlg())
    addHandlerChanged(btClose,  function(...) dispose(DEenv$dataw))
    UpdateDFView()
    visible(DEenv$dataw) <- TRUE
}

