
ExportCSV <- function(fname, sep, how, keepid)
{
    d <- DEenv$Data
    if(!keepid)
        d$id <- NULL

    # TODO: Add variable labels to sps script.
    data.frame2csv(d, fname, sep, how)

    msg <- sprintf(gettext("Data exported to \"%s\"", domain = "R-DataEntry"),
                   paste0(fname, ".csv"))
    gmessage(msg, type = "info")
}

ExportDlg <- function(...)
{
    if(!is.null(DEenv$expw)){
        focus(DEenv$expw)
        return(invisible(NULL))
    }

    onDestroy <- function(...)
    {
        rm(list = "expw", envir = DEenv)
    }

    DEenv$expw <- gwindow(gettext("Export data", domain = "R-DataEntry"),
                          handler = onDestroy, visible = FALSE)
    g <- ggroup(horizontal = FALSE, container = DEenv$expw)

    glabel(gettext("How to export?", domain = "R-DataEntry"), container = g, anchor = c(-1, 1))
    rdExphow <- gradio(c(gettext("Write CSV file", domain = "R-DataEntry"),
                         gettext("Write RData file", domain = "R-DataEntry"),
                         gettext("Copy to R Workspace", domain = "R-DataEntry")),
                       selected = DEenv$AppOpt$exphow, horizontal = FALSE, container = g)
    cbExpid <- gcheckbox(gettext("Include the column \"id\"",
                                 domain = "R-DataEntry"), checked = DEenv$AppOpt$expid, container = g)
    gn <- ggroup(horizontal = FALSE, container = g)

    gfn <- ggroup()
    nm <-  sub(".dte$", "", basename(DEenv$fpath))
    lbFnm <- glabel(gettext("File name:", domain = "R-DataEntry"), container = gfn)
    edFnm <- gedit(nm, width = 20, container = gfn)

    gon <- ggroup()
    nm <- gsub("[[:punct:]]", ".", nm)
    nm <- gsub("[ \t]", ".", nm)
    lbObnm <- glabel(gettext("Object name:", domain = "R-DataEntry"), container = gon)
    edObnm <- gedit(nm, width = 20, container = gon)

    gcsv <- ggroup(horizontal = FALSE, container = g)
    lbFac <- glabel(gettext("How to write factor variables?",
                            domain = "R-DataEntry"), anchor = c(-1, 1))
    rdExpfct <- gradio(c(gettext("As character", domain = "R-DataEntry"),
                         gettext("As integer + R script", domain = "R-DataEntry"),
                         gettext("As integer + SPSS script", domain = "R-DataEntry")),
                       selected = DEenv$AppOpt$expfct, horizontal = FALSE)

    lbSep <- glabel(gettext("What field separator should be used?",
                            domain = "R-DataEntry"), anchor = c(-1, 1))
    rdExpsep <- gradio(c(gettext("Comma", domain = "R-DataEntry"),
                         gettext("Semicolon", domain = "R-DataEntry"),
                         gettext("Tab", domain = "R-DataEntry")),
                       selected = DEenv$AppOpt$expsep, horizontal = FALSE)

    grda <- ggroup(horizontal = FALSE, container = g)
    lbLbl <- glabel(gettext("What to do with variable labels?",
                            domain = "R-DataEntry"))
    rdExplbl <- gradio(c(gettext("Nothing", domain = "R-DataEntry"),
                         gettext("Add a \"variable.labels\" attribute to the data.frame", domain = "R-DataEntry"),
                         gettext("Add a \"label\" attribute to each column", domain = "R-DataEntry")),
                       selected = DEenv$AppOpt$explbl, horizontal = FALSE)

    addSpring(g)
    gbt <- ggroup(container = g)
    addSpring(gbt)
    btCancel <- gbutton(gettext("Cancel", domain = "R-DataEntry"), container = gbt)
    btOk <- gbutton(gettext("OK", domain = "R-DataEntry"), container = gbt)

    ShowHide <- function(...)
    {
        delete(gn, gfn)
        delete(gn, gon)
        delete(gcsv, lbFac)
        delete(gcsv, rdExpfct)
        delete(gcsv, lbSep)
        delete(gcsv, rdExpsep)
        delete(grda, lbLbl)
        delete(grda, rdExplbl)
        if(svalue(rdExphow, index = TRUE) < 3)
            add(gn, gfn)
        if(svalue(rdExphow, index = TRUE) > 1)
            add(gn, gon)
        if(svalue(rdExphow, index = TRUE) == 1){
            add(gcsv, lbFac, anchor = c(-1, 1))
            add(gcsv, rdExpfct)
            add(gcsv, lbSep, anchor = c(-1, 1))
            add(gcsv, rdExpsep)
        } else {
            add(grda, lbLbl, anchor = c(-1, 1))
            add(grda, rdExplbl)
        }
    }

    onOKclick <- function(...)
    {
        DEenv$AppOpt$exphow <- svalue(rdExphow, index = TRUE)
        DEenv$AppOpt$expfct <- svalue(rdExpfct, index = TRUE)
        DEenv$AppOpt$expsep <- svalue(rdExpsep, index = TRUE)
        DEenv$AppOpt$explbl <- svalue(rdExplbl, index = TRUE)
        DEenv$AppOpt$expid  <- svalue(cbExpid)

        SaveAppOpt()

        if(DEenv$AppOpt$exphow == 1){
            sep <- c(",", ";", "\\t")[DEenv$AppOpt$expsep]
            how <- c("char", "R", "SPSS")[DEenv$AppOpt$expfct]
            ExportCSV(paste0(dirname(DEenv$fpath), "/", svalue(edFnm)),
                      sep, how, svalue(cbExpid))
            dispose(DEenv$expw)
            return(invisible(NULL))
        }

        d <- DEenv$Data
        if(!svalue(cbExpid))
            d$id <- NULL
        if(DEenv$AppOpt$explbl == 2){
            attr(d, "variable.labels") <- sapply(DEenv$VarAttr, function(x) x$label)
        } else if(DEenv$AppOpt$explbl == 3){
            for(n in names(d))
                attr(d[[n]], "label") <- DEenv$VarAttr[[n]]$label
        }

        if(DEenv$AppOpt$exphow == 2){
            assign(svalue(edObnm), d)
            fn <- paste0(dirname(DEenv$fpath), "/", svalue(edFnm), ".RData")
            save(list = svalue(edObnm), file = fn)
            msg <- sprintf(gettext("Data exported to \"%s\"", domain = "R-DataEntry"), fn)
            gmessage(msg, type = "info")
        } else {
            assign(svalue(edObnm), d, envir = .GlobalEnv)
        }
        dispose(DEenv$expw)
    }


    addHandlerChanged(rdExphow, ShowHide)
    addHandlerClicked(btCancel, function(...) dispose(DEenv$expw))
    addHandlerClicked(btOk, onOKclick)
    ShowHide()
    visible(DEenv$expw) <- TRUE
    focus(btCancel)
}
