ExportDlg <- function(...)
{
    if(!is.null(DEenv$expw)){
        focus(DEenv$expw) <- TRUE
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

    if(is.null(DEenv$ProjOpt$obname)){
        obname <- NA
        try(obname <- abbreviate(sub("\\....$", "", basename(DEenv$fpath)), 5),
            silent = TRUE)
        if(is.na(obname))
            obname <- sub(".dte$", "", basename(DEenv$fpath))
        obname <- gsub("[[:punct:]]", ".", obname)
        obname <- gsub("[ \t]", ".", obname)
        DEenv$ProjOpt$obname <- obname
    }

    gon <- ggroup(container = g)
    lbObnm <- glabel(gettext("Object name:", domain = "R-DataEntry"), container = gon)
    edObnm <- gedit(DEenv$ProjOpt$obname, width = 20, container = gon)

    gfn <- ggroup(container = g)
    lbFnm <- glabel(gettext("File name:", domain = "R-DataEntry"), container = gfn)
    edFnm <- gedit(sub(".dte$", "", basename(DEenv$fpath)),
                   width = 20, container = gfn)

    gcsv <- ggroup(horizontal = FALSE, container = g)
    lbFac <- glabel(gettext("How to write factor variables?", domain = "R-DataEntry"),
                    anchor = c(-1, 1), container = gcsv)
    rdExpfct <- gradio(c(gettext("As character", domain = "R-DataEntry"),
                         gettext("As integer + R script", domain = "R-DataEntry"),
                         gettext("As integer + SPSS script", domain = "R-DataEntry")),
                       container = gcsv, selected = DEenv$AppOpt$expfct, horizontal = FALSE)
    lbSep <- glabel(gettext("What field separator should be used?", domain = "R-DataEntry"),
                    container = gcsv, anchor = c(-1, 1))
    rdExpsep <- gradio(c(gettext("Comma", domain = "R-DataEntry"),
                         gettext("Semicolon", domain = "R-DataEntry"),
                         gettext("Tab", domain = "R-DataEntry")),
                       container = gcsv, selected = DEenv$AppOpt$expsep, horizontal = FALSE)

    glbl <- ggroup(horizontal = FALSE, container = g)
    lbLbl <- glabel(gettext("What to do with variable labels?", domain = "R-DataEntry"),
                    container = glbl, anchor = c(-1, 1))
    rdExplbl <- gradio(c(gettext("Add a \"label\" attribute to each column", domain = "R-DataEntry"),
                         gettext("Add a \"variable.labels\" attribute to the data.frame", domain = "R-DataEntry"),
                         gettext("Nothing", domain = "R-DataEntry")),
                       container = glbl, selected = DEenv$AppOpt$explbl, horizontal = FALSE)

    addSpring(g)
    gbt <- ggroup(container = g)
    addSpring(gbt)
    btCancel <- gbutton(gettext("Cancel", domain = "R-DataEntry"), container = gbt)
    btOk <- gbutton(gettext("OK", domain = "R-DataEntry"), container = gbt)

    ShowHide <- function(...)
    {
        exphow <- svalue(rdExphow, index = TRUE)
        visible(gfn) <- FALSE
        visible(gon) <- FALSE
        visible(gcsv) <- FALSE
        visible(glbl) <- FALSE
        if(exphow < 3)
            visible(gfn) <- TRUE
        if(exphow > 1)
            visible(gon) <- TRUE
        if(exphow == 1)
            visible(gcsv) <- TRUE
        if(exphow > 1 || (exphow == 1 && svalue(rdExpfct, index = TRUE) == 2))
            visible(glbl) <- TRUE
    }

    onOKclick <- function(...)
    {
        DEenv$AppOpt$exphow <- svalue(rdExphow, index = TRUE)
        DEenv$AppOpt$expfct <- svalue(rdExpfct, index = TRUE)
        DEenv$AppOpt$expsep <- svalue(rdExpsep, index = TRUE)
        DEenv$AppOpt$explbl <- svalue(rdExplbl, index = TRUE)
        DEenv$AppOpt$expid  <- svalue(cbExpid)
        DEenv$ProjOpt$obname <- svalue(edObnm)

        SaveAppOpt()
        SaveProject()

        d <- DEenv$Data
        if(!svalue(cbExpid))
            d$id <- NULL
        if(DEenv$AppOpt$explbl == 1){
            for(n in names(d))
                attr(d[[n]], "label") <- DEenv$VarAttr[[n]]$label
        } else if(DEenv$AppOpt$explbl == 2){
            vlbs <- sapply(DEenv$VarAttr, function(x) x$label)
            vlbs <- vlbs[names(d)]
            attr(d, "variable.labels") <- vlbs
        }

        if(DEenv$AppOpt$exphow == 1){
            fname <- paste0(dirname(DEenv$fpath), "/", svalue(edFnm))
            sep <- c(",", ";", "\\t")[DEenv$AppOpt$expsep]
            how <- c("char", "R", "SPSS")[DEenv$AppOpt$expfct]
            keepid <- svalue(cbExpid)
            dispose(DEenv$expw)
            data.frame2csv(d, DEenv$ProjOpt$obname, fname, sep, how)
            if(how == "char")
                msg <- sprintf(gettext("Data exported to \"%s\".",
                                       domain = "R-DataEntry"),
                               paste0(fname, ".csv"))
            else
                msg <- sprintf(gettext("Data exported to \"%s\", and script generated as \"%s\".",
                                       domain = "R-DataEntry"),
                               paste0(fname, ".csv"),
                               paste0(fname, ifelse(how == "R", ".R", ".sps")))
            gmessage(msg, type = "info")
        } else if(DEenv$AppOpt$exphow == 2){
            assign(svalue(edObnm), d)
            fn <- paste0(dirname(DEenv$fpath), "/", svalue(edFnm), ".RData")
            save(list = svalue(edObnm), file = fn)
            msg <- sprintf(gettext("Data exported to \"%s\"", domain = "R-DataEntry"), fn)
            dispose(DEenv$expw)
            gmessage(msg, type = "info")
        } else {
            assign(svalue(edObnm), d, envir = .GlobalEnv)
            dispose(DEenv$expw)
        }
    }


    addHandlerChanged(rdExphow, ShowHide)
    addHandlerChanged(rdExpfct, ShowHide)
    addHandlerClicked(btCancel, function(...) dispose(DEenv$expw))
    addHandlerClicked(btOk, onOKclick)
    ShowHide()
    visible(DEenv$expw) <- TRUE
    focus(btCancel) <- TRUE
}
