
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
    if("expw" %in% ls(DEenv)){
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
    exphow <- gradio(c(gettext("Write CSV file", domain = "R-DataEntry"),
                       gettext("Write RData file", domain = "R-DataEntry"),
                       gettext("Copy to R Workspace", domain = "R-DataEntry")),
                     selected = DEenv$AppOpt$exphow, horizontal = FALSE, container = g)
    expid <- gcheckbox(gettext("Include the column \"id\"",
                       domain = "R-DataEntry"), checked = DEenv$AppOpt$expid, container = g)
    gn <- ggroup(horizontal = FALSE, container = g)

    gfn <- ggroup()
    nm <-  sub(".dte$", "", basename(DEenv$fpath))
    fnlbl <- glabel(gettext("File name:", domain = "R-DataEntry"), container = gfn)
    fnedit <- gedit(nm, width = 20, container = gfn)

    gon <- ggroup()
    nm <- gsub("[[:punct:]]", ".", nm)
    nm <- gsub("[ \t]", ".", nm)
    oblbl <- glabel(gettext("Object name:", domain = "R-DataEntry"), container = gon)
    obedit <- gedit(nm, width = 20, container = gon)

    gcsv <- ggroup(horizontal = FALSE, container = g)
    faclbl <- glabel(gettext("How to write factor variables?",
                             domain = "R-DataEntry"), anchor = c(-1, 1))
    expfct <- gradio(c(gettext("As character", domain = "R-DataEntry"),
                       gettext("As integer + R script", domain = "R-DataEntry"),
                       gettext("As integer + SPSS script", domain = "R-DataEntry")),
                     selected = DEenv$AppOpt$expfct, horizontal = FALSE)

    seplbl <- glabel(gettext("What field separator should be used?",
                             domain = "R-DataEntry"), anchor = c(-1, 1))
    expsep <- gradio(c(gettext("Comma", domain = "R-DataEntry"),
                       gettext("Semicolon", domain = "R-DataEntry"),
                       gettext("Tab", domain = "R-DataEntry")),
                     selected = DEenv$AppOpt$expsep, horizontal = FALSE)

    grda <- ggroup(horizontal = FALSE, container = g)
    lbllbl <- glabel(gettext("What to do with variable labels?",
                             domain = "R-DataEntry"))
    explbl <- gradio(c(gettext("Nothing", domain = "R-DataEntry"),
                       gettext("Add a \"variable.labels\" attribute to the data.frame", domain = "R-DataEntry"),
                       gettext("Add a \"label\" attribute to each column", domain = "R-DataEntry")),
                     selected = DEenv$AppOpt$explbl, horizontal = FALSE)

    addSpring(g)
    gbt <- ggroup(container = g)
    addSpring(gbt)
    btCnc <- gbutton(gettext("Cancel",
                             domain = "R-DataEntry"), container = gbt)
    btOk <- gbutton(gettext("OK",
                            domain = "R-DataEntry"), container = gbt)

    ShowHide <- function(...)
    {
        delete(gn, gfn)
        delete(gn, gon)
        delete(gcsv, faclbl)
        delete(gcsv, expfct)
        delete(gcsv, seplbl)
        delete(gcsv, expsep)
        delete(grda, lbllbl)
        delete(grda, explbl)
        if(svalue(exphow, index = TRUE) < 3)
            add(gn, gfn)
        if(svalue(exphow, index = TRUE) > 1)
            add(gn, gon)
        if(svalue(exphow, index = TRUE) == 1){
            add(gcsv, faclbl, anchor = c(-1, 1))
            add(gcsv, expfct)
            add(gcsv, seplbl, anchor = c(-1, 1))
            add(gcsv, expsep)
        } else {
            add(grda, lbllbl, anchor = c(-1, 1))
            add(grda, explbl)
        }
    }

    onOKclick <- function(...)
    {
        DEenv$AppOpt$exphow <- svalue(exphow, index = TRUE)
        DEenv$AppOpt$expfct <- svalue(expfct, index = TRUE)
        DEenv$AppOpt$expsep <- svalue(expsep, index = TRUE)
        DEenv$AppOpt$explbl <- svalue(explbl, index = TRUE)

        SaveAppOpt()

        if(DEenv$AppOpt$exphow == 1){
            sep <- c(",", ";", "\\t")[DEenv$AppOpt$expsep]
            how <- c("char", "R", "SPSS")[DEenv$AppOpt$expfct]
            ExportCSV(paste0(dirname(DEenv$fpath), "/", svalue(fnedit)),
                      sep, how, svalue(expid))
            dispose(DEenv$expw)
            return(invisible(NULL))
        }

        d <- DEenv$Data
        if(!svalue(expid))
            d$id <- NULL
        if(DEenv$AppOpt$explbl == 2){
            attr(d, "variable.labels") <- sapply(DEenv$VarAttr, function(x) x$label)
        } else if(DEenv$AppOpt$explbl == 3){
            for(n in names(d))
                attr(d[[n]], "label") <- DEenv$VarAttr[[n]]$label
        }

        if(DEenv$AppOpt$exphow == 2){
            assign(svalue(obedit), d)
            fn <- paste0(dirname(DEenv$fpath), "/", svalue(fnedit), ".RData")
            save(list = svalue(obedit), file = fn)
            msg <- sprintf(gettext("Data exported to \"%s\"", domain = "R-DataEntry"), fn)
            gmessage(msg, type = "info")
        } else {
            assign(svalue(obedit), d, envir = .GlobalEnv)
        }
        dispose(DEenv$expw)
    }


    addHandlerChanged(exphow, ShowHide)
    addHandlerClicked(btCnc, function(...) dispose(DEenv$expw))
    addHandlerClicked(btOk, onOKclick)
    ShowHide()
    visible(DEenv$expw) <- TRUE
    focus(btCnc)
}
