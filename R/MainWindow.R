
DEenv <- new.env()
DEenv$DEversion <- "0.1"

SetDefaultAppOpt <- function()
{
    DEenv$AppOpt <- list("bckopen" = TRUE, "bcklast" = FALSE, "nbcks" = 3)
}

SetDefaultProjOpt <- function()
{
    if(is.null(DEenv$ProjOpt$size.roww)){
        DEenv$ProjOpt <- list("droplist" = FALSE, "emptycell" = FALSE, "missv" = "NA")
    } else {
        DEenv$ProjOpt <- list("droplist" = FALSE, "emptycell" = FALSE,
                              "missv" = "NA", "size.roww" = DEenv$ProjOpt$size.roww)
    }
}

SaveAppOpt <- function()
{
    if(DEenv$cachedir != "" && dir.exists(DEenv$cachedir)){
        AppOpt <- DEenv$AppOpt
        save(AppOpt, file = paste0(DEenv$cachedir, "/AppOptions.RData"))
    }
}

# Get the application options
DEenv$cachedir <- ""
if(.Platform$OS.type == "windows" && Sys.getenv("APPDATA") != "" && dir.exists(Sys.getenv("APPDATA"))){
    DEenv$cachedir <- paste0(gsub("\\\\", "/", Sys.getenv("APPDATA")), "/DataEntry")
} else if(Sys.getenv("XDG_CACHE_HOME") != "" && dir.exists(Sys.getenv("XDG_CACHE_HOME"))){
    DEenv$cachedir <- paste0(Sys.getenv("XDG_CACHE_HOME"), "/DataEntry")
} else if(dir.exists(paste0(Sys.getenv("HOME"), "/.cache"))){
    DEenv$cachedir <- paste0(Sys.getenv("HOME"), "/.cache/DataEntry")
} else if(dir.exists(paste0(Sys.getenv("HOME"), "/Library/Caches"))){
    DEenv$cachedir <- paste0(Sys.getenv("HOME"), "/Library/Caches/DataEntry")
}

if(DEenv$cachedir == ""){
    SetDefaultAppOpt()
} else {
    if(!dir.exists(DEenv$cachedir))
        try(dir.create(DEenv$cachedir), silent = TRUE)
    if(dir.exists(DEenv$cachedir)){
        if(file.exists(paste0(DEenv$cachedir, "/AppOptions.RData"))){
            obj <- load(paste0(DEenv$cachedir, "/AppOptions.RData"), envir = DEenv)
            if(!identical(obj, "AppOpt")){
                gmessage("AppOptions.RData is corrupted")
            }
        } else {
            SetDefaultAppOpt()
            SaveAppOpt()
        }
    } else {
        DEenv$cachedir <- ""
        SetDefaultAppOpt()
    }
}

if(is.null(DEenv$AppOpt))
    SetDefaultAppOpt()

# The AppOpt might have been saved by an outdated version of DataEntry
if(is.null(DEenv$AppOpt$bckopen))
    DEenv$AppOpt$bckopen <- TRUE
if(is.null(DEenv$AppOpt$bcklast))
    DEenv$AppOpt$bcklast <- FALSE
if(is.null(DEenv$AppOpt$nbcks))
    DEenv$AppOpt$nbcks <- 3

SaveProject <- function()
{
    Data <- DEenv$Data
    VarAttr <- DEenv$VarAttr
    id <- DEenv$id
    ProjOpt <- DEenv$ProjOpt
    DEversion <- DEenv$DEversion

    save(Data, VarAttr, id, ProjOpt, DEversion, file = DEenv$fpath)
}

NewProject <- function()
{
    x <- capture.output(try(path <- gfile(type = "save",
                                          text = gettext("Type a name for the new project and click OK.", domain = "R-DataEntry"),
                                          filter = list("DataEntry files" = list(patterns = "*.dte"))),
                            silent = TRUE))
    if(!exists("path"))
        return(FALSE)
    if(path == "")
        return(FALSE)

    if(Encoding(path) != "UTF-8")
        Encoding(path) <- "UTF-8"

    path <- paste0(path, ".dte")
    DEenv$fpath <- path
    DEenv$id <- 0
    DEenv$Data <- data.frame(id = integer(0))
    DEenv$VarAttr <- list()
    DEenv$VarAttr[["id"]] <- list("label" = gettext("Internal variable (created automatically)", domain = "R-DataEntry"),
                                  "class" = "integer",
                                  "valid.values" = NA,
                                  "min" = NA, "max" = NA)
    SetDefaultProjOpt()
    SaveProject()
    return(TRUE)
}

OpenProject <- function()
{
    x <- capture.output(try(path <- gfile(type = "open",
                                          text = gettext("Choose a project and click OK.", domain = "R-DataEntry"),
                                          filter = list("DataEntry files" = list(patterns = "*.dte"))),
                            silent = TRUE))
    if(!exists("path"))
        return(FALSE)
    if(path == "")
        return(FALSE)

    if(Encoding(path) != "UTF-8")
        Encoding(path) <- "UTF-8"

    objs <- load(path, envir = DEenv)

    # Check if the project is OK (it might have been manually edited):
    if(length(objs) != 5){
        gmessage("The project is corrupt: the number of objects should be 5.")
        rm(list = objs, envir = DEenv)
        return(FALSE)
    }
    if(sum(objs %in% c("Data", "id", "VarAttr", "DEversion", "ProjOpt")) != 5){
        gmessage("The project is corrupt: not all expected objects found.")
        rm(list = objs, envir = DEenv)
        return(FALSE)
    }
    if(!identical(names(DEenv$Data), names(DEenv$VarAttr))){
        gmessage("The project is corrupt: data.frame columns and attributes names differ.")
        rm(list = objs, envir = DEenv)
        return(FALSE)
    }

    if(is.null(DEenv$ProjOpt))
        SetDefaultProjOpt()

    # The Project options might have been saved by an outdated version of DataEntry
    if(is.null(DEenv$ProjOpt$droplist))
        DEenv$AppOpt$droplist <- FALSE
    if(is.null(DEenv$ProjOpt$emptycell))
        DEenv$AppOpt$emptycell <- FALSE
    if(is.null(DEenv$ProjOpt$droplist))
        DEenv$AppOpt$missv <- "NA"

    # Backup the project in its current state
    if(grepl("\\.dte$", path)){
        DEenv$fpath <- paste0(sub("\\.dte$", "", path), "_backup_",
                              format(Sys.time(), "%y%m%d%H%M"), ".dte")
        SaveProject()
    }

    DEenv$fpath <- path
    return(TRUE)
}

ExportProject <- function(...)
{
    cat("ExportProject\n")
}

DataEntry <- function()
{
    if("mainw" %in% ls(DEenv)){
        focus(DEenv$mainw)
        return(invisible(NULL))
    }

    onDestroy <- function(...)
    {
        if("varw" %in% ls(DEenv))
            dispose(DEenv$varw)
        if("dataw" %in% ls(DEenv))
            dispose(DEenv$dataw)
        if("optw" %in% ls(DEenv))
            dispose(DEenv$optw)
        rm(list = ls(DEenv), envir = DEenv)
    }

    DEenv$mainw <- gwindow("DataEntry",
                           handler = onDestroy, visible = FALSE)
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

