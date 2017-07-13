
IsNumericInt <- function(s, cls)
{
    oldwarn <- getOption("warn")
    on.exit(options(warn = oldwarn))
    options(warn = -1)
    ni <- try(as.numeric(s), silent = TRUE)
    options(warn = oldwarn)
    if(is.na(ni)){
        gmessage(sprintf(gettext("'%s' is not a valid numeric value.",
                                 domain = "R-DataEntry"), s), type = "warning")
        return(FALSE)
    }
    if(cls == "integer" && ni != try(as.integer(s), silent = TRUE)){
        gmessage(sprintf(gettext("'%s' is not a valid integer value.",
                                 domain = "R-DataEntry"), s), type = "warning")
        return(FALSE)
    }
    return(TRUE)
}

SetDefaultAppOpt <- function(clean = TRUE)
{
    if(clean)
        DEenv$AppOpt <- list()

    if(is.null(DEenv$AppOpt$bckopen))
        DEenv$AppOpt$bckopen <- TRUE
    if(is.null(DEenv$AppOpt$bcklast))
        DEenv$AppOpt$bcklast <- FALSE
    if(is.null(DEenv$AppOpt$nbcks))
        DEenv$AppOpt$nbcks <- 3
    if(is.null(DEenv$AppOpt$exphow))
        DEenv$AppOpt$exphow <- 1
    if(is.null(DEenv$AppOpt$expfct))
        DEenv$AppOpt$expfct <- 1
    if(is.null(DEenv$AppOpt$expsep))
        DEenv$AppOpt$expsep <- 1
    if(is.null(DEenv$AppOpt$explbl))
        DEenv$AppOpt$explbl <- 1
    if(is.null(DEenv$AppOpt$expid))
        DEenv$AppOpt$expid <- FALSE
}

SetCacheDir <- function()
{
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
        gmessage(gettext("Could not find the directory for saving the application options.",
                         domain = "R-DataEntry"), type = "warning")
        return(FALSE)
    }
    if(!dir.exists(DEenv$cachedir)){
        try(ok <- dir.create(DEenv$cachedir), silent = TRUE)
        if(!ok){
            gmessage(sprintf(gettext("Could not create the directory \"%s\" for saving the application options.",
                                     domain = "R-DataEntry"), DEenv$cachedir), type = "warning")
            return(FALSE)
        }
    }
    return(TRUE)
}

SaveAppOpt <- function()
{
    if(DEenv$cachedir == "" && !SetCacheDir())
        return(invisible(NULL))

    AppOpt <- DEenv$AppOpt
    save(AppOpt, file = paste0(DEenv$cachedir, "/AppOptions.RData"))
}

GetAppOpt <- function()
{
    if(DEenv$cachedir == "" && !SetCacheDir()){
        SetDefaultAppOpt(TRUE)
        return(invisible(NULL))
    }
    if(file.exists(paste0(DEenv$cachedir, "/AppOptions.RData"))){
        obj <- load(paste0(DEenv$cachedir, "/AppOptions.RData"), envir = DEenv)
        if(!identical(obj, "AppOpt")){
            gmessage("AppOptions.RData is corrupted", type = "warning")
            SetDefaultAppOpt(TRUE)
            return(invisible(NULL))
        }
    }
    # The AppOpt might have been saved by an outdated version of DataEntry
    SetDefaultAppOpt(FALSE)
}

SetDefaultProjOpt <- function()
{
    po <- list("droplist" = FALSE, "emptycell" = FALSE, "missv" = "NA")

    # Do not reset some options:
    if(!is.null(DEenv$ProjOpt$size.roww))
        po$size.roww <- DEenv$ProjOpt$size.roww
    if(!is.null(DEenv$ProjOpt$locked))
        po$locked <- DEenv$ProjOpt$locked
    else
        po$locked <- FALSE
    if(!is.null(DEenv$ProjOpt$md5))
        po$md5 <- DEenv$ProjOpt$md5

    DEenv$ProjOpt <- po
}

SaveProject <- function()
{
    Data <- DEenv$Data
    VarAttr <- DEenv$VarAttr
    id <- DEenv$id
    ProjOpt <- DEenv$ProjOpt
    DEversion <- packageVersion("DataEntry")

    save(Data, VarAttr, id, ProjOpt, DEversion, file = DEenv$fpath)
}

NewProject <- function()
{
    x <- capture.output(try(path <- gfile(type = "save",
                                          text = gettext("Type a name for the new project and click OK.",
                                                         domain = "R-DataEntry"),
                                          filter = list("DataEntry files" = list(patterns = "*.dte"))),
                            silent = TRUE))
    if(!exists("path") || length(path) == 0 || path == "")
        return(FALSE)

    if(Encoding(path) != "UTF-8")
        Encoding(path) <- "UTF-8"

    path <- paste0(path, ".dte")
    DEenv$fpath <- path
    DEenv$id <- as.integer(0)
    DEenv$Data <- data.frame(id = integer(0))
    DEenv$VarAttr <- list()
    DEenv$VarAttr[["id"]] <- list("label" = gettext("Internal variable (created automatically)", domain = "R-DataEntry"),
                                  "class" = "integer", "valid.values" = NA, "min" = NA, "max" = NA)

    SetDefaultProjOpt()
    SaveProject()
    if(is.null(DEenv$AppOpt))
        GetAppOpt()
    return(TRUE)
}

OpenProject <- function()
{
    x <- capture.output(try(path <- gfile(type = "open",
                                          text = gettext("Choose a project and click OK.",
                                                         domain = "R-DataEntry"),
                                          filter = list("DataEntry files" = list(patterns = "*.dte"))),
                            silent = TRUE))
    if(!exists("path") || length(path) == 0 || path == "")
        return(FALSE)

    if(Encoding(path) != "UTF-8")
        Encoding(path) <- "UTF-8"

    if(grepl("_backup_[0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9][0-9]\\.dte$", path)){
        gmessage(sprintf(gettext("This is a backup file.\nIf you really want to use it, remove \"%s\" from its name.",
                                 domain = "R-DataEntry"),
                         sub(".*_backup_(.*)\\.dte", "_backup_\\1", path)), type = "error")
        return(FALSE)
    }

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
    if(is.null(DEenv$ProjOpt$locked)){
        DEenv$ProjOpt$locked <- FALSE
        DEenv$ProjOpt$md5 <- ""
    }

    if(is.null(DEenv$AppOpt))
        GetAppOpt()

    # Backup the project in its current state
    if(DEenv$AppOpt$bckopen && grepl("\\.dte$", path)){
        DEenv$fpath <- paste0(sub("\\.dte$", "", path), "_backup_",
                              format(Sys.time(), "%y%m%d%H%M"), ".dte")
        SaveProject()
    }
    DEenv$fpath <- path

    # Delete old backups
    if(DEenv$AppOpt$bckopen && DEenv$AppOpt$bcklast){
        fdir <- dirname(DEenv$fpath)
        flist <- dir(fdir)
        flist <- flist[grep("_backup_[0-9]*\\.dte", flist)]
        if(length(flist) > DEenv$AppOpt$nbcks){
            flist <- sort(flist, decreasing = TRUE)
            flist <- flist[(DEenv$AppOpt$nbcks + 1):length(flist)]
            flist <- paste0(fdir, "/", flist)
            unlink(flist)
        }
    }

    return(TRUE)
}
