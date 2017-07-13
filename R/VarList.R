
LockIt <- function(h, ...)
{
    DEenv$ProjOpt$md5 <- digest(h$input)
    DEenv$ProjOpt$locked <- TRUE
    enabled(DEenv$btEdit) <- FALSE
    enabled(DEenv$btAdd) <- FALSE
    enabled(DEenv$btDelete) <- FALSE
    svalue(DEenv$btLock) <- gettext("Unlock", domain = "R-DataEntry")
    SaveProject()
}

UnlockIt <- function(h, ...)
{
    md5 <- digest(h$input)
    if(md5 == DEenv$ProjOpt$md5){
        DEenv$ProjOpt$locked <- FALSE
        enabled(DEenv$btEdit) <- TRUE
        enabled(DEenv$btAdd) <- TRUE
        enabled(DEenv$btDelete) <- TRUE
        svalue(DEenv$btLock) <- gettext("Lock", domain = "R-DataEntry")
        SaveProject()
    } else {
        gmessage(gettext("The password does not match the registered one.",
                 domain = "R-DataEntry"), type = "warning")
    }
}

LockUnlock <- function(...)
{
    if(DEenv$ProjOpt$locked){
        ginput("Enter the password to edit the list of variables",
               handler = UnlockIt)
    } else {
        ginput("Enter the password to lock the list of variables",
               handler = LockIt)
    }
}

UpdateVarList <- function()
{
    DEenv$vlist[,] <- data.frame(Var = names(DEenv$Data),
                                 stringsAsFactors = FALSE)
    if(!is.null(DEenv$dfview))
        UpdateDFView()
}

DeleteVariable <- function(...)
{
    n <- svalue(DEenv$vlist)
    if(n == "id"){
        gmessage(gettext("Cannot delete 'id'.", domain = "R-DataEntry"),
                 type = "warning")
        return(invisible(NULL))
    }
    DEenv$Data <- DEenv$Data[, names(DEenv$Data) != n]
    DEenv$VarAttr <- DEenv$VarAttr[names(DEenv$VarAttr) != n]
    UpdateVarList()
    svalue(DEenv$vlist) <- "id"
    SaveProject()
}

ShowAttributes <- function(...)
{
    n <- svalue(DEenv$vlist)
    if(length(n) == 0)
        return(invisible(NULL))
    l <- DEenv$VarAttr[[n]]
    svalue(DEenv$aNm) <- n
    svalue(DEenv$aLb) <- l$label
    svalue(DEenv$aCl) <- l$class
    if(is.na(l$min))
        svalue(DEenv$aMi) <- ""
    else
        svalue(DEenv$aMi) <- as.character(l$min)
    if(is.na(l$max))
        svalue(DEenv$aMa) <- ""
    else
        svalue(DEenv$aMa) <- as.character(l$max)
    if(is.na(l$valid.values[1]))
        svalue(DEenv$aVv) <- ""
    else
        svalue(DEenv$aVv) <- as.character(l$valid.values)
}

MoveUpDown <- function(up)
{
    nm <- svalue(DEenv$vlist)
    focus(DEenv$varw)
    if(length(nm) == 0){
        gmessage(gettext("No variable is selected.",
                         domain = "R-DataEntry"), type = "warning")
        return(invisible(NULL))
    }


    s <- grep(nm, names(DEenv$VarAttr))
    len <- length(DEenv$VarAttr)

    if((up && s == 1) || (!up && s == len))
        return(invisible(NULL))

    if((up && s == 2) || (!up && nm == "id")){
        gmessage(gettext("Cannot move 'id'.", domain = "R-DataEntry"),
                 type = "warning")
        return(invisible(NULL))
    }

    if(up){
        i <- c(1:(s - 2), s, s - 1)
        if(s < len)
            i <- c(i, (s + 1):len)
    } else {
        i <- integer(0)
        if(s > 1)
            i <- 1:(s - 1)
        i <- c(i, s + 1, s)
        if(len > (s + 1))
            i <- c(i, (s + 2):len)
    }

    DEenv$Data <- DEenv$Data[, names(DEenv$Data)[i]]
    DEenv$VarAttr <- DEenv$VarAttr[names(DEenv$VarAttr)[i]]
    UpdateVarList()
    Sys.sleep(0.1)
    svalue(DEenv$vlist, index = TRUE) <- ifelse(up, s-1, s+1)
    focus(DEenv$vlist)
    SaveProject()
}

VarListDlg <- function(...)
{
    if(!is.null(DEenv$varw)){
        focus(DEenv$varw)
        return(invisible(NULL))
    }
    onDestroy <- function(...)
    {
        if(!is.null(DEenv$attrw))
            dispose(DEenv$attrw)
        rm(list = c("varw", "vlist", "aNm", "aLb", "aCl", "aMi", "aMa", "aVv"),
           envir = DEenv)
    }

    DEenv$varw <- gwindow(gettext("Attributes of variables", domain = "R-DataEntry"),
                          handler = onDestroy, visible = FALSE)
    g <- ggroup(horizontal = TRUE, container = DEenv$varw)
    g1 <- ggroup(horizontal = FALSE, container = g)
    g2 <- ggroup(horizontal = FALSE, container = g)
    g3 <- ggroup(horizontal = FALSE, container = g, expand = TRUE)
    DEenv$vlist <- gtable(names(DEenv$Data), container = g1, expand = TRUE)
    names(DEenv$vlist) <- gettext("Variables", domain = "R-DataEntry")
    g1b <- ggroup(horizontal = TRUE, container = g1)
    addSpring(g1b)
    DEenv$btEdit <- gbutton(gettext("Edit", domain = "R-DataEntry"), container = g1b)
    DEenv$btDelete <- gbutton(gettext("Delete", domain = "R-DataEntry"), container = g1b)
    DEenv$btAdd <- gbutton(gettext("Add", domain = "R-DataEntry"), container = g1b)

    addSpring(g2)
    btUp <- gbutton("^", container = g2)
    addSpace(g2, 10)
    btDown <- gbutton("v", container = g2)
    addSpring(g2)

    DEenv$aNm <- glabel(anchor = c(-1, 1))
    DEenv$aLb <- glabel(anchor = c(-1, 1))
    DEenv$aCl <- glabel(anchor = c(-1, 1))
    DEenv$aMi <- glabel(anchor = c(-1, 1))
    DEenv$aMa <- glabel(anchor = c(-1, 1))
    DEenv$aVv <- glabel(anchor = c(-1, 1))

    # FIXME: The labels are center aligned
    f <- gframe(gettext("Attributes", domain = "R-DataEntry"), horizontal = FALSE, container = g3)
    lt <- glayout(container = f)
    lt[1, 1] <- gettext("Name:", domain = "R-DataEntry")
    lt[1, 2] <- DEenv$aNm
    lt[2, 1] <- gettext("Label:", domain = "R-DataEntry")
    lt[2, 2] <- DEenv$aLb
    lt[3, 1] <- gettext("Class:", domain = "R-DataEntry")
    lt[3, 2] <- DEenv$aCl
    lt[4, 1] <- gettext("Min:", domain = "R-DataEntry")
    lt[4, 2] <- DEenv$aMi
    lt[5, 1] <- gettext("Max:", domain = "R-DataEntry")
    lt[5, 2] <- DEenv$aMa
    lt[6, 1] <- gettext("Valid values:", domain = "R-DataEntry")
    lt[6, 2] <- DEenv$aVv

    addSpring(g3)
    g2b <- ggroup(horizontal = TRUE, container = g3)
    addSpring(g2b)
    DEenv$btLock <- gbutton(gettext("Lock", domain = "R-DataEntry"), container = g2b)
    btClose <- gbutton(gettext("Close", domain = "R-DataEntry"), container = g2b)

    addHandlerClicked(DEenv$btEdit, function(...) AttrDlg(FALSE))
    addHandlerClicked(DEenv$btAdd, function(...) AttrDlg(TRUE))
    addHandlerClicked(DEenv$btDelete, DeleteVariable)
    addHandlerSelect(DEenv$vlist, ShowAttributes)
    addHandlerClicked(btUp, function(...) MoveUpDown(TRUE))
    addHandlerClicked(btDown, function(...) MoveUpDown(FALSE))
    addHandlerClicked(DEenv$btLock, function(...) LockUnlock())
    addHandlerClicked(btClose, function(...) dispose(DEenv$varw))

    svalue(DEenv$vlist) <- "id"
    focus(DEenv$varw)
    if(DEenv$ProjOpt$locked){
        enabled(DEenv$btEdit) <- FALSE
        enabled(DEenv$btAdd) <- FALSE
        enabled(DEenv$btDelete) <- FALSE
        svalue(DEenv$btLock) <- gettext("Unlock", domain = "R-DataEntry")
    }
    visible(DEenv$varw) <- TRUE
}
