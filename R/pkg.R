
DEenv <- new.env()

.onAttach <- function(...){
    if(interactive()){
        startnow <- FALSE
        if(is.null(getOption("DataEntry.autostart"))){
            startnow <- TRUE
        } else {
            if(getOption("DataEntry.autostart") == TRUE)
                startnow <- TRUE
        }
        if(startnow){
            packageStartupMessage(paste0("\n",
                                         gettext("If necessary, type 'DataEntry()' to start the application again.",
                                                 domain = "R-DataEntry"), "\n"))
            DataEntry()
        }
    }
}
