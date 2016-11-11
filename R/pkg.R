
DEenv <- new.env()

.onAttach <- function(...){
    if(interactive()){
        packageStartupMessage(paste0("\n",
                                     gettext("If necessary, type 'DataEntry()' to start the application again.",
                                             domain = "R-DataEntry"), "\n"))
        DataEntry()
    }
}
