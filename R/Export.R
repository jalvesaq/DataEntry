
ExportDlg <- function(...)
{
    fn <- sub(".dte$", ".csv", DEenv$fpath)
    d <- DEenv$Data
    d$id <- NULL
    write.table(d, file = fn, sep = ",", row.names = FALSE)
    gmessage(sprintf(gettext("Data exported to \"%s\"", domain = "R-DataEntry"), fn), type = "info")
}
