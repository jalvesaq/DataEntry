
data.frame2csv <- function(x, fname, fieldsep, how)
{
    x.names <- names(x)

    if(how == "R"){
        sink(paste0(fname, ".R"))
        cat('d <- read.table("', fname, '.csv", sep = "', fieldsep,
            '", header = TRUE, as.is = TRUE)\n\n', sep = "")
        for(column in x.names){
            xx <- x[[column]]
            if(is.factor(xx)){
                xx.levels <- gsub('"', '\\\\"', levels(xx))
                n.levels <- length(xx.levels)
                cat("d", "$", column, " <- factor(", "d", "$", column,
                    ", levels = 1:", n.levels, ',\n  labels = c("', sep = "")
                cat(xx.levels[1], '"', sep = "")
                if(n.levels > 1) cat(", ")
                i <- 2
                len <- 2
                n.levels1 <- n.levels - 1
                while(i < n.levels){
                    len <- len + nchar(xx.levels[i]) + 4
                    if(len > 80){
                        cat("\n    ")
                        len <- nchar(xx.levels[i]) + 6
                    }
                    cat('"', xx.levels[i], '", ', sep = "")
                    i <- i + 1
                }
                if(len > 80) cat("\n  ")
                if(n.levels > 1) cat('"', xx.levels[n.levels], '"', sep = "")
                cat("))\n")
            }
        }
        for(column in x.names){
            xx <- x[[column]]
            xx.label <- attr(xx, "label")
            if(!is.null(xx.label)){
                cat("attr(", "d", "$", column, ', "label") <- "', xx.label,
                    '"\n', sep = "")
            }
        }
        cat("save(", "d", ", file = \"", fname, ".RData\")\n", sep = "")
        sink()
    } else if(how == "SPSS"){
        sink(paste0(fname, ".sps"))
        cat("GET DATA\n")
        cat("  /TYPE=TXT\n")
        cat("  /FILE='", fname, ".csv", "'\n", sep = "")
        cat("  /DELCASE=LINE\n")
        cat("  /DELIMITERS=\"", fieldsep, "\"\n", sep = "")
        cat("  /ARRANGEMENT=DELIMITED\n")
        cat("  /FIRSTCASE=2\n")
        cat("  /IMPORTCASE=ALL\n")
        cat("  /VARIABLES=\n")
        for(column in x.names){
            cat("  ", column, " ", sep = "")
            xx <- x[[column]]
            if(is.character(xx)) cat("A", max(nchar(xx)), "\n", sep = "")
            else if(is.factor(xx)){
                nlevs <- length(levels(xx))
                if(nlevs < 10) cat("F1.0\n")
                else if(nlevs > 9 && nlevs < 100) cat("F2.0\n")
                else if(nlevs > 99) cat("F3.0\n")
            } else if(is.numeric(xx)){
                if(sum(grepl("(chron|dates|times)", class(xx))) > 0){
                    cat("A", max(nchar(as.character(xx))), "\n", sep = "")
                } else {
                    cat("F", max(nchar(as.character(xx))), ".0\n", sep = "")
                }
            } else cat("error: undefined type\n")
        }
        cat("  .\n")
        cat("EXECUTE.\n\n")

        for(column in x.names){
            xx <- x[[column]]
            xx.label <- attr(xx, "label")
            if(!is.null(xx.label))
                cat("VARIABLE LABELS ", column, ' "', xx.label, '" .\n', sep = "")
        }
        cat("\n")

        for(column in x.names){
            xx <- x[[column]]
            if(is.factor(xx)){
                cat("VALUE LABELS ", column, "\n", sep = "")
                xx.levels <- levels(xx)
                len <- length(xx.levels)
                for(i in 1:len){
                    if(i < len){
                        cat("  ", i, ' "', xx.levels[i], '"\n', sep = "")
                    } else {
                        cat("  ", i, ' "', xx.levels[i], '" .\n', sep = "")
                    }
                }
                cat("\n")
            }
        }
        cat("SAVE OUTFILE='", fname, ".sav'\n  /COMPRESSED.\n", sep = "")
        sink()
    }

    if(how != "char"){
        for(column in x.names)
            if(is.factor(x[[column]])) x[[column]] <- as.numeric(x[[column]])
    }

    if(fieldsep == "\\t")
        fieldsep <- "\t"
    write.table(x, file = paste0(fname, ".csv"), sep = fieldsep,
                col.names = TRUE, row.names = FALSE, na = "")
}
