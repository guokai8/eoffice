##' get the suffix name
.getext<-function (x)
{
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
}

##' set first row as column name
##' @importFrom utils type.convert
##' @param file input data frame
##' @author Kai Guo
##' @export
.setcolnames <- function(file){
    names(file) <- as.matrix(file[1, ])
    file <- file[-1, ]
    file[] <- lapply(file, function(x) type.convert(as.character(x)))
    file
}
