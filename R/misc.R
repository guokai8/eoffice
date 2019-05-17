##' get the suffix name
.getext <- function(x)
{
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
}
##' get the prefix name
.getfilename <- function(x){
    filename <- basename(x)
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, 1, pos - 1L), x)
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
##' convert basic plot to ggplot object
##' @importFrom ggplotify as.grob
##' @importFrom ggplotify as.ggplot
##' @param expr expression formula of basic plot
##' @return ggplot object
##' @author Kai Guo
##' @export
convertplot <- function(exp){
    exp <- substitute(exp)
    p <- as.grob(formula(paste0('~',deparse(exp))))
    p <- as.ggplot(p)
    if(file.exists("Rplots.pdf")){
        file.remove("Rplots.pdf")
    }
    return(p)
}


