##' get the suffix name
##' @param x filename
.getext <- function(x)
{
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
}
##' get the prefix name
##' @param x filename
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
##' @importFrom stats formula
##' @param exp expression formula of basic plot
##' @return ggplot object
##' @examples
##' \donttest{
##' p <- convertplot(plot(1:10))
##' class(p)
##' print(p)
##' }
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

##' blank theme
##' @importFrom ggplot2 theme
##' @importFrom ggplot2 element_blank
##'
.theme_blank<-function(){
    theme(panel.border=element_blank(),panel.grid=element_blank(),
        panel.background=element_blank(),axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),legend.position="none")
}
##' col2hex gplots
##' @importFrom grDevices rgb
##' @importFrom grDevices col2rgb
##' @param cname color name
col2hex <- function (cname)
{
    colMat <- col2rgb(cname)
    rgb(red = colMat[1, ]/255, green = colMat[2, ]/255, blue = colMat[3,
                                                                      ]/255)
}
