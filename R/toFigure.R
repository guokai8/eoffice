##' output figures to different formats
##' @name tofigure
##' @importFrom R.devices devEval
##' @importFrom grDevices dev.off
##' @importFrom devEMF emf
##' @importFrom grDevices recordPlot
##' @param figure output figure function, set NULL output the current figure
##' @param format output format (could be ingored)
##' @param filename output filename with different format as suffix
##' @examples
##' \dontrun{
##' p <- ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point()
##' tofigure(p,filename = "mtcars.pdf")
##' ## or use ggplot directly
##' tofigure(ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) +
##' geom_point(), filename = "mtcars.eps")
##' ## if you use basic plot function or other plot function you
##' ## need first use convertplot to convert it to ggplot object
##' ## when you are working on the platform without GUI
##' p <- convertplot(plot(1:10))
##' tofigure(p, filename = "test.pdf")
##' topptx(p, filename = "test.pptx")
##' }
##' @author Kai Guo
##' @export
tofigure <- function(figure, format = NULL, filename= "temp.pdf"){
    if(is.null(format)){
        format = .getext(filename)
    }
    path = dirname(filename)
    ofile = filename
    filename = .getfilename(filename)
    format = tolower(format)
    if(format == "emf"){
        if(missing(figure)){
            emf(file=ofile)
            p <- recordPlot()
            print(p)
            dev.off()
        }else{
            emf(file=ofile)
            print(figure)
            dev.off()
        }
    }else{
        if(missing(figure)){
            devEval(type = format, name = filename, path = path)
        }else{
            devEval(type = format, name = filename, path = path,
                    expr = {print(figure)})
        }
        }
}
