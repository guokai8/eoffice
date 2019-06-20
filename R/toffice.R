##' @title export graph to MS office
##' @name toffice
##' @importFrom officer read_docx
##' @importFrom officer read_pptx
##' @importFrom magrittr %>%
##' @importFrom officer add_slide
##' @importFrom rvg ph_with_vg
##' @importFrom rvg body_add_vg
##' @importFrom grDevices recordPlot
##' @importFrom grDevices dev.cur hcl
##' @param figure plot figure function
##' @param format file format
##' @param filename output filename
##' @param append append or not
##' @param width width of the output figure
##' @param height height of the output figure
##' @param devsize Boolean value show use device size or not (default = FALSE)
##' @param units the units in which to return the value – inches, cm, or pixels (device units)
##' @examples
##' if(interactive()){
##' plot(mtcars$mpg, mtcars$disp)
##' toffice(filename = file.path(tempdir(), "mtcars.pptx"), format = "pptx")
##' ## use ggplot2
##' ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point()
##' toffice(filename = file.path(tempdir(), "mtcars.pptx"),format = "pptx")
##' }
##' @author Kai Guo
##' @export
toffice <- function(figure = NULL, format = "pptx", filename= "temp.pptx",
                    append = FALSE, width = 4, height = 4, devsize = FALSE,
                    units = "in"){
    format = tolower(format)
    if (format == "ppt" | format == "pptx") {
        format = "ppt"
    }
    if (format == "doc" | format == "docx") {
        format = "doc"
    }
    if(is.null(figure)){
        if (dev.cur() == 1)
            stop("No graphics device found")
        p <- recordPlot()
    }else{
        if(is.function(figure)){
            p <- figure()
        }else{
            p <- figure
        }
    }
    if(isTRUE(devsize)){
        width = dev.size(units)[1]
        height = dev.size(units)[2]
    }
    if(format == "ppt"){
        if(isTRUE(append)){
            if(file.exists(filename)){
                doc <- read_pptx(filename)
            }else{
                doc <- read_pptx()
                doc <- add_slide(doc,"Title and Content", "Office Theme")
                print(doc,target=filename)
            }
        }else{
            doc <- read_pptx()
        }
        doc <- add_slide(doc,"Title and Content", "Office Theme")
        doc <- ph_with_vg(doc, print(p), width = width, height = height)
        print(doc,target=filename)
    }
    if(format == "doc"){
        if(isTRUE(append)){
            if(file.exists(filename)){
                doc <- read_docx(filename)
            }else{
                doc <- read_docx()
                print(doc,target=filename)
            }
        }else{
            doc <- read_docx()
        }
        doc <- body_add_vg(doc, print(p), width = width, height = height)
        print(doc,target=filename)
    }
}

##' export figure to pptx
##' @name topptx
##' @param figure plot figure function
##' @param filename output filename
##' @param width width of the output figure
##' @param height height of the output figure
##' @param append append or not
##' @param devsize Boolean value show use device size or not (default = FALSE)
##' @param units the units in which to return the value – inches, cm, or pixels (device units)
##' @examples
##' if(interactive()){
##' plot(mtcars$mpg, mtcars$disp)
##' topptx(filename = file.path(tempdir(), "mtcars.pptx"))
##' ## use ggplot2
##' ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point()
##' topptx(filename = file.path(tempdir(), "mtcars.pptx"))
##' }
##' @author Kai Guo
##' @export
topptx <- function(figure = NULL, filename = NULL, width = 6, height = 6,
        append = FALSE, devsize = FALSE, units = "in"){
    toffice(figure = figure, filename = filename, format = "pptx",
            width = width, height = height, append = append, devsize = devsize,
            units = units)
}
##' export figure to docx
##' @name todocx
##' @param figure plot figure function
##' @param filename output filename
##' @param width width of the output figure
##' @param height height of the output figure
##' @param append append or not
##' @param devsize Boolean value show use device size or not (default = FALSE)
##' @param units the units in which to return the value – inches, cm, or pixels (device units)
##' @examples
##' if(interactive()){
##' plot(mtcars$mpg, mtcars$disp)
##' todocx(filename = file.path(tempdir(), "mtcars.docx"))
##' ## use ggplot2
##' ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point()
##' todocx(filename = file.path(tempdir(), "mtcars.docx"), height = 6, width = 4)
##' }
##' @author Kai Guo
##' @export
todocx <- function(figure =NULL, filename = NULL, width = 6, height = 6,
                    append = FALSE, devsize = FALSE, units = "in"){
    toffice(figure = figure, filename = filename, format = "docx",
            width = width, height = height,append = append, devsize = devsize,
            units = units)
}

