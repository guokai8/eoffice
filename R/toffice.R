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
##' @author Kai Guo
##' @export
toffice <- function(figure = NULL, format = "pptx", filename= "temp.pptx",
                    append = FALSE, width = 4, height = 4){
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
##' @author Kai Guo
##' @export
topptx <- function(figure = NULL, filename = NULL, width = 6, height = 6, append = FALSE){
    toffice(figure = figure, filename = filename, format = "pptx",
            width = width, height = height, append = append)
}
##' export figure to docx
##' @name todocx
##' @param figure plot figure function
##' @param filename output filename
##' @param width width of the output figure
##' @param height height of the output figure
##' @param append append or not
##' @author Kai Guo
##' @export
todocx <- function(figure =NULL, filename = NULL, width = 6, height = 6, append = FALSE){
    toffice(figure = figure, filename = filename, format = "docx",
            width = width, height = height,append = append)
}

