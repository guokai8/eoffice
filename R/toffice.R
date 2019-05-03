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
##' @param figure figure function
##' @param format file type
##' @param filename output filename
##' @author Kai Guo
##' @export
toffice <- function(figure = NULL, plot = NULL, format = "pptx", filename= "temp.pptx"){
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
        doc <- read_pptx()
        doc <- add_slide(doc,"Title and Content", "Office Theme")
        doc <- ph_with_vg(doc, print(p))
        print(doc,target=filename)
    }
    if(format == "doc"){
        doc <- read_docx()
        doc <- body_add_vg(doc, print(p))
        print(doc,target=filename)
    }
}

##' export figure to pptx
##' @name topptx
##' @param figure figure function
##' @param filename output filename
##' @author Kai Guo
##' @export
topptx <- function(figure = NULL, filename = NULL){
    toffice(figure = figure, filename = filename, format = "pptx" )
}
##' export figure to docx
##' @name todocx
##' @param figure figure function
##' @param filename output filename
##' @author Kai Guo
##' @export
todocx <- function(figure =NULL, filename = NULL){
    toffice(figure = figure, filename = filename, format = "docx" )
}

