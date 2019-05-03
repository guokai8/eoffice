##' write table out to pptx or docx
##' @importFrom officer add_slide
##' @importFrom flextable flextable
##' @importFrom flextable ph_with_flextable
##' @importFrom flextable theme_booktabs
##' @importFrom flextable body_add_flextable
##' @importFrom flextable autofit
##' @importFrom flextable bold
##' @importFrom flextable align
##' @importFrom flextable hline
##' @importFrom flextable font
##' @importFrom officer fp_border
##' @importFrom officer read_pptx
##' @importFrom officer read_docx
##' @importFrom flextable empty_blanks
##' @importFrom magrittr %>%
##' @param data datasets
##' @param filename output filename
##' @author Kai Guo
##' @export
toTable <- function(data, filename, format = NULL){
    if(is.null(format)){
        format = .getext(filename)
    }
    format = tolower(format)
    if (format == "ppt" | format == "pptx") {
        format = "ppt"
    }
    if (format == "doc" | format == "docx") {
        format = "doc"
    }
    ft <- flextable(data = data) %>%
        theme_booktabs() %>% bold(part = "header") %>%
        font(fontname = "Times", part = "all") %>%
        align(align = "center", part ="all") %>%
        hline(border = fp_border(width = 2, color = "#007FA6"), part = "header" ) %>%
        empty_blanks() %>%
        autofit()
    if(format == "ppt"){
        doc <- read_pptx()
        doc <- add_slide(doc, "Title and Content", "Office Theme")
        doc <- ph_with_flextable(doc, ft)
        print(doc,target=filename)
    }
    if(format == "doc"){
        doc <- read_docx()
        doc <- body_add_flextable(doc, ft)
        print(doc,target=filename)
    }
}
