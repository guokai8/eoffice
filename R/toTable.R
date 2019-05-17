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
##' @importFrom broom tidy
##' @importFrom magrittr %>%
##' @param data datasets
##' @param filename output filename
##' @param format pptx or docx
##' @param append append into file
##' @examples
##' \dontrun{
##' tt <- t.test(wt ~ am, mtcars)
##' totable(tt, filename = "mtcars_test.pptx")
##' totable(t.test(wt ~ am, mtcars), filename = "mtcars_test.pptx")
##' totable(head(mtcars), filename = "mtcars_head.docx")
##' }
##' @author Kai Guo
##' @export
totable <- function(data, filename, format = NULL, append = FALSE){
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
    typecl <- c("matrix","data.frame","tbl_df","tbl")
    if(length(intersect(class(data),typecl))==0){
        data = tidy(data)
    }else{
        if(class(data)=="matrix"){
            data = as.data.frame(data)
        }
    }
    ft <- flextable(data = data) %>%
        theme_booktabs() %>% bold(part = "header") %>%
        font(fontname = "Times", part = "all") %>%
        align(align = "center", part ="all") %>%
        hline(border = fp_border(width = 2, color = "#007FA6"), part = "header" ) %>%
        empty_blanks() %>%
        autofit()
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
        doc <- add_slide(doc, "Title and Content", "Office Theme")
        doc <- ph_with_flextable(doc, ft)
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
        doc <- read_docx()
        doc <- body_add_flextable(doc, ft)
        print(doc,target=filename)
    }
}
