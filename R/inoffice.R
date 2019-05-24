##' read tables from ppt and word
##' @importFrom officer read_docx
##' @importFrom officer docx_summary
##' @importFrom officer pptx_summary
##' @importFrom magrittr %>%
##' @importFrom dplyr filter_
##' @importFrom dplyr select_
##' @param filename input filename
##' @param format pptx or docx
##' @param header use first row as column name
##' @examples
##' ## use tempdir for the example
##' totable(t.test(wt ~ am, mtcars), filename = file.path(tempdir(), "mtcars.pptx"))
##' tabs <- inoffice(filename = file.path(tempdir(), "mtcars.pptx"), header = TRUE)
##' tabs
##' @export
##' @author Kai Guo
inoffice <- function(filename, format = NULL, header = TRUE){
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
    if(format == "ppt"){
        doc <- read_pptx(filename)
        content <- pptx_summary(doc)
        tent <- tapply(content$id,
               content$content_type,
               function(x) length(unique(x)))
        tent["slides"] <- length(unique(content$slide_id))
    }
    if(format == "doc"){
        doc <- read_docx(filename)
        content <- docx_summary(doc)
        tent <- tapply(content$doc_index,
                       content$content_type,
                       function(x) length(unique(x)))
    }
    table_cells <- filter_(content, ~content_type %in% "table cell")
    if(format == "ppt"){
        len <- length(unique(table_cells$slide_id))
        res<-vector(len,mode="list")
        for(i in 1:len){
            sub_table <- filter_(table_cells, ~slide_id == i)
            for(j in unique(sub_table$id)){
                data <- filter_(table_cells, ~id == j) %>% select_(~row_id, ~cell_id, ~text)
                data <- tapply(data$text,
                        list(row_id = data$row_id,
                        cell_id = data$cell_id
                   ), FUN = I )
                res[[i]][[j]] <- data
            }
        }
        names(res) <- paste("slide",1:len,sep="")
        res <- unlist(res, recursive = FALSE)
        tent["table cell"] <- length(res)
        }
    if(format == "doc"){
        len <- length(unique(table_cells$doc_index))
        res<-vector(len,mode="list")
        for(i in unique(table_cells$doc_index)){
            data <- filter_(table_cells, ~doc_index == i)%>%select_(~row_id, ~cell_id, ~text)
            data <- tapply(data$text,
                           list(row_id = data$row_id,
                                cell_id = data$cell_id
                           ), FUN = I )
            res[[i]] <- data
        }
        res <- Filter(Negate(is.null), res)
        names(res) <- paste("Table",1:length(res),sep="")
    }
    res <- lapply(res, function(x) as.data.frame(x))
    if(isTRUE(header)){
        res <- lapply(res, function(x).setcolnames(x))
    }
    cat("Here are the details in the ",filename,":\n")
    print(tent)
    return(res)
    }

##' read table from pptx
##' @name inpptx
##' @param filename input filename
##' @param header use first row as column name
##' @examples
##' totable(t.test(wt ~ am, mtcars), filename = file.path(tempdir(), "mtcars.pptx"))
##' tabs <- inpptx(filename = file.path(tempdir(), "mtcars.pptx"), header = TRUE)
##' tabs
##' @author Kai Guo
##' @export
inpptx <- function(filename, header = FALSE){
    res <- inoffice(filename, format = "pptx", header = header)
    return(res)
}

##' read table from docx
##' @name indocx
##' @param filename input filename
##' @param header use first row as column name
##' @examples
##' totable(t.test(wt ~ am, mtcars), filename = file.path(tempdir(), "mtcars.docx"))
##' tabs <- indocx(filename = file.path(tempdir(), "mtcars.docx"), header = TRUE)
##' tabs
##' @author Kai Guo
##' @export
indocx <- function(filename, header = FALSE){
    res <- inoffice(filename, format = "docx", header = header)
    return(res)
}


