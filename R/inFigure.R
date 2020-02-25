##' import figures and extract the colors used in the figures
##' @importFrom magick image_read_svg
##' @importFrom magick image_read_pdf
##' @importFrom magick image_read
##' @importFrom magick image_raster
##' @importFrom magick image_write
##' @importFrom magick image_ggplot
##' @importFrom ggplot2 geom_bar
##' @importFrom ggplot2 geom_text
##' @importFrom ggplot2 ggplot
##' @importFrom ggplot2 aes
##' @importFrom ggplot2 aes_string
##' @importFrom ggplot2 scale_fill_manual
##' @importFrom grDevices dev.size
##' @importFrom graphics plot
##' @name infigure
##' @param filename input filename
##' @param format format of input file
##' @param exclude_col vector of colors to be excluded from the analysis
##' @param topn display the most frequent colors
##' @param showfig display the figure or not (default: FALSE)
##' @param showcol display extracted colors or not (default: FALSE)
##' @param savegg save the figure as ggplot2 object or not (default: FALSE)
##' @param density resolution to render pdf
##' @param pages integer vector with page numbers (pdf file). Defaults to all pages.
##' @examples
##' if(interactive()){
##' require(ggplot2)
##' p <- ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point()
##' tofigure(p,filename = file.path(tempdir(), "mtcars.pdf"))
##' pp <- infigure(filename = file.path(tempdir(), "mtcars.pdf"), exclude_col="white")
##' pp
##' }
##' @export
##' @author Kai Guo
infigure <- function(filename, format = NULL, exclude_col = NULL, topn = 10,
                    showfig = FALSE, showcol =FALSE, savegg = FALSE,
                    density = 300, pages = NULL){
    if(is.null(format)){
        format = .getext(filename)
    }
    format = tolower(format)
    if(format == "pdf"){
        p = image_read_pdf(filename, density = density, pages = pages)
    }else if(format == "svg"){
        p = image_read_svg(filename)
    }else{
        p = image_read(filename)
    }
    rasterp <- image_raster(p)
    coln <- table(rasterp$col)
    names(coln) <- sub('ff$', '', names(coln))
    if(!is.null(exclude_col)){
        start <- substr(exclude_col,1,1)
        exclude_col <- tolower(c(col2hex(exclude_col[start != "#"]),
                            exclude_col[start == "#"]))
        coln <- coln[setdiff(names(coln), exclude_col)]
    }
    coln <- sort(coln,decreasing = TRUE)
    coln <- names(coln[1:topn])
    if(isTRUE(showfig)){
        plot(p)
    }
    if(isTRUE(showcol)){
        dd <- data.frame(rep(1,topn),coln)
        colnames(dd) <- c("name","col")
        pp<-ggplot(dd,aes_string("col","name",fill="col"))+geom_bar(stat="identity")+
            geom_text(aes(label=col),vjust=10)+scale_fill_manual(values=coln)+
            .theme_blank()
        print(pp)
    }
    if(isTRUE(savegg)){
        return(list(ggplot = image_ggplot(p), colors = coln))
    }else{
        return(coln)
    }
}


