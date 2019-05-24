##' export as plotly html (only support ggplot2 object)
##' @name tohtml
##' @importFrom plotly ggplotly
##' @importFrom htmlwidgets saveWidget
##' @importFrom plotly as_widget
##' @importFrom ggplot2 last_plot
##' @param figure output figure function, set NULL output the current figure
##' @param filename output format (could be ingored)
##' @param save save figure or not (default: TRUE)
##' @examples
##' \dontrun{
##' ##set tempdir used for the examples
##' currentwd <- getwd()
##' setwd(tempdir())
##' p <- ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point()
##' tohtml(p,filename = "mtcars.html")
##' ## or use ggplot directly
##' tohtml(ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) +
##' geom_point(), filename = "mtcars.html")
##' setwd(currentwd)
##' }
##' @export
##' @author Kai Guo
tohtml <- function(figure, filename= "temp.html", save = TRUE){
    if(missing(figure)){
        p <- last_plot()
    }else{
        p <- print(figure)
    }
    p <- ggplotly(p)
    if(isTRUE(save)){
        saveWidget(as_widget(p),filename)
    }else{
        print(p)
    }
}
