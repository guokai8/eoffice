# eoffice
Export graph and tables to MicroSoft office
Note: a wrap of officer package
## Description
_eoffice_ provide wrap functions to export graphics and data.frames in R to MicroSoft office (docx, pptx format)

## Installation
```
library(devtools)
install_github("guokai8/eoffice")
``` 

## Software Usage

```
library(eoffice)
## make a figure with you custom function
plot(mtcars$mpg, mtcars$disp)
topptx(filename = "mtcars.pptx")
## You can also use ggplot 
ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point()
topptx(filename = "mtcars.pptx")
## or todocx(filename = "mtcars.docx")
p <- ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point()
topptx(p, filename = "mtcars.pptx")
## write out table to office
toTable(head(mtcars), filename = "mtcars.docx")
## append was supported if you want add figures or tables.
```
## Note
The _eoffice_ just a package for funs. _eoffice_ depends on _officer_ and _rvg_ package which include all fantastic functions. Here, _eoffice_ provides simplified functions which could be save some time to learn the complete functions from above packages. Also there is a wonderful package already there called _export_. Comparing with that package, _eoffice_ include own features which I think really helpful to me.   

## Contact information

For any questions please contact guokai8@gmail.com
