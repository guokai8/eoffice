# eoffice
Export and import graph and tables to MicroSoft office
## Description
_eoffice_ provide wrap functions to export and import graphics and data.frames in R to MicroSoft office (docx, pptx format)

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
## inpptx and indocx provide function read the tables in pptx or docx
tabs <- inpptx(filename = "file_with_table.pptx",header = TRUE)
```
## Note
The _eoffice_ just a package for funs. _eoffice_ depends on _officer_ and _rvg_ package which include all fantastic functions. Here, _eoffice_ provides simplified functions which could be save some time to learn the complete functions from above packages. Also there are some packages support these functions. Comparing with these packages, _eoffice_ include own features which I think really helpful to me.  Besides, _eoffice_ also provide functions to read tables from pptx and docx.

## Contact information

For any questions please contact guokai8@gmail.com
