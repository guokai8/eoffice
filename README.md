# eoffice  
[![](https://cranlogs.r-pkg.org/badges/eoffice)](https://cran.r-project.org/package=eoffice)
<a href="https://cran.r-project.org/web/checks/check_results_eoffice.html"><img border="0" src="http://www.r-pkg.org/badges/version/eoffice" alt="CRAN version"></a>
<a href="https://travis-ci.org/guokai8/eoffice"><img src="https://travis-ci.org/guokai8/eoffice.svg" alt="Build status"></a> 
[![Project Status:](http://www.repostatus.org/badges/latest/active.svg)](http://www.repostatus.org/#active)
[![](https://img.shields.io/badge/devel%20version-0.1.9-green.svg)](https://github.com/guokai8/eoffice)

Export and import graphics and tables to MicroSoft office

## Description
_eoffice_ provide wrap functions to export and import graphics and data.frames in R to MicroSoft office with fully editable figures and tables(docx, pptx format).
And _eoffice_ also provide write out figures with lots of different formats, such as pdf, eps, emf, tiff, svg, wmf, png and jpeg. _eoffice_ also support write out or display ggplot2 type figure with plotly. Since people may work on the platform without GUI support, _eoffice_ also provide function to easily write out figures to all above formats, pptx and docx._eoffice_ is avaiable on CRAN now. _eoffice_ provides function to extract colors from figures with different formats or pdf files. For linux platform please install imageMagick to use the _infigure_ function.       

on Debian/Ubuntu this is called libmagick++-dev:    
sudo apt-get install libmagick++-dev         
On Fedora or CentOS/RHEL we need ImageMagick-c++-devel:       
sudo yum install ImageMagick-c++-devel       
If you can't install the ImageMagick you can go with _eoffice_ 0.1.6 version 
## Installation
```
library(devtools)
install_github("guokai8/eoffice")
## eoffice is available on CRAN
install.packages("eoffice")
``` 

## Software Usage

```
library(eoffice)
library(ggplot2)
## make a figure with you custom function
plot(mtcars$mpg, mtcars$disp)
topptx(filename = "mtcars.pptx")
## You can also use ggplot 
ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point()
topptx(filename = "mtcars.pptx")
## or todocx(filename = "mtcars.docx")
p <- ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point()
topptx(p, filename = "mtcars.pptx", width = 6, height = 4)
## use above method if you want use topptx with ggplot function in a loop
## write out table to office
totable(head(mtcars), filename = "mtcars.docx")
totable(head(mtcars), filename = "mtcars.pptx")
## append was supported if you want add figures or tables.
## Now support direct output of common objects produced by R statistical functions
tt <- t.test(wt ~ am, mtcars)
totable(tt, filename = "mtcars_test.pptx")
totable(t.test(wt ~ am, mtcars), filename = "mtcars_test.pptx")
## inpptx and indocx provide function read the tables in pptx or docx
tabs <- inpptx(filename = "mtcars.pptx", header = TRUE)
## output different figure formats
tofigure(ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point(),filename = "mtcars.pdf")
tofigure(ggplot(mtcars, aes(mpg, disp, color = factor(cyl))) + geom_point(),filename = "mtcars.eps")
## display figure with plotly
tohtml(p, save = FALSE)
## if you use basic plot function or other plot function you need first use convertplot to convert 
## it to ggplot object when you are working on the platform without GUI
p <- convertplot(plot(1:10))
tofigure(p, filename = "test.pdf")
topptx(p, filename = "test.pptx")
## if you use ggplot like function you don't need to transform the format
```
## Note
The _eoffice_ just a package for funs. _eoffice_ depends on _officer_ and _rvg_ package which include lots of fantastic functions. Here, _eoffice_ provides simplified functions which could be save some time to learn the complete functions from above packages. And there are some other packages provide these functions. Comparing with these packages, _eoffice_ include own features which I think really helpful to me.  Besides, _eoffice_ also provide functions to read tables from pptx and docx. Read graphics and extract colors from the figures are available now.

## Contact information

For any questions please contact guokai8@gmail.com

