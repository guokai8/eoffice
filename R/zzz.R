.onLoad <- function(libname, pkgname) {
    old <- options()         
    options(stringsAsFactors = FALSE)
    on.exit(options(old)) 
}
if(getRversion() >= "2.15.1") {
    utils::globalVariables(c(".","Abundance", "Group","LDA","LDAscore", "OTU",
                             "Phylum","Sample","direction",
                             "log2FoldChange","ma","p.adj",
                             "p.value", "tax","type", "val"))
}
