simpleCap <- function(x) {
    s <- strsplit(x, " ")[[1]]
    paste(toupper(substring(s, 1,1)), substring(s, 2),
          sep="", collapse=" ")
}

#' light colors for making figures
#' @author Kai Guo
#' @export
lightcolor<-c('#E5D2DD', '#53A85F', '#F1BB72', '#F3B1A0', '#D6E7A3', '#57C3F3', '#476D87',
                '#E95C59', '#E59CC4', '#AB3282', '#23452F', '#BD956A', '#8C549C', '#585658',
                '#9FA3A8', '#E0D4CA', '#5F3D69', '#C5DEBA', '#58A4C3', '#E4C755', '#F7F398',
                '#AA9A59', '#E63863', '#E39A35', '#C1E6F3', '#6778AE', '#91D0BE', '#B53E2B',
                '#712820', '#DCC1DD', '#CCE0F5',  '#CCC9E6', '#625D9E', '#68A180', '#3A6963',
                '#968175','#e6194b', '#3cb44b', '#ffe119', '#4363d8','#f58231', '#911eb4',
                '#46f0f0', '#f032e6', '#bcf60c',
                '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8',
                '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075',
                '#808080'
)
#' distinguish colors for making figures
#' @author Kai Guo
#' @export
distcolor<-c("#A6761D","#D95F02","#66A61E","#1B9E77","#E7298A","#7570B3","#E6AB02",
             "#A6CEE3", "#1F78B4", "#B2DF8A", "#33A02C", "#FB9A99", "#E31A1C",
             "#A6761D","#D95F02","#66A61E","#1B9E77","#E7298A","#7570B3","#E6AB02",'#e6194b', '#3cb44b', '#4363d8',
         '#f58231', '#911eb4', '#46f0f0', '#f032e6', '#bcf60c',
         '#fabebe', '#008080', '#e6beff', '#9a6324', '#fffac8',
         '#800000', '#aaffc3', '#808000', '#ffd8b1', '#000075',
         '#808080', '#ffffff', '#000000')
#' do anova test and return results as data.frame
#' @importFrom rstatix anova_test
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @param x data.frame with sample id as the column name, genes or otu as rownames
#' @param group group factor used for comparison
#' @param ... parameters to anova_test
#' @examples
#' {
#' data("ToothGrowth")
#' do_aov(ToothGrowth,group="supp")
#' }
#' @export
#' @author Kai Guo
do_aov<-function(x,group,...){
    d<-x[,setdiff(colnames(x),group)]
    d$group<-x[,group]
    d<-d%>%gather(type,val,-group)
    res<-d%>%group_by(type)%>%anova_test(val~group,...)
    return(res)
}

#' do t.test
#' @importFrom rstatix t_test
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @param x data.frame with sample id as the column name, genes or otu as rownames
#' @param group group factor used for comparison
#' @param ref reference group
#' @param ... parameters to t_test
#' @examples
#' {
#' data("mtcars")
#' do_ttest(mtcars,group="vs")
#' do_ttest(mtcars,group="cyl",ref="4")
#' }
#' @export
#' @author Kai Guo
do_ttest<-function(x,group,ref=NULL,...){
    d<-x[,setdiff(colnames(x),group)]
    d$group<-x[,group]
    d<-d%>%gather(type,val,-group)
    res<-d%>%group_by(type)%>%t_test(val~group,ref.group = ref,...)
    res$p.adj<-p.adjust(res$p,method="BH")
    return(res)
}

#' do wilcox test
#' @importFrom rstatix wilcox_test
#' @importFrom tidyr gather
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by
#' @param x data.frame with sample id as the column name, genes or otu as rownames
#' @param group group factor used for comparison
#' @param ref reference group
#' @param ... parameters to wilcox_test
#' @examples
#' {
#' data("mtcars")
#' do_wilcox(mtcars,group="vs")
#' do_wilcox(mtcars,group="cyl",ref="4")
#' }
#' @export
#' @author Kai Guo
do_wilcox<-function(x,group,ref=NULL,...){
    d<-x[,setdiff(colnames(x),group)]
    d$group<-x[,group]
    d<-d%>%gather(type,val,-group)
    res<-d%>%group_by(type)%>%wilcox_test(val~group,ref.group = ref,...)
    res$p.adj<-p.adjust(res$p,method="BH")
    return(res)
}
#'
gm_mean = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

#' replace p value with star
#' @param x a (non-empty) numeric data values
.getstar<-function(x){
    if(x>=0.05){
        return("ns")
    }else if(x>=0.01 & x<0.05){
        return("*")
    }else if(x<0.01){
        return("**")
    }else{
    return("***")
    }
}
#' check file format
#' @param file filename
.checkfile <- function(file){
    ex <- strsplit(basename(file), split="\\.")[[1]]
    return(ex[-1])
}

#' LEfse function
#' @param df a dataframe with groups and bacteria abundance
.lda.fun<-function(df){
     # modified from https://github.com/xia-lab/MicrobiomeAnalystR/blob/
    # 0a8d81afeb3b637122c97c2d17146a44fa978c4f/R/general_anal.R
    ldares <- MASS::lda(group~seqs,df,tol = 1.0e-10);
    ldamean <- as.data.frame(t(ldares$means));
    class_no <- length(unique(df$group));
    ldamean$max <- apply(ldamean[,1:class_no],1,max);
    ldamean$min <- apply(ldamean[,1:class_no],1,min);
    ldamean$LDAscore <- signif(log10(1+abs(ldamean$max-ldamean$min)/2),digits=3);
    resTable <- ldamean;
    resTable$direction <- colnames(resTable)[which(resTable[,1:class_no]==resTable$max)]
    return(resTable)
}

#' contruction of plylogenetic tree (extreme slow)
#' @importFrom phangorn phyDat dist.ml NJ pml optim.pml pml.control
#' @importFrom stats update
#' @param seqs DNA sequences
#' @author Kai Guo
#' @return tree object
#' @export
buildTree<-function(seqs){
    alignment <- DECIPHER::AlignSeqs(Biostrings::DNAStringSet(seqs), anchor=NA,verbose=T)
    phangAlign <- phyDat(as(alignment, "matrix"), type="DNA")
    dm <- dist.ml(phangAlign)
    treeNJ <- NJ(dm) # Note, tip order != sequence order
    fit = pml(treeNJ, data=phangAlign)
    fitGTR <- update(fit, k=4, inv=0.2)
    fitGTR <- optim.pml(fitGTR, model="GTR", optInv=TRUE, optGamma=TRUE,
                    rearrangement = "stochastic", control = pml.control(trace = 0))
    return(fitGTR)
}
#' extract otu table
#' @param physeq (Required). An integer matrix, otu_table-class, or phyloseq-class.
#' @param ... parameters for the otu_table function in phyloseq package
#' @export
otu_table<-function(physeq,...){
    phyloseq::otu_table(physeq,...)
}
#' extract taxonomy table
#' @param physeq An object among the set of classes defined by the phyloseq package that contain taxonomyTable.
#' @param ... parameters for the tax_table function in phyloseq package
#' @export
tax_table<-function(physeq,...){
    phyloseq::tax_table(physeq,...)
}
#' extract sample information
#' @param physeq (Required). A data.frame-class, or a phyloseq-class object.
#' @param ... parameters for the sample_data function in phyloseq package
#' @export
sample_data<-function(physeq,...){
    phyloseq::sample_data(physeq,...)
}

#' Retrieve phylogenetic tree (phylo-class) from object.
#' @param physeq (Required). An instance of phyloseq-class that contains a phylogenetic tree.
#'        If physeq is a phylogenetic tree (a component data class), then it is returned as-is.
#' @param ... parameters for the phy_tree function in phyloseq package
#' @export
phy_tree<-function(physeq,...){
    phyloseq::phy_tree(physeq,...)
}

#' Subset the phyloseq based on sample
#' @param physeq A sample_data-class, or a phyloseq-class object with a sample_data.
#' If the sample_data slot is missing in physeq, then physeq will be returned as-is,
#' and a warning will be printed to screen.
#' @param ... parameters for the subset_samples function in phyloseq package
#' @export
subset_samples<-function(physeq,...){
    phyloseq::subset_samples(physeq,...)
}

#' Subset species by taxonomic expression
#' @param physeq A sample_data-class, or a phyloseq-class object with a sample_data.
#' If the sample_data slot is missing in physeq, then physeq will be returned as-is,
#' and a warning will be printed to screen.
#' @param ... parameters for the subset_taxa function in phyloseq package
#' @export
subset_taxa<-function(physeq,...){
    phyloseq::subset_taxa(physeq,...)
}

#' Melt phyloseq data object into large data.frame
#' @param physeq A sample_data-class, or a phyloseq-class object with a sample_data.
#' If the sample_data slot is missing in physeq, then physeq will be returned as-is,
#' and a warning will be printed to screen.
#' @param ... parameters for the subset_samples function in phyloseq package
#' @export
psmelt<-function(physeq,...){
    phyloseq::psmelt(physeq,...)
}



