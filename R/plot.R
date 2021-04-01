#' plot beta diversity
#'
#' @importFrom ggplot2 ggplot aes_string
#' @importFrom ggplot2 geom_point
#' @importFrom ggplot2 scale_color_manual
#' @importFrom ggplot2 xlab ylab stat_ellipse
#' @importFrom ggplot2 theme_light
#' @importFrom phyloseq taxa_are_rows
#' @param physeq A \code{phyloseq} object containing merged information of abundance,
#'        taxonomic assignment, sample data including the measured variables and categorical information
#'        of the samples, and / or phylogenetic tree if available.
#' @param group (Required). Character string specifying name of a categorical variable that is preferred for grouping the information.
#'        information.
#' @param shape shape(Optional) Character string specifying shape of a categorical variable
#' @param method A character string specifying ordination method. All methods available to the \code{ordinate} function
#'        of \code{phyloseq} are acceptable here as well.
#' @param distance A string character specifying dissimilarity index to be used in calculating pairwise distances (Default index is "bray".).
#'                       "unifrac","wunifrac","manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower",
#'                       "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao" or "mahalanobis".
#' @param color user defined color for group
#' @param size the point size
#' @param ellipse draw ellipse or not
#' @examples
#' {
#' data("Physeq")
#' phy<-normalize(physeq)
#' plotbeta(phy,group="SampleType")
#' }
#' @return ggplot2 object
#' @author Kai Guo
#' @export
plotbeta<-function(physeq,group,shape=NULL,distance="bray",method="PCoA",color=NULL,size=3,ellipse=FALSE){
    if(!taxa_are_rows(physeq)){
        physeq <- t(physeq)
    }
    beta<-betadiv(physeq,distance = distance,method=method)
    df <- as.data.frame(beta$beta)
    PCs <- beta$PCs
    tab <- as(sample_data(physeq),"data.frame")
    df <- cbind(df[,1:4],tab[rownames(df),])
    df$group<-tab[,group]
    if(is.null(color)){
        color<-distcolor[1:length(unique(df$group))]
    }
    if(!is.null(shape)){
        df$shape<-tab[,shape]
        p <- ggplot(df,aes_string("Axis.1","Axis.2",color="group",shape="shape"))
    }else{
        p <- ggplot(df,aes_string("Axis.1","Axis.2",color="group"))
    }
    p<-p+geom_point(size=size)+scale_color_manual(values=color)
    p <- p+theme_light(base_size=15)+xlab(paste0("Axis1 (",round(PCs[1]*100,2),"%)"))+ylab(paste0("Axis2 (",round(PCs[2]*100,2),"%)"))
    if(isTRUE(ellipse)){
        p <- p + stat_ellipse()
    }
    p
}

#' plot alpha diversity
#' @importFrom rstatix t_test
#' @importFrom rstatix wilcox_test
#' @importFrom ggpubr ggboxplot
#' @importFrom ggpubr ggviolin
#' @importFrom ggpubr ggdotplot
#' @importFrom ggpubr stat_pvalue_manual
#' @importFrom ggpubr facet
#' @importFrom ggplot2 xlab ylab scale_color_manual theme
#' @importFrom dplyr summarise group_by
#' @importFrom tidyr gather spread
#' @param physeq A \code{phyloseq} object containing merged information of abundance,
#'        taxonomic assignment, sample data including the measured variables and categorical information
#'        of the samples, and / or phylogenetic tree if available.
#' @param group group (Required). A character string specifying the name of a categorical variable containing  grouping information.
#' @param method A list of character strings specifying \code{method} to be used to calculate for alpha diversity
#'        in the data. Available methods are: "Observed","Chao1","ACE","Richness", "Fisher", "Simpson", "Shannon", "Evenness","InvSimpson".
#' @param color A vector of character use specifying the color
#' @param geom different geom to display("boxplot","violin","dotplot")
#' @param pvalue pvalue threshold for significant dispersion results
#' @param sig.only display the significant comparsion only(TRUE/ FALSE)
#' @param padj adjust p value threshold for significant dispersion results
#' @param wilcox use wilcoxon test or not
#' @param show.number to show the pvalue instead of significant symbol(TRUE/FALSE)
#' @examples
#' {
#' data("Physeq")
#' plotalpha(physeq,group="SampleType")
#' }
#' @return Returns a ggplot object. This can further be manipulated as preferred by user.
#' @author Kai Guo
#' @export
plotalpha<-function(physeq,group,method=c("Observed","Simpson", "Shannon"),color=NULL,geom="boxplot",
                    pvalue=0.05,padj=NULL,sig.only=TRUE, wilcox=FALSE,show.number=FALSE){
    if (!taxa_are_rows(physeq)) {
        physeq <- t(physeq)
    }
    rich<-richness(physeq,method = method)
    name<-levels(factor(colnames(rich)))
    tab<-as(sample_data(physeq),"data.frame")
    rich$group<-tab[rownames(rich),group]
    if(isTRUE(wilcox)){
        res<-do_wilcox(rich,"group")
    }else{
        res<-do_ttest(rich,"group")
    }
    if(sum(res$p<pvalue)<1){
        message("No significant difference between any of the groups")
        pvalue = 1
    }
    if(isTRUE(sig.only)){
        res <- subset(res,p < pvalue)
        if(!is.null(padj)){
            res <- res[res$p.adj < padj,]
        }
    }
    vals<-rich%>%gather(type,val,-group)%>%group_by(type,group)%>%summarise(ma=max(val))%>%spread(group,ma)
    pos <- apply(res, 1, function(x)max(vals[vals$type==x[1],x[3:4]]))
    mpos <- apply(res, 1, function(x)min(vals[vals$type==x[1],x[3:4]]))
    if(geom=="boxplot"){
        p<-rich%>%gather(type,val,-group)%>%ggboxplot(x="group",y="val",color="group")
    }else if(geom=="violin"){
        p<-rich%>%gather(type,val,-group)%>%ggviolin(x="group",y="val",color="group")
    }else if(geom=="dotplot"){
        p<-rich%>%gather(type,val,-group)%>%ggdotplot(x="group",y="val",color="group")
    }else{
        stop("Please specify one type of boxplot,violin,dotplot")
    }
    if(!isTRUE(show.number)){
        res$p.signif<-sapply(res$p,function(x).getstar(x))
        res$p.adj.signif<-sapply(res$p.adj,function(x).getstar(x))
    }else{
        res$p.signif<-res$p
        res$p.adj.signif<-res$p.adj
    }
    if(is.null(color)){
        color<-distcolor[1:length(unique(rich$group))]
    }
    p<-facet(p,facet.by = "type",scales = "free_y",ncol = length(method))
    if(!is.null(padj)){
        p<-p+stat_pvalue_manual(res,label = "p.adj.signif",y.position = pos+2*mpos/nrow(res))
    }else{
        p<-p+stat_pvalue_manual(res,label = "p.signif",y.position = pos+mpos/nrow(res))
    }
        p<-p+xlab("")+ylab("")+
        theme(legend.position = "none",axis.text.x=element_text(angle=90,vjust=0.5, hjust=1))+
        scale_color_manual(values=color)
    p
}


#' plot bar for relative abundance for bacteria
#' @importFrom phyloseq psmelt
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_bar
#' @importFrom ggplot2 scale_fill_manual
#' @importFrom ggplot2 theme
#' @importFrom ggplot2 element_text
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 element_blank scale_y_continuous
#' @importFrom dplyr group_by_at
#' @importFrom dplyr vars pull
#' @importFrom dplyr one_of
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom rlang `!!`
#' @importFrom utils head
#' @param physeq A \code{phyloseq} object containing merged information of abundance,
#'        taxonomic assignment, sample data including the measured variables and categorical information
#'        of the samples, and / or phylogenetic tree if available.
#' @param level the level to plot
#' @param color A vector of character use specifying the color
#' @param group group (Optional). A character string specifying the name of a categorical variable containing  grouping information.
#' @param top the number of most abundance bacteria to display
#' @param fontsize.x the size of x axis label
#' @param fontsize.y the size of y axis label
#' @examples
#' \donttest{
#' data("Physeq")
#' phy<-normalize(physeq)
#' plotbar(phy,level="Phylum")
#' }
#' @return Returns a ggplot object. This can further be manipulated as preferred by user.
#' @author Kai Guo
#' @export
plotbar<-function(physeq,level="Phylum",color=NULL,group=NULL,top=5,fontsize.x = 5, fontsize.y = 12){
    pm <- psmelt(physeq)
    if(is.null(color)){
        len<-length(unique(pm[,level]))
        color<-distcolor[1:len]
    }
    if(is.null(group)){
        group_var<-c("Sample",level)
    }else{
        group_var<-c(group,level)
    }
    d<-pm%>%group_by_at(vars(one_of(group_var)))%>%summarise(su=sum(Abundance))
    d <- as.data.frame(d)
    d[,level][is.na(d[,level])]<-"NA"
    dx <- pm%>%group_by_at(vars(one_of(level)))%>%summarise(su=sum(Abundance))
    dx <- dx[order(dx$su,decreasing = T),]
    sel <- dx%>%head(top)%>%select(!!level)%>%pull(1)
    d <- d[d[,level]%in%sel,]
    if(is.null(group)){
        p<-ggplot(d,aes_string("Sample","su",fill=level))
    }else{
        p<-ggplot(d,aes_string(group,"su",fill=level))
    }
    p<-p+geom_bar(stat = "identity",position = "fill")+scale_fill_manual(values=color)+
    theme_light()+
    scale_y_continuous(expand = c(0, 0.001)) +
    theme(axis.text.x=element_text(angle=90,size=fontsize.x, vjust=0.5, hjust=1),
              axis.text.y=element_text(size=fontsize.y),
              panel.background = element_blank(),axis.ticks.x = element_blank())+
        xlab("")+ylab("")
    p
}

#' @title plot differential results
#' @importFrom ggplot2 ggplot theme geom_point element_text xlab
#' @importFrom ggplot2 aes_string scale_color_manual theme_light coord_flip
#' @param res differential test results from diff_test
#' @param level the level to plot
#' @param color A vector of character use specifying the color
#' @param pvalue pvalue threshold for significant  results
#' @param padj adjust p value threshold for significant  results
#' @param log2FC log2 Fold Change threshold
#' @param size size for the point
#' @param fontsize.x the size of x axis label
#' @param fontsize.y the size of y axis label
#' @param horiz horizontal or not (TRUE/FALSE)
#' @examples
#'  \donttest{
#' data("Physeq")
#' res <- difftest(physeq,group="group")
#' plotdiff(res,level="Genus",padj=0.001)
#' }
#' @return ggplot object
#' @author Kai Guo
#' @export
plotdiff<-function(res,level="Genus",color=NULL,pvalue=0.05,padj=NULL,log2FC=0,size=3,fontsize.x=5,fontsize.y=10,horiz=TRUE){
    if(!is.null(padj)){
        pval<-padj
        sigtab <- subset(res,padj<pval&abs(log2FoldChange)>log2FC)
    }else{
        pval<-pvalue
        sigtab <- subset(res,pvalue<pval&abs(log2FoldChange)>log2FC)
    }
    x <- tapply(sigtab$log2FoldChange, sigtab$Phylum, function(x) max(x))
    x <- sort(x, TRUE)
    sigtab$Phylum <- factor(as.character(sigtab$Phylum), levels=names(x))
    if(is.null(color)){
        len<-length(unique(sigtab$Phylum))
        color<-distcolor[1:len]
    }
    # Genus order
    sigtab$name<-paste0(sigtab[,level],"(",rownames(sigtab),")")
    x <- tapply(sigtab$log2FoldChange, sigtab$name, function(x) max(x))
    x <- sort(x, TRUE)
    sigtab$name <- factor(as.character(sigtab$name), levels=names(x))
    p <- ggplot(sigtab, aes_string(x="name", y="log2FoldChange", color="Phylum"))+
        geom_point(size=3) +theme_light()+xlab(level)+
        theme(axis.text.x = element_text(angle = -90, hjust = 0, vjust=0.5,size=fontsize.x),
              axis.text.y = element_text(size=fontsize.y))+
        scale_color_manual(values=color)
    if(isTRUE(horiz)){
        p<-p+coord_flip()+theme(axis.text.x=element_text(angle=0,size=fontsize.x))
    }
    p
}

#' plot LEfSe results from ldamarker function
#' @importFrom ggplot2 ggplot geom_bar coord_flip theme_light element_text
#' @importFrom ggplot2 scale_fill_manual xlab
#' @importFrom ggplot2 aes
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom stats reorder
#' @param x LEfse results from ldamarker
#' @param group a vector include two character to show the group comparsion
#' @param lda LDA threshold for significant biomarker
#' @param pvalue pvalue threshold for significant results
#' @param padj adjust p value threshold for significant results
#' @param color A vector of character use specifying the color
#' @param fontsize.x the size of x axis label
#' @param fontsize.y the size of y axis label
#' @examples
#' \donttest{
#' data("Physeq")
#' res <- ldamarker(physeq,group="group")
#' plotLDA(res,group=c("A","B"),lda=5,pvalue=0.05)
#' }
#' @return ggplot2 object
#' @author Kai Guo
#' @export
plotLDA<-function(x,group,lda=2,pvalue=0.05,padj=NULL,color=NULL,fontsize.x=4,fontsize.y=5){
    x <- subset(x,LDAscore>lda)
    if(!is.null(padj)){
        x <- subset(x,p.adj<padj)
    }else{
        x <- subset(x,p.value<pvalue)
    }
    x <- subset(x,direction%in%group)
    x<-x %>%mutate(LDA=ifelse(direction==group[1],LDAscore,-LDAscore))
    p<-ggplot(x,aes(x=reorder(tax,LDA),y=LDA,fill=direction))+
        geom_bar(stat="identity",color="white")+coord_flip()+
        theme_light()+theme(axis.text.x = element_text(size=fontsize.x),
                            axis.text.y = element_text(size=fontsize.y))
    if(is.null(color)){
        color <- distcolor[1:2]
    }
    p<-p+scale_fill_manual(values=color)+xlab("")
    p
}

#'
#' plot the biomarker from the biomarker function with randomForest
#' @importFrom ggpubr ggdotchart
#' @importFrom ggplot2 xlab ylab
#' @param x biomarker results from randomForest
#' @param level the bacteria level to display
#' @param top the number of important biomarker to draw
#' @param rotate TRUE/FALSE
#' @param dot.size size for the dot
#' @param label.size label size
#' @param label.color label color
#' @return ggplot2 object
#' @examples
#' \donttest{
#' data("Physeq")
#' res <- biomarker(physeq,group="group")
#' plotmarker(res,level="Genus")
#' }
#' @export
#' @author Kai Guo
plotmarker<-function(x,level="Genus",top=30,rotate=FALSE,dot.size=8,label.color="black",label.size=6){
    x <- x[1:top,]
    x <- x[order(x$Value),]
    x$label<-paste0(x[,level],"(",x$OTU,")")
    p<-ggdotchart(x,x="label",y="Value",add="segments",color=I("#00AFBB"),rotate=rotate,dot.size=dot.size,sorting="descending",
                  add.params = list(color = "#00AFBB", size = 1.5),
               label=round(x$Value,2),font.label = list(color = label.color, size = label.size,vjust=0.2))
    if(isTRUE(rotate)){
        p<-p+xlab(level)+ylab("Mean Decrease Accuracy")
    }else{
        p<-p+xlab(level)+ylab("Mean Decrease Accuracy")
    }
    p
}

#' plot the quality for the fastq file
#' @importFrom dada2 plotQualityProfile
#' @param file 	(Required). character. File path(s) to fastq or fastq.gz file(s).
#' @param n	(Optional). Default 500,000. The number of records to sample from the fastq file.
#' @param aggregate	(Optional). Default FALSE. If TRUE, compute an aggregate quality profile for all fastq files provided.
#' @examples
#' \donttest{
#' plotquality(system.file("extdata", "sam1F.fastq.gz", package="dada2"))
#' }
#' @export
#' @return figure
plotquality<-function(file,n = 5e+05, aggregate = FALSE){
    plotQualityProfile(file,n=n,aggregate = aggregate)
}
