#' Download the reference database
#' @importFrom utils download.file
#' @param ref_db the reference database
#' @param path path for the  database
#' @return the path of the database
#' @author Kai Guo
#' @examples
#' \donttest{
#' preRef(ref_db="silva",path=tempdir())
#' }
#' @export
preRef<-function(ref_db,path="."){
    if (ref_db == "rdp"){
        ifelse(!file.exists(paste0(path,"/rdp_train_set_16.fa.gz")),
        download.file(url = "https://zenodo.org/record/801828/files/rdp_train_set_16.fa.gz?download=1",
                     destfile = file.path(paste0(path, "/rdp_train_set_16.fa.gz")),
                     method = "auto"),
       FALSE);
       ifelse(!file.exists(paste0(path,"/rdp_species_assignment_16.fa.gz")),
       download.file(url = "https://zenodo.org/record/801828/files/rdp_species_assignment_16.fa.gz?download=1",
                     destfile = file.path(paste0(path, "/rdp_species_assignment_16.fa.gz")),
                     method = "auto"),
       FALSE);
        message("Database: ")
        message(paste0(path,"/rdp_train_set_16.fa.gz"))
        message(paste0(path, "/rdp_species_assignment_16.fa.gz"))
    } else if (ref_db == "silva"){
            ifelse(!file.exists(paste0(path,"/silva_nr99_v138_train_set.fa.gz")),
            download.file(url = "https://zenodo.org/record/4587955/files/silva_nr99_v138.1_train_set.fa.gz?download=1",
                         destfile = file.path(paste0(path, "/silva_nr99_v138_train_set.fa.gz")),
                         method = "auto"),
           FALSE);
           ifelse(!file.exists(paste0(path,"/silva_species_assignment_v138.fa.gz?")),
           download.file(url = 'https://zenodo.org/record/4587955/files/silva_species_assignment_v138.1.fa.gz?download=1',
                         destfile = file.path(paste0(path,"/silva_species_assignment_v138.fa.gz")),
                         method = 'auto'),
           FALSE);
          message("Database: ")
          message(paste0(path,"/silva_nr99_v138_train_set.fa.gz"))
          message(paste0(path, "/silva_species_assignment_v138.fa.gz"))
    } else {
           ifelse(!dir.exists(paste0(path,"/gg_13_8_train_set_97.fa.gz")),
           download.file(url = "https://zenodo.org/record/158955/files/gg_13_8_train_set_97.fa.gz?download=1",
                         destfile = file.path(paste0(path, "/gg_13_8_train_set_97.fa.gz")),
                         method = "auto"),
           FALSE);
    message("Database:  ")
    message(paste0(path,"/gg_13_8_train_set_97.fa.gz"))
}
}

#' filter the phyloseq
#' @importFrom phyloseq subset_taxa prune_taxa otu_table taxa_are_rows tax_table
#' @importFrom phyloseq taxa_sums get_taxa_unique
#' @importFrom phyloseq 'tax_table<-'
#' @importFrom plyr ddply
#' @param physeq A \code{phyloseq} object containing merged information of abundance,
#'        taxonomic assignment, sample data including the measured variables and categorical information
#'        of the samples, and / or phylogenetic tree if available.
#' @param min Numeric, the threshold for mininal Phylum shown in samples
#' @param perc Numeric, input the percentage of samples for which to filter low counts.
#' @examples
#' \donttest{
#' data("Physeq")
#' physeqs<-prefilter(physeq)
#' }
#' @return filter phyloseq object
#' @author Kai Guo
#' @export

prefilter<-function(physeq,min=10,perc=0.05){
    ##remove "" in phylum level
    ps <- subset_taxa(physeq, !is.na(Phylum) & !Phylum %in% c("", "uncharacterized"))
                      # Compute prevalence of each feature, store as data.frame
    prevdf = apply(X = otu_table(ps),
               MARGIN = ifelse(taxa_are_rows(ps), yes = 1, no = 2),
               FUN = function(x){sum(x > 0)})
    # Add taxonomy and total read counts to this data.frame
    prevdf = data.frame(Prevalence = prevdf,
                    TotalAbundance = taxa_sums(ps),
                    tax_table(ps))
    #compute the total and the average prevalences of the features in each phylum
    prer<-ddply(prevdf, "Phylum", function(df1){cbind(mean(df1$Prevalence),sum(df1$Prevalence))})
    colnames(prer)[2:3]<-c("average","total")
    filterPhyla<-prer$Phylum[which(prer$total/prer$average<min)]
    tax<-as.data.frame(as(tax_table(ps),"matrix"))
    tax<-subset(tax,!Phylum%in%filterPhyla)
    tax_table(ps)<-tax_table(as(tax,"matrix"))
   # ps1 = phyloseq::subset_taxa(ps, Phylum %in%filterPhyla)
    prevdf1 = subset(prevdf, Phylum %in% get_taxa_unique(ps, "Phylum"))
    prevalenceThreshold = perc * nsamples(ps)
    message("prevalence Threshold is: ",prevalenceThreshold)
    keepTaxa = rownames(prevdf1)[(prevdf1$Prevalence >= prevalenceThreshold)]
    prevdfr<-prevdf1[keepTaxa,]
    psf = prune_taxa(keepTaxa, ps)
}

#' @title calculat the richness for the phyloseq object
#' @importFrom phyloseq estimate_richness otu_table
#' @importFrom vegan rarefy
#' @importFrom vegan diversity
#' @importFrom vegan specnumber
#' @importFrom phyloseq otu_table
#' @param physeq A \code{phyloseq} object containing merged information of abundance,
#'        taxonomic assignment, sample data including the measured variables and categorical information
#'        of the samples, and / or phylogenetic tree if available.
#' @param method A list of character strings specifying \code{method} to be used to calculate for alpha diversity
#'        in the data. Available methods are: "Observed","Chao1","ACE","Richness", "Fisher", "Simpson", "Shannon", "Evenness","InvSimpson".
#' @examples
#' {
#' data("Physeq")
#' rich <-richness(physeq,method=c("Simpson", "Shannon"))
#' }
#' @return data.frame of alpha diversity
#' @export
#' @author Kai Guo
richness<-function(physeq,method=c("Observed","Simpson", "Shannon")){
    method<-as.character(sapply(method,function(x)simpleCap(x),simplify = T))
    method<- match.arg(method,c("Observed","Chao1","ACE","Richness", "Fisher", "Simpson", "Shannon", "Evenness","InvSimpson"), several.ok = TRUE)
    df <- estimate_richness(physeq)
    if(!isTRUE(taxa_are_rows(physeq))){
        tab<-t(otu_table(physeq))
    }else{
        tab<-otu_table(physeq)
    }
    rownames(df)<-colnames(tab)
    if("Evenness"%in%method){
        ta<-as.data.frame(t(tab))
        H<-diversity(ta)
        S <- specnumber(ta)
        J <- H/log(S)
        df_J<-data.frame(Evenness=J)
        df<-cbind(df,df_J)
    }
    if("Richness"%in%method){
        ta<-as.data.frame(t(tab))
        R<- rarefy(ta,min(rowSums(ta)))
        df_R<-data.frame(Richness=R)
        df<-cbind(df,df_R)
    }
    df<-df[,method,drop=FALSE]
    return(df)
}
#' @title calcaute beta diversity
#' @importFrom phyloseq ordinate
#' @param physeq A \code{phyloseq} object containing merged information of abundance,
#'        taxonomic assignment, sample data including the measured variables and categorical information
#'        of the samples, and / or phylogenetic tree if available.
#' @param method A character string specifying ordination method. All methods available to the \code{ordinate} function
#'        of \code{phyloseq} are acceptable here as well.
#' @param distance A string character specifying dissimilarity index to be used in calculating pairwise distances (Default index is "bray".).
#'                       "unifrac","wunifrac","manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower",
#'                       "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao" or "mahalanobis".
#' @export
#' @author Kai Guo
#' @examples
#' {
#' data("Physeq")
#' phy<-normalize(physeq)
#' res <- betadiv(phy)
#' }
#' @return list with beta diversity data.frame and PCs
betadiv<-function(physeq,distance="bray",method="PCoA"){
    beta<-ordinate(physeq,method = method,distance = distance)
    pcs<-beta$values[,2]
    df<-beta$vectors
    return(list(beta=df,PCs=pcs))
}
#' @title PERMANOVA test for phyloseq
#' @importFrom phyloseq distance
#' @importFrom vegan adonis
#' @importFrom tidyr gather
#' @importFrom dplyr group_by
#' @importFrom dplyr do
#' @importFrom magrittr %>%
#' @param physeq A \code{phyloseq} object containing merged information of abundance,
#'        taxonomic assignment, sample data including the measured variables and categorical information
#'        of the samples, and / or phylogenetic tree if available.
#' @param distance A string character specifying dissimilarity index to be used in calculating pairwise distances (Default index is "bray".).
#'                       "unifrac","wunifrac","manhattan", "euclidean", "canberra", "bray", "kulczynski", "jaccard", "gower", "altGower",
#'                       "morisita", "horn", "mountford", "raup" , "binomial", "chao", "cao" or "mahalanobis".
#' @param group (Required). Character string specifying name of a categorical variable that is preferred for grouping the information.
#'        information.
#' @examples
#'{
#' data("Physeq")
#' phy<-normalize(physeq)
#' beta <-betatest(phy,group="SampleType")
#' }
#' @return PERMANOVA test result
#' @export
#' @author Kai Guo
betatest<-function(physeq,group,distance="bray"){
    message("Do PERMANOVA for: ",group)
    dist<-distance(physeq,method = distance)
    tab <- as(sample_data(physeq),"data.frame")
    tab<-tab[,group,drop=F]
    res<-NULL
    if(length(group)>1){
        res<- tab%>%gather(Group,val)%>%group_by(Group)%>%do(as.data.frame(adonis(dist~val,.)$aov.tab))
    }else{
        tab$Group <- tab[,group]
        res<-as.data.frame(adonis(dist~Group,tab)$aov.tab)
    }
    return(as.data.frame(res))
}

#' Normalize the phyloseq object with different methods
#' @importFrom phyloseq transform_sample_counts sample_data
#' @importFrom phyloseq taxa_are_rows nsamples otu_table psmelt
#' @importFrom DESeq2 DESeqDataSetFromMatrix estimateSizeFactors
#' @importFrom DESeq2 estimateDispersions varianceStabilizingTransformation
#' @importFrom SummarizedExperiment assay
#' @importFrom S4Vectors DataFrame
#' @importFrom edgeR calcNormFactors
#' @param physeq A \code{phyloseq} object containing merged information of abundance,
#'      taxonomic assignment, sample data including the measured variables and categorical information
#'      of the samples, and / or phylogenetic tree if available.
#' @param method A list of character strings specifying \code{method} to be used to normalize the phyloseq object
#'      Available methods are: "relative","TMM","vst","log2".
#' @param group group (DESeq2). A character string specifying the name of a categorical variable containing  grouping information.
#' @param table return a data.frame or not
#' @examples
#' {
#' data("Physeq")
#' phy<-normalize(physeq)
#' }
#' @return phyloseq object with normalized data
#' @author Kai Guo
#' @export
normalize<-function(physeq,group,method="relative",table=FALSE){
    if(!taxa_are_rows(physeq)){
        physeq<-t(physeq)
    }
    otu<-as(otu_table(physeq),"matrix")
    tab<-as(sample_data(physeq),"data.frame")
    group<-tab[,group]
    if(method=="vst"){
        message("Normalization using DESeq2 varianceStabilizingTransformation method")
        otu <- otu+1
        condition=group
        dds = DESeqDataSetFromMatrix(otu, DataFrame(condition), ~ condition)
        dds = estimateSizeFactors(dds)
        dds = estimateDispersions(dds)
        vst <- varianceStabilizingTransformation(dds)
        otu_table(physeq) <- otu_table(assay(vst), taxa_are_rows=TRUE)
    }
    if(method=="relative"){
        message("Normalization using relative method ")
        physeq<-transform_sample_counts(physeq,function(x)x/sum(x))
    }
    if(method=="TMM"){
        # modified from https://github.com/aametwally/MetaLonDA/blob/master/R/Normalization.
        message("Normalization using TMM method ")
        otu = otu + 1
        # Check `group` argument
        factors = calcNormFactors(otu, method="TMM")
        eff.lib.size = colSums(otu) * factors
        ref.lib.size = mean(eff.lib.size) #Use the mean of the effective library sizes as a reference library size
        count = sweep(otu, MARGIN = 2, eff.lib.size, "/") * ref.lib.size
        otu_table(physeq) <- otu_table(count, taxa_are_rows=TRUE)
    }
    if(method=="log2"){
        message("Normalization using log2 of the RA method ")
        physeq<-transform_sample_counts(physeq,function(x)log2(x/sum(x)+1))
    }
    if(isTRUE(table)){
      physeq <- psmelt(physeq)
    }
    return(physeq)
}
#' @title Calculate differential bacteria with DESeq2
#' @importFrom DESeq2 DESeqDataSetFromMatrix counts
#' @importFrom phyloseq otu_table taxa_are_rows
#' @importFrom phyloseq sample_data
#' @importFrom DESeq2 results DESeq
#' @importFrom stats as.formula
#' @param physeq A \code{phyloseq} object containing merged information of abundance,
#'      taxonomic assignment, sample data including the measured variables and categorical information
#'      of the samples, and / or phylogenetic tree if available.
#' @param group group (DESeq2). A character string specifying the name of a categorical variable containing  grouping information.
#' @param ref reference group
#' @param pvalue pvalue threshold for significant results
#' @param padj adjust p value threshold for significant results
#' @param log2FC log2 Fold Change threshold
#' @param gm_mean TRUE/FALSE calculate geometric means prior to estimate size factors
#' @param fitType either "parametric", "local", or "mean" for the type of fitting of dispersions to the mean intensity.
#' @param quiet whether to print messages at each step
#' @examples
#'  \donttest{
#' data("Physeq")
#' res <- difftest(physeq,group="group")
#' }
#' @return datafame with differential test with DESeq2
#' @author Kai Guo
#' @export
#'
difftest<-function(physeq,group,ref=NULL,pvalue=0.05,padj=NULL,log2FC=0,gm_mean=TRUE,fitType="local",quiet=FALSE){
    if(!taxa_are_rows(physeq)){
        physeq<-t(physeq)
    }
    otu <- as(otu_table(physeq),"matrix")
    tax <- as.data.frame(as.matrix(tax_table(physeq)))
    colData<-as(sample_data(physeq),"data.frame")
    colData$condition<-colData[,group]
    contrasts<-levels(factor(unique(colData$condition)))
    if(!is.null(ref)){
      contrasts <- c(setdiff(contrasts,ref),ref)
    }
    if(isTRUE(gm_mean)){
        countData<-round(otu, digits = 0)
    }else{
        countData<-round(otu, digits = 0)+1
    }
    dds <- DESeqDataSetFromMatrix(countData, colData, as.formula(~condition))
    if(isTRUE(gm_mean)){
        geoMeans = apply(counts(dds), 1, gm_mean)
        dds = estimateSizeFactors(dds, geoMeans = geoMeans)
    }
    dds <- DESeq(dds, fitType=fitType)
    res <- results(dds,contrast=c("condition",contrasts),cooksCutoff = FALSE)
    res_tax = cbind(as.data.frame(res), as.matrix(countData[rownames(res), ]))
    if(!is.null(padj)){
        pval<-padj
        sig <- rownames(subset(res,padj<pval&abs(log2FoldChange)>log2FC))
    }else{
        pval<-pvalue
        sig <- rownames(subset(res,pvalue<pval&abs(log2FoldChange)>log2FC))
    }
    res_tax$Significant<- "No"
    res_tax$Significant <- ifelse(rownames(res_tax) %in% sig, "Yes", "No")
    res_tax <- cbind(res_tax, tax[rownames(res),])
    return(as.data.frame(res_tax))
}

#' @title Identify biomarker by using randomForest method
#' @importFrom phyloseq taxa_are_rows otu_table sample_data t
#' @importFrom randomForest randomForest importance
#' @importFrom tidyr gather
#' @importFrom dplyr group_by filter
#' @importFrom dplyr do
#' @importFrom magrittr %>%
#' @importFrom broom tidy
#' @importFrom stats kruskal.test
#' @param physeq A \code{phyloseq} object containing merged information of abundance,
#'      taxonomic assignment, sample data including the measured variables and categorical information
#'      of the samples, and / or phylogenetic tree if available.
#' @param group group. A character string specifying the name of a categorical variable containing  grouping information.
#' @param ntree Number of trees to grow. This should not be set to too small a number,
#'  to ensure that every input row gets predicted at least a few times.
#' @param pvalue pvalue threshold for significant results from kruskal.test
#' @param normalize to normalize the data before analysis(TRUE/FALSE)
#' @param method A list of character strings specifying \code{method} to be used to normalize the phyloseq object
#'      Available methods are: "relative","TMM","vst","log2".
#' @examples
#' \donttest{
#' data("Physeq")
#' res <- biomarker(physeq,group="group")
#' }
#' @return data frame with significant biomarker
#' @author Kai Guo
#' @export
biomarker<-function(physeq,group,ntree=500,pvalue=0.05,normalize=TRUE,method="relative"){
    if(isTRUE(normalize)){
        physeq<-normalize(physeq,method = method)
    }
    if(taxa_are_rows(physeq)){
        physeq<-t(physeq)
    }
    tax <- as.data.frame(as.matrix(tax_table(physeq)))
    sam <- as(sample_data(physeq),"data.frame")
    tab <- as.data.frame(otu_table(physeq))
    tab$group<-sam[,group]
    sel<-tab%>%gather(OTU,val,-group)%>%group_by(OTU)%>%do(tidy(kruskal.test(val~group,.)))%>%
      filter(p.value<0.05)
    data<-tab[,sel$OTU]
    #change the colnames in case only have number in the colname
    colnames(data)<-paste0("X",colnames(data))
    data$group<-tab$group
    data$group<-factor(data$group)
    val<-randomForest(group ~ ., data=data, importance=TRUE, proximity=TRUE,ntree=ntree)
    print(val)
    imp<- importance(val)
    res<-data.frame(row.names=NULL,OTU=sub('X','',rownames(imp)),
                    Value=abs(as.numeric(imp[,"MeanDecreaseAccuracy"])),
                    Index=rep("Mean Decrease Accuracy",dim(imp)[1]))
    #Rearrange the features in terms of importance for ggplot2 by changing factor levels
    res$rank <- rank(res$Value, ties.method = "min")
    res$rank <- max(res$rank)-res$rank+1
    res<-cbind(res,tax[res$OTU,])
    res<-res[order(res$rank),]
    res
}
#' Identify biomarker by using LEfSe method
#' @importFrom phyloseq taxa_are_rows otu_table sample_data
#' @importFrom phyloseq `otu_table<-`
#' @importFrom dplyr group_by summarize do left_join
#' @importFrom dplyr ungroup bind_rows mutate
#' @importFrom broom tidy
#' @importFrom magrittr %>%
#' @importFrom stats p.adjust
#'
#' @param physeq A \code{phyloseq} object containing merged information of abundance,
#'      taxonomic assignment, sample data including the measured variables and categorical information
#'      of the samples, and / or phylogenetic tree if available.
#' @param group group. A character string specifying the name of a categorical variable containing  grouping information.
#' @param pvalue pvalue threshold for significant results from kruskal.test
#' @param normalize to normalize the data before analysis(TRUE/FALSE)
#' @param method A list of character strings specifying \code{method} to be used to normalize the phyloseq object
#'      Available methods are: "relative","TMM","vst","log2".
#' @examples
#' \donttest{
#' data("Physeq")
#' res <- ldamarker(physeq,group="group")
#' }
#'
#' @author Kai Guo
#' @export
ldamarker<-function(physeq,group,pvalue=0.05,normalize=TRUE,method="relative"){
    if(isTRUE(normalize)){
        physeq<-normalize(physeq,method = method)
        # count per million *10e6 (CPM)
        otu_table(physeq)<-otu_table(physeq)*10e6
    }
    if(!taxa_are_rows(physeq)){
        physeq<-t(physeq)
    }
    tax <- as.data.frame(as.matrix(tax_table(physeq)))
    sam<-as(sample_data(physeq),"data.frame")
    level<-colnames(tax)
    tab<-psmelt(physeq)
    otul <- lapply(1:length(level),function(i) {
          lvls <- level[1:i]
          lvl <- level[i]
          otu_lev <- tab
          otu_lev$tax <- do.call(paste,c(lapply(lvls,function(l) tab[[l]]),sep="|"))
          otu_lev$rank <- lvl
          otu_lev2 <- otu_lev %>% group_by(Sample,tax,rank) %>%
              summarize(seqs=sum(Abundance)) %>% ungroup()
        return(otu_lev2)
        })
    otu <- bind_rows(otul) %>%
            mutate(tax=gsub("\\|","_",tax))
    ###
    otu$group<-sam[otu$Sample,group]
    ###
    pvalues<-otu%>%group_by(rank,tax)%>%do(tidy(kruskal.test(seqs~group,.)))
    pvalues$p.adj<-p.adjust(pvalues$p.value, method ="fdr");
    ##
    df<-pvalues%>%left_join(otu,by=c("tax"="tax"))
    dd<-df[df$p.value<pvalue,]
    dfl<-split(dd,dd$tax)
    #Linear Discriminant analysis (LDA)
    ldares <- lapply(dfl, function(x).lda.fun(x))
    resTable <- do.call(rbind,ldares);
    resTable$tax <- rownames(resTable)
    resTable <- pvalues%>%left_join(resTable,by=c("tax"="tax"))
    return(resTable)
}
