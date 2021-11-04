#' Perform dada2 analysis
#' @importFrom phyloseq phyloseq otu_table sample_data tax_table
#' @importFrom utils read.delim write.table
#' @param path working dir for the input reads
#' @param truncLen (Optional). Default 0 (no truncation). Truncate reads after truncLen bases. Reads shorter than this are discarded.
#' @param trimLeft (Optional). The number of nucleotides to remove from the start of each read.
#' @param trimRight (Optional). Default 0. The number of nucleotides to remove from the end of each read.
#'        If both truncLen and trimRight are provided, truncation will be performed after trimRight is enforced.
#' @param sample_info (Optional).sample information for the sequence
#' @param minLen (Optional). Default 20. Remove reads with length less than minLen. minLen is enforced after trimming and truncation.
#' @param maxLen Optional). Default Inf (no maximum). Remove reads with length greater than maxLen. maxLen is enforced before trimming and truncation.
#' @param train_data (Required).training database
#' @param train_species (Required). species database
#' @param outpath (Optional).the path for the filtered reads and th out table
#' @param saveobj (Optional).Default FALSE. save the phyloseq object output.
#' @param buildtree build phylogenetic tree or not(default: FALSE)
#' @param verbose (Optional). Default TRUE. Print verbose text output.
#' @author Kai Guo
#' @return list include count table, summary table, taxonomy information and phyloseq object
#' @export
processSeq <- function(path=".",
                       truncLen = c(0, 0),
                       trimLeft=0,
                       trimRight=0,
                       minLen=20,
                       maxLen=Inf,
                       sample_info=NULL,
                       train_data="silva_nr99_v138_train_set.fa.gz",
                       train_species="silva_species_assignment_v138.fa.gz",
                       outpath=NULL,
                       saveobj=FALSE,
                       buildtree=FALSE,
                       verbose=TRUE){
    OS<-.Platform$OS.type
    if(OS=="windows"){
        multithread<-FALSE
    }else{
        multithread<-TRUE
    }
    fnFs <- sort(list.files(path, pattern="R1", full.names = TRUE))
    fnRs <- sort(list.files(path, pattern="R2", full.names = TRUE))
    message("check the filename ......")
    if(any(grepl('R1|R2',fnFs)==FALSE)){
        stop("All fastq name should be either contain R1 or R2 \n")
    }
    sample.names <-sub('@@@@.*','',sub('(\\.|_)R(1|2)','@@@@',basename(fnFs)))
    if(sum(duplicated(sample.names))>=1){
        stop('The fastq filenames are not unique!\n')
    }
    #filter and trim;
    if(isTRUE(verbose)){
    message("Filtering......");
    }
    if(is.null(outpath)){
        outpath<-path
    }
    ifelse(!dir.exists(paste0(outpath,"/filtered")),dir.create(file.path(outpath,"filtered"),recursive=TRUE),FALSE);
    filtFs <- file.path(outpath, "filtered", paste0(sample.names, "_F_filt.fastq.gz"))
    filtRs <- file.path(outpath, "filtered", paste0(sample.names, "_R_filt.fastq.gz"))
    out <- dada2::filterAndTrim(fnFs, filtFs, fnRs, filtRs, truncLen=truncLen,trimLeft=trimLeft,trimRight=trimRight,
                         maxN=0, maxEE=c(2,2), truncQ=2, rm.phix=TRUE,minLen=minLen,maxLen = maxLen,
                         compress=TRUE, multithread=multithread) # On Windows set multithread=FALSE
    if(isTRUE(verbose)){
        message("Learning error......")
    }
    errF <- dada2::learnErrors(filtFs, multithread=multithread)
    errR <- dada2::learnErrors(filtRs, multithread=multithread)
    if(isTRUE(verbose)){
        message("Dereplicating......")
    }
    derepFs <- dada2::derepFastq(filtFs, verbose=FALSE)
    derepRs <- dada2::derepFastq(filtRs, verbose=FALSE)
    # Name the derep-class objects by the sample names
    names(derepFs) <- sample.names
    names(derepRs) <- sample.names
    if(isTRUE(verbose)){
        message("Error correction......")
    }
    dadaFs <- dada2::dada(derepFs, err=errF, multithread=TRUE)
    dadaRs <- dada2::dada(derepRs, err=errR, multithread=TRUE)
    if(isTRUE(verbose)){
        message("Mergering.......")
    }
    mergers <- dada2::mergePairs(dadaFs, derepFs, dadaRs, derepRs, verbose=FALSE)
    if(isTRUE(verbose)){
        message("Making table.......")
    }
    seqtab <- dada2::makeSequenceTable(mergers)
    if(isTRUE(verbose)){
        message("Remove chimeras.......")
    }
    seqtab.nochim <- dada2::removeBimeraDenovo(seqtab, method="consensus", multithread=TRUE, verbose=FALSE)
    asv_seqs <- colnames(seqtab.nochim)
    asv_headers <- vector(dim(seqtab.nochim)[2], mode="character")
    for (i in 1:dim(seqtab.nochim)[2]) {
        asv_headers[i] <- paste(">ASV", i, sep="_")
    }
    getN <- function(x) sum(dada2::getUniques(x))
    track <- cbind(out, sapply(dadaFs, getN), sapply(dadaRs, getN), sapply(mergers, getN), rowSums(seqtab.nochim))
    # If processing a single sample, remove the sapply calls: e.g. replace sapply(dadaFs, getN) with getN(dadaFs)
    colnames(track) <- c("input", "filtered", "denoisedF", "denoisedR", "merged", "nonchim")
    rownames(track) <- sample.names
    # count table:
    asv_count <- t(seqtab.nochim)
    rownames(asv_count) <- sub(">", "", asv_headers)
    ### set back to the previous work dir
    if(isTRUE(verbose)){
        message("Write out the count table.......")
    }
    write.table(asv_count,paste0(outpath, "/ASVs_counts.txt"), sep="\t", quote=F)
    if(isTRUE(verbose)){
        message("Assign taxonomy........")
    }
    if(is.null(train_data)|is.null(train_species)){
        stop("Please specify the path for the sliva database......\n ")
    }else{
    taxa <- dada2::assignTaxonomy(seqtab.nochim, train_data, multithread=multithread)
    taxa <- dada2::addSpecies(taxa, train_species)
    ###
    taxtab<-unname(taxa)
    ### get sequence and do phylo anaylsis
    seqs <- dada2::getSequences(seqtab.nochim)
    names(seqs) <- seqs # This propagates to the tip labels of the tree
    if(isTRUE(verbose)){
        message("write out sequence and taxonomy results")
    }
    # fasta:
    asv_fasta <- c(rbind(asv_headers, asv_seqs))
    write(asv_fasta,paste0(outpath, "/ASVs.fa"))
    # tax table:
    asv_taxa <- taxa
    row.names(asv_taxa) <- sub(">", "", asv_headers)
    write.table(asv_taxa, paste0(outpath,"/ASVs_taxonomy.txt"), sep="\t", quote=F)
    }
    if(isTRUE(verbose)){
        message("creating phyloseq object......")
    }
    if(!is.null(sample_info)){
        if(is.character(sample_info)){
            ext<-.checkfile(sample_info)
            if(ext=="txt"){
                sampdf<-read.delim(sample_info,sep="\t",header = TRUE,row.names = 1)
                }
            if(ext=="csv"){
                sampdf<-read.delim(sample_info,sep=",",header = TRUE,row.names = 1)
                }
            sampdf<-sampdf[rownames(seqtab.nochim),]
            }else{
                sampdf<-sample_info
                sampdf<-sampdf[rownames(seqtab.nochim),]
        }
    }else{
        sampdf<-data.frame(ID=colnames(asv_count))
        rownames(sampdf)<-colnames(asv_count)
    }
    if(isTRUE(buildtree)){
        tree <- buildTree(seqs)
    }
    ps <- phyloseq(otu_table(asv_count, taxa_are_rows=T),
                   sample_data(sampdf),
                   tax_table(asv_taxa))
    if(isTRUE(saveobj)){
        save(ps,file=paste0(outpath,"phyloseq.rdata"),compress=TRUE)
    }
    res<-list(track=track,count=asv_count,taxonomy=asv_taxa,physeq=ps)
    return(res)
}
