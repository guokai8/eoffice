#' @title Do the generalized linear model regression
#' @importFrom phyloseq taxa_are_rows otu_table sample_data
#' @importFrom broom tidy
#' @importFrom stats binomial glm
#' @param physeq phyloseq object
#' @param group the group factor to regression
#' @param factors  a vector to indicate adjuested factors
#' @param ref the reference group
#' @param family binomial() or gaussian()
#' @examples
#' \donttest{
#' data("Physeq")
#' phy<-normalize(physeq)
#' fit <-glmr(phy,group="SampleType")
#' }
#' @export
#' @author Kai Guo

glmr<-function(physeq,group,factors=NULL,ref=NULL,family=binomial(link = "logit")){
      if (!taxa_are_rows(physeq)) {
            physeq <- t(physeq)
      }
      otu <- as(otu_table(physeq), "matrix")
      otu <- as.data.frame(t(otu))
      colnames(otu)<-paste0('ASV_',colnames(otu))
      samd <- sample_data(physeq)[,c(group,factors)]
      dd <- cbind(samd[rownames(otu),],otu)
      if(!is.null(ref)){
         level <- unique(dd[,group])
         level <- c(ref,setdiff(level,ref))
      }else{
         level <- unique(dd[,group])
      }
      dd[,group]<-factor(dd[,group],levels = level)
      cat('##########################################\n')
      cat('Do the generalized linear model regression with ',factors,'adjusted',"\n")
      cat(paste0(group,"~",paste0(factors,collapse="+"),"+x"),"\n")
      cat('##########################################\n')
      rr<-lapply(colnames(otu)[1:50],function(x)tidy(glm(as.formula(paste0(group,"~",paste0(factors,collapse="+"),"+",x)),data=dd,family=family)))
      names(rr)<- sub('ASV_','',colnames(otu)[1:50])
      res <- do.call(rbind,rr)
      res <- res[grep('ASV_',res$term),]
      res$term<-sub('ASV_','',res$term)
      res$padj <- p.adjust(res$p.value,method="BH")
      res <- res[order(res$padj),]
      return(res)
}
