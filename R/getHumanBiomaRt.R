#' wrapper for getting ensemblID association with gene symbols
#' #'@export
getHumanBiomaRt <- function(){
      mart <- biomaRt::useMart( biomart = 'ENSEMBL_MART_ENSEMBL')
      martData <- biomaRt::useDataset( dataset = 'hsapiens_gene_ensembl', mart = mart )
      return(martData)
}