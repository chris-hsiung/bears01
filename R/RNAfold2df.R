#' Function to parse the output from RNAfold 2.4.18 into data frame and write to tab-delimited file
#' parse output according to https://www.tbi.univie.ac.at/RNA/RNAfold.1.html#heading5
#' this description is based on manual entry of a single sequence -- "Here, the first line just repeats the sequence input. The second line contains a MFE structure in dot bracket notation followed by the minimum free energy. After this, the pairing probabilities for each nucleotide are shown in a pseudo dot-bracket notation followed by the free energy of ensemble. The next two lines show the centroid structure with its free energy and its distance to the ensemble as well as the MEA structure, its free energy and the maximum expected accuracy, respectively. The last line finally contains the frequency of the MFE representative in the complete ensemble of secondary structures and the ensemble diversity."
#' the output for multiple sequences contains same info formatted slightly differently, figured it out and parsed accordingly.
# the call to RNAfold should be with the following parameters 'RNAfold --MEA -d2 -p --noPS --noDP < test.txt > test.out'. 'file' is path to output from RNAfold. The basename of this file must have the suffix '.txt'.
#' writes a tab-delimited file with the in the format '{basename of input file}df.txt'
#' @export
RNAfold2df <- function( file, writedir = getwd() ){
      library(dplyr)
      library(tidyr)
      library(stringr)
      library(Biostrings)

      assertthat::assert_that( grepl( '\\.txt$', file), msg = 'please use an input file with suffix .txt' )

      lines <- readLines(file)

      # check input arguments
      assertthat::assert_that( length(lines) > 0, msg = 'empty file' )

      numentries <- length(lines)/6

      allids <- c()

      for ( i in 1:numentries ){
            singleid <- rep( i , 6 )
            allids <- c( allids, singleid )
      }

      assertthat::assert_that( length(allids) == length(lines), msg = paste0( 'number of allids is not equal to number of lines in file') )

      variable <- rep( c('sequence', 'MFE', 'Ensemble',  'Centroid', 'MEA', 'MFEStructFreq; EnsembleDiversity' ), numentries )

      folddf <- data.frame( seqID = allids, line = lines ) %>%
            cbind( . , variable ) %>%
            tidyr::pivot_wider( id_cols = 'seqID', names_from = 'variable', values_from = 'line' ) %>%
            tidyr::separate( col = MFE, into = c('MFEstruct', 'MFE'), sep = ' \\(' , remove = TRUE ) %>%
            dplyr::mutate( MFE = str_remove( MFE, '\\)') ) %>%
            tidyr::separate( col = Ensemble, into = c('Ensemblestruct', 'EnsembleFE'), sep = ' \\[', remove = TRUE ) %>%
            dplyr::mutate( EnsembleFE = str_remove( EnsembleFE, '\\]') ) %>%
            tidyr::separate( col = Centroid, into = c('Centroidstruct', 'centroidholder'), sep = ' \\{', remove = TRUE ) %>%
            dplyr::mutate( centroidholder = str_remove( centroidholder, '^ ') ) %>%
            tidyr::separate( col = centroidholder, into = c('CentroidFE', 'CentroidDist'), sep = ' d=' ) %>%
            dplyr::mutate( CentroidDist = str_remove( CentroidDist, '\\}') ) %>%
            tidyr::separate( col = MEA, into = c('MEAstruct', 'MEAholder'), sep = ' \\{', remove = TRUE ) %>%
            dplyr::mutate( MEAholder = str_remove( MEAholder, '^ ') ) %>%
            tidyr::separate( col = MEAholder, into = c('MEAstructFE', 'MEA'), sep = ' MEA=', remove = TRUE ) %>%
            dplyr::mutate( MEA = str_remove( MEA, '\\}')) %>%
            tidyr::separate( col = `MFEStructFreq; EnsembleDiversity`, into = c('MFEStructFreq', 'EnsembleDiversity'), sep = '\\; ', remove = TRUE ) %>%
            dplyr::mutate(
                  MFEStructFreq = str_remove( MFEStructFreq, ' frequency of mfe structure in ensemble ' ),
                  EnsembleDiversity = str_remove( EnsembleDiversity, 'ensemble diversity ')
            )

      # use Biostrings package to check input sequence string corresponds to the correct type of nucleic acid
      sapply( folddf$sequence, Biostrings::RNAString )


      ## some QC checks
      QCdf <- dplyr::mutate(folddf,
                            lengthcheck = ifelse( nchar( sequence ) == nchar(MFEstruct) &
                                                        nchar( sequence ) == nchar(Ensemblestruct) &
                                                        nchar( sequence ) == nchar(Centroidstruct) &
                                                        nchar( sequence ) == nchar(MEAstruct), TRUE, FALSE ),
      )

      assertthat::assert_that( sum( QCdf$lengthcheck ) == dim(QCdf)[1] , msg = 'length of structure does not match length of sequence' )

      assertthat::assert_that( any( sapply( QCdf , grepl, pattern = ' ') ), msg = 'empty space exists in data, there is formatting error' )

      # get basename only from input file path to create output file name
      outfilename <- stringr::str_replace( basename(file), '\\.txt$', 'df.txt')

      write.table( folddf, file = file.path( writedir, outfilename ), sep = '\t', col.names = TRUE, row.names = FALSE, quote = FALSE )

      return(folddf)
}
