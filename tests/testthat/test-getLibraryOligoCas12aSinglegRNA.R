test_that('getLibraryOligoCas12aTriplegRNA', {
      # test for result that has been manually checked in Snapgene after in silico cloning into pCH49
      expect_equal(
        oligo <- bears01::getLibraryOligoCas12aSinglegRNA(
              pos1name = 'CD81-1',
              pos1spacer = 'GGCGCGACCCCCAGGAAGGTCTC',
              Adaptor5p = 'ATTTTGCCCCTGGTTCTT',
              Adaptor3p = 'CCAGTTCATTTCTTAGGG',
              polyTcheck = FALSE ),
        list( oligosequence = 'ATTTTGCCCCTGGTTCTTCGTCTCAAGATGGCGCGACCCCCAGGAAGGTCTCAATTCGAGACGCCAGTTCATTTCTTAGGG', oligoname = 'CD81-1' )
  )
      #check for problematic inclusion of BsmbI site in adaptor
      expect_error(
            getLibraryOligoCas12aTriplegRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'GGCGCGACCCCCAGGAAGGTCTC',
                  Adaptor5p = 'ATTTTGCCCCTGcgtctc',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG',
                  polyTcheck = FALSE
            )
      )
      # test for spacers starting with TTT
      expect_error(
            getLibraryOligoCas12aTriplegRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'TTTGCGACCCCCAGGAAGGTCTC',
                  Adaptor5p = 'ATTTTGCCCCTGGTTCTT',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG',
                  polyTcheck = TRUE )
      )

      # test for spacer containing TTTT
      expect_error(
            getLibraryOligoCas12aTriplegRNA(
                  pos1name = 'B2M-1',
                  pos1spacer = 'ATATAAGTGTTTTCGTCGCGCTG',
                  Adaptor5p = 'ATTTTGCCCCTGGTTCTT',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG',
                  polyTcheck = TRUE
            )
      )
})
