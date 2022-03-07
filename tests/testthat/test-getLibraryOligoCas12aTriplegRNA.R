test_that("getLibraryOligoCas12aTriplegRNA", {
      # test for result that has been manually checked in Snapgene after in silico cloning into pCH49
      expect_equal(
            getLibraryOligoCas12aTriplegRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'GGCGCGACCCCCAGGAAGGTCTC',
                  pos2name = 'B2M-1',
                  pos2spacer = 'ATATAAGTGGAGGCGTCGCGCTG',
                  pos3name = 'KIT-2',
                  pos3spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                  Adaptor5p = 'ATTTTGCCCCTGGTTCTT',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG'),
            list( 'oligosequence' = "ATTTTGCCCCTGGTTCTTCGTCTCAAGATGGCGCGACCCCCAGGAAGGTCTCAATTTCTACTGTCGTAGATATATAAGTGGAGGCGTCGCGCTGAATTTCTACTCTAGTAGATTCTGCGTTCTGCTCCTACTGCTTAATTCGAGACGCCAGTTCATTTCTTAGGG", 'oligoname' = "CD81-1_B2M-1_KIT-2")
)
      # test for spacers starting with TTT
      expect_error(
            getLibraryOligoCas12aTriplegRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'TTTGCGACCCCCAGGAAGGTCTC',
                  pos2name = 'B2M-1',
                  pos2spacer = 'ATATAAGTGGAGGCGTCGCGCTG',
                  pos3name = 'KIT-2',
                  pos3spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                  Adaptor5p = 'ATTTTGCCCCTGGTTCTT',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG')
      )
      # test for spacer containing TTTT
      expect_error(
                  getLibraryOligoCas12aTriplegRNA(
                        pos1name = 'CD81-1',
                        pos1spacer = 'GGCGCGACCCCCAGGAAGGTCTC',
                        pos2name = 'B2M-1',
                        pos2spacer = 'ATATAAGTGTTTTCGTCGCGCTG',
                        pos3name = 'KIT-2',
                        pos3spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                        Adaptor5p = 'ATTTTGCCCCTGGTTCTT',
                        Adaptor3p = 'CCAGTTCATTTCTTAGGG'
                  )
      )
      #check for problematic inclusion of BsmbI site in adaptor
      expect_error(
            getLibraryOligoCas12aTriplegRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'GGCGCGACCCCCAGGAAGGTCTC',
                  pos2name = 'B2M-1',
                  pos2spacer = 'ATATAAGTGGAGGCGTCGCGCTG',
                  pos3name = 'KIT-2',
                  pos3spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                  Adaptor5p = 'ATTTTGCCCCTGcgtctc',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG'
            )
      )
      #check for warning for unusual adaptor length
      expect_warning(
            getLibraryOligoCas12aTriplegRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'GGCGCGACCCCCAGGAAGGTCTC',
                  pos2name = 'B2M-1',
                  pos2spacer = 'ATATAAGTGGAGGCGTCGCGCTG',
                  pos3name = 'KIT-2',
                  pos3spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                  Adaptor5p = 'ATTTTGCCCCTGTGAGCCTGGG',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG'
            ),
            "adaptors are not 18nt long, this is unexpected"
      )
      # check for non-DNA characters
      expect_error(
            getLibraryOligoCas12aTriplegRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'GGCGCGACCCCCxyzAAGGTCTC',
                  pos2name = 'B2M-1',
                  pos2spacer = 'ATATAAGTGGAGGCGTCGCGCTG',
                  pos3name = 'KIT-2',
                  pos3spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                  Adaptor5p = 'ATTTTGCCCCTGGTTCTT',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG'
            )
      )
        })

