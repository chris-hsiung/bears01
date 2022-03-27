test_that("getLibraryOligoCas12a6plexgRNA", {
      # test for result that has been manually checked in Snapgene after in silico cloning into pCH49 -- see manualinspection directory
      expect_equal(
            getLibraryOligoCas12a6plexgRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'GGCGCGACCCCCAGGAAGGTCTC',
                  pos2name = 'B2M-1',
                  pos2spacer = 'ATATAAGTGGAGGCGTCGCGCTG',
                  pos3name = 'B2M-3',
                  pos3spacer = 'AGGAATGCCCGCCAGCGCGACGC',
                  pos4name = 'KIT-2',
                  pos4spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                  pos5name = 'KIT-3',
                  pos5spacer = 'AGCTCTCGCCCAAGTGCAGCGAG',
                  pos6name ='CD55-4',
                  pos6spacer = 'ACTGGTATTGCGGAGCCACGAGG',
                  Adaptor5p = 'ATTTTGCCCCTGGTTCTT',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG'),
            list( 'oligosequence' = "ATTTTGCCCCTGGTTCTTCGTCTCAAGATGGCGCGACCCCCAGGAAGGTCTCAATTTCTACTGTCGTAGATATATAAGTGGAGGCGTCGCGCTGAATTCCTACTATTGTAGGTAGGAATGCCCGCCAGCGCGACGCAATTCCTACTCTAGTAGGTTCTGCGTTCTGCTCCTACTGCTTAATTCCTACTCTCGTAGGTAGCTCTCGCCCAAGTGCAGCGAGAATTTCTACTCTAGTAGATACTGGTATTGCGGAGCCACGAGGAATTCGAGACGCCAGTTCATTTCTTAGGG", 'oligoname' = "CD81-1_B2M-1_B2M-3_KIT-2_KIT-3_CD55-4")
      )
      # test for spacers starting with TTT
      expect_error(
            getLibraryOligoCas12a6plexgRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'TTTGCGACCCCCAGGAAGGTCTC',
                  pos2name = 'B2M-1',
                  pos2spacer = 'ATATAAGTGGAGGCGTCGCGCTG',
                  pos3name = 'B2M-3',
                  pos3spacer = 'AGGAATGCCCGCCAGCGCGACGC',
                  pos4name = 'KIT-2',
                  pos4spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                  pos5name = 'KIT-3',
                  pos5spacer = 'AGCTCTCGCCCAAGTGCAGCGAG',
                  pos6name ='CD55-4',
                  pos6spacer = 'ACTGGTATTGCGGAGCCACGAGG',
                  Adaptor5p = 'ATTTTGCCCCTGGTTCTT',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG')
      )
      # test for spacer containing TTTT
      expect_error(
            getLibraryOligoCas12a6plexgRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'GGCGCGACCCCCAGGAAGGTCTC',
                  pos2name = 'B2M-1',
                  pos2spacer = 'ATATAAGTGTTTTCGTCGCGCTG',
                  pos3name = 'B2M-3',
                  pos3spacer = 'AGGAATGCCCGCCAGCGCGACGC',
                  pos4name = 'KIT-2',
                  pos4spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                  pos5name = 'KIT-3',
                  pos5spacer = 'AGCTCTCGCCCAAGTGCAGCGAG',
                  pos6name ='CD55-4',
                  pos6spacer = 'ACTGGTATTGCGGAGCCACGAGG',
                  Adaptor5p = 'ATTTTGCCCCTGGTTCTT',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG'
            )
      )
      #check for problematic inclusion of BsmbI site in adaptor
      expect_error(
            getLibraryOligoCas12a6plexgRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'GGCGCGACCCCCAGGAAGGTCTC',
                  pos2name = 'B2M-1',
                  pos2spacer = 'ATATAAGTGGAGGCGTCGCGCTG',
                  pos3name = 'B2M-3',
                  pos3spacer = 'AGGAATGCCCGCCAGCGCGACGC',
                  pos4name = 'KIT-2',
                  pos4spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                  pos5name = 'KIT-3',
                  pos5spacer = 'AGCTCTCGCCCAAGTGCAGCGAG',
                  pos6name ='CD55-4',
                  pos6spacer = 'ACTGGTATTGCGGAGCCACGAGG',
                  Adaptor5p = 'ATTTTGCCCCTGcgtctc',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG'
            )
      )
      #check for warning for unusual adaptor length
      expect_warning(
            getLibraryOligoCas12a6plexgRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'GGCGCGACCCCCAGGAAGGTCTC',
                  pos2name = 'B2M-1',
                  pos2spacer = 'ATATAAGTGGAGGCGTCGCGCTG',
                  pos3name = 'B2M-3',
                  pos3spacer = 'AGGAATGCCCGCCAGCGCGACGC',
                  pos4name = 'KIT-2',
                  pos4spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                  pos5name = 'KIT-3',
                  pos5spacer = 'AGCTCTCGCCCAAGTGCAGCGAG',
                  pos6name ='CD55-4',
                  pos6spacer = 'ACTGGTATTGCGGAGCCACGAGG',
                  Adaptor5p = 'ATTTTGCCCCTGTGAGCCTGGG',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG'
            ),
            "adaptors are not 18nt long, this is unexpected"
      )
      # check for non-DNA characters
      expect_error(
            getLibraryOligoCas12a6plexgRNA(
                  pos1name = 'CD81-1',
                  pos1spacer = 'GGCGCGACCCCCxyzAAGGTCTC',
                  pos2name = 'B2M-1',
                  pos2spacer = 'ATATAAGTGGAGGCGTCGCGCTG',
                  pos3name = 'B2M-3',
                  pos3spacer = 'AGGAATGCCCGCCAGCGCGACGC',
                  pos4name = 'KIT-2',
                  pos4spacer = 'TCTGCGTTCTGCTCCTACTGCTT',
                  pos5name = 'KIT-3',
                  pos5spacer = 'AGCTCTCGCCCAAGTGCAGCGAG',
                  pos6name ='CD55-4',
                  pos6spacer = 'ACTGGTATTGCGGAGCCACGAGG',
                  Adaptor5p = 'ATTTTGCCCCTGGTTCTT',
                  Adaptor3p = 'CCAGTTCATTTCTTAGGG'
            )
      )
})

