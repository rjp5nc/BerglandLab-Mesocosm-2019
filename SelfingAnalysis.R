# ijob -A berglandlab_standard -c20 -p standard --mem=40G
### module load gcc/7.1.0  openmpi/3.1.4 R/4.1.1; R

### library

  library(data.table)
  library(SeqArray)
  library(foreach)
  library(doMC)
  registerDoMC(20)

### load in data

  snp.dt <- fread("/project/berglandlab/Robert/DorsetPooledSequencing2018_2019/AllPooledVCFsIncludingDcatDbunk/DorsetpoolsALL_snpdt.csv")
  snpStats <- fread("/project/berglandlab/Robert/DorsetPooledSequencing2018_2019/AllPooledVCFsIncludingDcatDbunk/DorsetpoolsALL_snpStats.csv")
  samps <- fread("/project/berglandlab/Robert/DorsetPooledSequencing2018_2019/AllPooledVCFsIncludingDcatDbunk/DorsetpoolsALL_metadata.csv")
  fivekbchrassignHiCnew <- fread("/project/berglandlab/Robert/DorsetPooledSequencing2018_2019/5kbchrassignHiCnew.csv")

### genofile definition
  genofile.fn <- "/project/berglandlab/Robert/DorsetPooledSequencing2018_2019/AllPooledVCFsIncludingDcatDbunk/Allpools.concat.Removereps.gds.gz"
  genofile <- seqOpen(genofile.fn)
  ##seqResetFilter(genofile)