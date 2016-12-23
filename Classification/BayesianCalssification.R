#http://www.r-tutor.com/gpu-computing/gaussian-process/rvbm
#https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4930525/
packages_required = c('vbmp')

for(pkg in packages_required){
  if(!(pkg %in% rownames(installed.packages()))){
    install.packages(pkg)
  }
}
library(vbmp)
