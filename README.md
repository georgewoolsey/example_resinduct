# resinduct
# R code for axial resin duct quantification in tree rings

This repository includes the R code for multiple steps of quantification of axial resin ducts in tree rings as well as examples from three tree cores (TXT and RWL files) with which the script can be run. The script will take TXT files built from the measurement of resin ducts in ImageJ and process them so that the final output will be a CSV file containing 5 resin duct metrics corresponding to calendar years in which the ducts were formed. For further information about the project and for more detailed information on processing tree cores for resin duct analyses, see Hood et al., 2020 (https://doi.org/10.1016/j.mex.2020.101035).

## Getting Started
Copy script from resinductscript.R into new R script. If using the example duct measurement (TXT) and ring width (RWL) files, save these as .txt and .rwl files, respectively, to your working directory. Ensure that file names for TXT and RWL files are as listed in the repository (e.g. SNF117La.rwl).

### Prerequisites
R statistical software (https://www.r-project.org/)
R packages (also listed within script):
data.table 
Matt Dowle and Arun Srinivasan (2019). data.table: Extension of `data.frame`. R package version 1.12.8. https://CRAN.R-project.org/package=data.table

dplR 
Bunn AG (2008). “A dendrochronology program library in R (dplR).” _Dendrochronologia_, *26*(2), 115-124. ISSN 1125-7865, doi: 10.1016/j.dendro.2008.01.002 (URL: https://doi.org/10.1016/j.dendro.2008.01.002).

tidyr
Hadley Wickham and Lionel Henry (2020). tidyr: Tidy Messy Data. R package version 1.1.0.  https://CRAN.R-project.org/package=tidyr

##################################################################################

## Built With
R version 4.0.0

## Authors
Sharon M. Hood, Charlotte C. Reed, Jeffrey M. Kane

## Acknowledgements
We thank Lisa Holsinger for assistance with brainstorming how to add calendar years to resin duct measurements and writing preliminary code.


