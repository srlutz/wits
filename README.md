# Water Isotope modelling for Transit time and Storage (WITS)

### This is the project for the Storage and transit time estimation in hydrological systems with the Water Isotope modelling for Transit time and Storage (WITS) software. It has been produced within the COST ACTION WATSON.

Please download the R programming language (https://cran.rstudio.com/). We recommend to use Rstudio which can be downloaded here: https://posit.co/download/rstudio-desktop/). 

#### Running WITS
First, browse to the respective folder, where you installed the toolbox. Go to ´\src…´ and load and execute the main script in RStudio: ´main_script_0_7.R´. 
The folder setup of the example must be kept as is, simply changing the local host name to the main folder 

folder<-c("C:/exampleuser/exampleuser/exampleWITS/exampleWITSversion/") <-- put here your local host name.

To run the script, click on "Source." Pop-up windows will appear, allowing you to customize your preferences for running the script. Once you've selected your options, click "OK" to proceed with the next steps. 
Please continue selecting your options in the following windows, adjust them according to your needs. Follow the prompts until the process is complete.

The results of successful model executions are stored in ´\results…´. Each results folder will contain multiple runs. A total of up to 10 runs can be performed. In addition to the individual run folders, a summary text file will be generated, providing a summary of the results. The output figures are also stored in the results folder.

The toolbox provides a basic graphical output for ad-hoc visual evaluation of the model results, but the users are encouraged to create their own graphical output once satisfied with the results. The raw data of the simulated output is stored in ´.txt-file format´.   

See folder ./docs for more detailed instructions in the beginner's guide. 

Cite the code: [![DOI](https://zenodo.org/badge/876698880.svg)](https://doi.org/10.5281/zenodo.14036931)
