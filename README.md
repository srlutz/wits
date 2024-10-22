# WATSON model comparison project

### This is the project for the model comparison between transit time distributions (TTDs) calculated by lumped parameter models (LPMs) and storAge slection (SAS) functions. 

## Introduction
This project allows computing and comparing TTDs using LPMs and SAS functions. It provides different transfer functions for the LPMs and different shapes for the SAS functions. TTDs can be computed for example data from ... 
*some more text*

## Requirements
TO DO: Instructions for requirement
--> R language / R studio
This project has been build under *update*
R version 4.2.2 (2022-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64 (64-bit)
Running under: Windows 10 x64 (build 19042)

## Download & installation
```
git clone git@github.com:srlutz/watson.git
```
- download .zip file / unpack .zip file / click on watson.Rproj
TO DO: Instructions for installation
### Required r packages
dowload and load rpackage "renv" [link], then run 
```
renv::init() 
````
to install all required packages in their required versions. 

## Project organization
- PG = project-generated
- HW = human-writable
- RO = read only
```
.
├── .gitignore
├── CITATION.md
├── LICENSE.md
├── README.md
├── tutorial.md
├── requirements.txt
├── bin                <- Compiled and external code, ignored by git (PG)
│   └── external       <- Any external source code, ignored by git (RO)
├── config             <- Configuration files (HW)
├── data               <- All project data, ignored by git
│   ├── processed      <- The final, canonical data sets for modeling. (PG)
│   ├── raw            <- The original, immutable data dump. (RO)
│   └── temp           <- Intermediate data that has been transformed. (PG)
├── docs               <- Documentation notebook for users (HW)
│   ├── manuscript     <- Manuscript source, e.g., LaTeX, Markdown, etc. (HW)
│   └── reports        <- Other project reports and notebooks (e.g. Jupyter, .Rmd) (HW)
├── results
│   ├── figures        <- Figures for the manuscript or reports (PG)
│   └── output         <- Other output for the manuscript or reports (PG)
└── src                <- Source code for this project (HW)

```
## Demo
TO DO --> create tutorial.md and link to it ("Features" might go there)

## Features
### 1. Lumped parameter models (LPMs)
*some text*

#### 1.1 Exponential model
*some text*

#### 1.2 Model 2
*some text*

#### 1.3 Model 3
*some text*

### 2. StorAge selection (SAS) functions
*some text*

#### 2.1 Beta function
*some text*

#### 2.2 Function 2
*some text*

#### 2.3 Function 3
*some text*

### 3. Young water fractions

### 4. Data

## Troubleshooting
- known bugs
- orphaned R packages? 

## License

This project is licensed under the terms of the [MIT License](/LICENSE.md)

## Citation
who are contributors.... Acknowledgements...

## Bibliography
1. Kirchner, J. W.: Aggregation in environmental systems – Part 1: Seasonal tracer cycles quantify young water fractions, but not mean transit times, in spatially heterogeneous catchments, Hydrol. Earth Syst. Sci., 20, 279–297, https://doi.org/10.5194/hess-20-279-2016, 2016.
2. Kirchner, J. W.: Aggregation in environmental systems – Part 2: Catchment mean transit times and young water fractions under hydrologic nonstationarity, Hydrol. Earth Syst. Sci., 20, 299–328, https://doi.org/10.5194/hess-20-299-2016, 2016.
