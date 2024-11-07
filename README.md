# Transmission Estimator  
[![License GPL v3](https://img.shields.io/badge/license-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12582875.svg)](https://doi.org/10.5281/zenodo.12582875)


This shiny app is designed to identify transmission clusters among neonatal sepsis bacterial isolates using genomic (genetic distance) and epidemiological (spatiotemporal) data.

<br>   

## Usage instructions  
  
### Running locally
Follow these steps to clone the repository, set up the required environment, and run the Shiny app.

#### 1. Clone the Repository

- Install and open [RStudio](https://posit.co/download/rstudio-desktop/) on your machine
- Go to File > New Project > Version Control > Git
- Paste the repository URL (https://github.com/klebgenomics/transmission_estimator.git)
- Choose a location to save the project, and click Create Project  

#### 2. Install Required Packages Using `renv`
This project uses the [renv](https://rstudio.github.io/renv/) package to manage package dependenciesand ensure that correct versions of packages are installed.  

To install `renv` if it's not already installed, run:
```
install.packages("renv")
```
Then, use `renv` to install the packages specified in the `renv.lock` file:
```
renv::restore()
```  
  
#### 3. Run the Shiny App
Once the dependencies are installed, you can run the app using the following command in the RStudio console:
```
shiny::runApp()
```  
  
Alternatively, you can open the `app.R` file in the root of the repository, and simply click **Run App** at the top right of the script window.  

> Note: Adjust the view to fit your screen by pressing `Cmd` + `-/+` on Mac or `Ctrl` + `-/+` on Windows.  

  
  
### Web app
Transmission Estimator is also available as a web app at [klebsiella.shinyapps.io/transmission_estimator](https://klebsiella.shinyapps.io/transmission_estimator)  
  
  
### Input data  
Two input files in TSV or CSV format are required.  

- **Metadata file**  
Metadata file containing epidemiological and other information on the isolates.
Required columns include: `id`, `Year`, `Month`, `Day`, `Country`, `Site`, and `ST`  

- **Distance matrix file**  
Pairwise genetic distance matrix. This can be generated using [Pathogenwatch](https://pathogen.watch/).  
This file must be in square matrix format with the first column labelled 'Name'.  


### Clustering options 
Clusters are identified based on the clustering options / thresholds specified in the `Clusters` tab.  

Clusters comprise isolates with sufficient genetic similarity (as specified with the genetic distance threshold) that were isolated within a given time frame (as specified with the temporal distance threshold) and in the same location (as specified with the spatial clustering variable).  

The proportion of cases attributable to transmission is calculated by excluding a hypothetical index case per cluster.

Use the `Sensitivity` tab to explore the sensitivity of the transmission estimates to the choice of temporal and genetic distance thresholds.  
  
<br>   


## Citation  
This tool is still under development. If you use this tool, please cite this repository using this DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12582875.svg)](https://doi.org/10.5281/zenodo.12582875)
