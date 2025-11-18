# Transmission Estimator  
[![License GPL v3](https://img.shields.io/badge/license-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17593948.svg)](https://doi.org/10.5281/zenodo.17593948)


This shiny app is designed to identify transmission clusters among neonatal sepsis bacterial isolates using genomic (genetic distance) and epidemiological (spatiotemporal) data.

<br>   

## Usage instructions  

  
### Web app
Transmission Estimator is available as a web app at [klebsiella.shinyapps.io/transmission_estimator](https://klebsiella.shinyapps.io/transmission_estimator)  
  
<br> 


  
### Running locally
You may also run this tool locally on your computer. Follow these steps to clone the repository, set up the required environment, and run the Shiny app.

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


<br>  
 

### Input data  
Two input files in TSV or CSV format are required.  

- **Metadata file**  
Metadata file containing epidemiological and other information on the isolates. See [sample metadata](https://github.com/klebgenomics/transmission_estimator/blob/main/data/demo_data/BARNARDS/BARNARDS_metadata.csv).
Required columns include: 
    - `id`: Sample ID column. Values must match IDs in distance matrix file  
    - `Year`: YYYY format, e.g., 2019  
    - `Month`: MM format, e.g., 10  
    - `Day`: DD format, e.g., 25  
    - `Site`: Site/location of isolation. Used by default as the spatial clustering variable    
    - `Country`: Country of isolation  
    - `ST`: Sequence type of the strain. This column is not used for clustering but is only used to group clusters in the plot. 

- **Genetic distance matrix file**  
Pairwise genetic distance matrix. See [sample matrix](https://github.com/klebgenomics/transmission_estimator/blob/main/data/demo_data/BARNARDS/BARNARDS_metadata.csv). This can be generated using [Pathogenwatch](https://pathogen.watch/).  
This file must be in square matrix format with the first column labelled 'Name'.  

<br>  

### Clustering options 
Clusters are identified using single linkage clustering based on the clustering options / thresholds specified in the `Clusters` tab.  

Clusters comprise isolates with sufficient genetic similarity (as specified with the genetic distance threshold) that were isolated within a given time frame (as specified with the temporal distance threshold) and in the same location (as specified with the spatial clustering variable, default: `Site`). Users may provide alternative columns like `Ward`, `Unit`, `Hospital`, etc. in the metadata file for use as the spatial clustering variable (adjustable in the `Clusters` tab).

The proportion of cases attributable to transmission is calculated by excluding a hypothetical index case per cluster.

Use the `Sensitivity` tab to explore the sensitivity of the transmission estimates to the choice of temporal and genetic distance thresholds.  
  
<br>   


## Citation  
If you use this tool, please cite:  
1. This repository using this DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.17593948.svg)](https://doi.org/10.5281/zenodo.17593948)
2. The preprint: "Contribution of nosocomial transmission to *Klebsiella pneumoniae* neonatal sepsis in Africa and South Asia: a meta-analysis of infection clusters inferred from pathogen genomics and temporal data." Odih et al, 2025, _MedRxiv_. DOI: [10.1101/2025.11.15.25340095v1](https://www.medrxiv.org/content/10.1101/2025.11.15.25340095v1))
