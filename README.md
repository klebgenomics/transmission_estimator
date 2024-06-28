# Transmission Estimator  
[![License GPL v3](https://img.shields.io/badge/license-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html) [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12582875.svg)](https://doi.org/10.5281/zenodo.12582875)


This shiny app is designed to identify transmission clusters among neonatal sepsis bacterial isolates using genomic (genetic distance) and epidemiological (spatiotemporal) data.

<br>   

## Usage instructions  

### Input data  
Two input files in TSV or CSV format are required.  

- **Metadata file**  
Metadata file containing epidemiological and other information on the isolates.
Required columns include: `id`, `Year`, `Month`, `Day`, `Country`, `Site`, and `ST`  

- **Distance matrix file**  
Pairwise genetic distance matrix. This can be generated using [Pathogenwatch](https://pathogen.watch/).  
This file must be in square matrix format with the first column labelled 'Name'.  


### Clustering options 
Clusters are identified based on the clustering options / thresholds specified in the 'Clusters' tab.  

Clusters comprise isolates with sufficient genetic similarity (as specified with the genetic distance threshold) that were isolated within a given time frame (as specified with the temporal distance threshold) and in the same location (as specified with the spatial clustering variable).  

The proportion of cases attributable to transmission is calculated by excluding a hypothetical index case per cluster.

Use the 'Sensitivity' tab to explore the sensitivity of the transmission estimates to the choice of temporal and genetic distance thresholds.  
  
<br>   


## Citation  
This tool is still under development. If you use this tool, please cite this repository using this DOI: [![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.12582875.svg)](https://doi.org/10.5281/zenodo.12582875)
