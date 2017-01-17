[![DOI](https://zenodo.org/badge/69814279.svg)](https://zenodo.org/badge/latestdoi/69814279)

# ELA-N-Deposition

Data and code to accompany the manuscript Venkiteswaran, Schiff, Paterson, Flinn, Shao, Elgood. 2017. *Increasing Nitrogen Deposition with low δ<sup>15</sup>N-NH<sub>4</sub><sup>+</sup> and δ<sup>15</sup>N-NO<sub>3</sub><sup>-</sup> Values at the Experimental Lakes Area, northwestern Ontario, Canada*. FACETS doi: 10.1139/facets-2016-0060.

Manuscript submitted October 2016.

The raw data and R-scripts used to perform the analyses and figures are here.

* 01-data_calculations_and_figures.R: R-script to reproduce the N deposition figures and calculations
* 02-isotope_figures.R: R-script to reproduce the isotope figures
* 03-cows_pigs_fertilizer_figure: R-script for the agriculture (cows, pigs, and fertilizer) figure
* cows fertilizer pigs.csv: agriculture (cows, pigs, and fertilizer) data
* ELA N precip data for calculations.csv: raw precipitation chemistry and depths
* lake stream survey.csv: dissolved organic matter, particulate organic matter, and zooplankton isotope data
* precip amount conc delta.csv: precipitation data with concentrations and isotopes
* precip delta NH4.csv: survey of NH4+ isotope data
* precip delta NO3.csv: survey of NO3- isotope data

The R-code for the analyses, figures, and mixing models requires (at the very least) dply, ggplot2, Kendall, reshape2, and zoo. Thanks to the authors of these packages.
