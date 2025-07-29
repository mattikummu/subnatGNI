# global subnational dataset for gross domestic income (GNI) per capita at purchasing power parity (PPP) over 1990-2023

These codes were used to create the subnational dataset for GNI per capita that is part of the following preprint. Please do cite this paper if using the codes. 

Chrisendo D, Niva V, Hoffman R, Sayyar SM, Rocha J, Sandström V, Solt F, Kummu M. 2024. Income inequality has increased for over two-thirds of the global population. Preprint. doi: https://doi.org/10.21203/rs.3.rs-5548291/v1

**SOFTWARE**
We used R (version 4.3.2) to develop and test the code. 


**CODE SCRIPTS**

The code is numbered with the order it should be run. Below each code is briefly explained. 

**0_install_packages.R**: install the needed packages

**1_gni_prepare_adm0.R**: puts together admin 0 level (national) data. Interpolates and extrapolates the missing values.

**2_gni_prepare_adm1.R**: puts together admin 1 level (sbunational) data, interpolates and extrapolates the missing values

**3_gni_prepare_spatial.R**: combines the admin 0 and admin 1 level to a global grid and gpkg file

**4_gni_adm0_data2raster_polyg.R**: puts data to raster and polygon files

**5_storeFinalFiles.R**: store final files

**6_gni_plot_maps.R**: script to plot maps shown in the manuscript

**functions**: the functions used in the scripts above are in this folder


**RELATED CODE**
The other two parts of the code for the above mentioned paper are given in:
https://github.com/mattikummu/subnatGini  
https://github.com/mattikummu/gini_gni_analyses 


**REPRODUCTION**
The input data needed to run the code is available in the repository: http://doi.org/10.5281/zenodo.14056855. Please extract the zip-file (input_data_GNI.zip) under the same folder with the code. To run all the scripts, will take on average one hours to complete. 

The input data for the Gini coefficient (code in https://github.com/mattikummu/subnatGini) are also in the zenodo repository, and instructions to run the code are given in the github page. The script for the analyses (https://github.com/mattikummu/gini_gni_analyses) can be run after Gini and GNI codes are completed. 

The final output data files, i.e. the expected outcome from the code, are within the same repository than the input data. 



For more information, please contact Matti Kummu (matti.kummu@aalto.fi)