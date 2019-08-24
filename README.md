# HBV.IANIGLA

This package contains the HBV hydrological  model. I have decoupled it to allow the user to build his/her own model. I developed it as part of my PhD thesis in [IANIGLA-CONICET](https://www.mendoza.conicet.gov.ar/portal/ianigla/)   (Instituto Argentino de Nivología Glaciología y Ciencias Ambientales - Consejo Nacional de Investigaciones Científicas y Técnicas) for hydroclimatic studies in the Andes. **HBV.IANIGLA** incorporates modules for precipitation and 
  temperature interpolation,  and also for clean and debris covered ice melt estimations. 

## Installation instructions
### Dependencies
**HBV.IANIGLA** just depends on: 
- Rcpp (>= 0.12.0)

To install the dependencies, you can use the menu command **Packages | Install** in Rstudio, or the command install.packages as in

```
install.packages("Rcpp")

```
### Installing HBV.IANIGLA

You can download the complete package  from CRAN repository as usual: 
```
install.packages("HBV.IANIGLA")

```
