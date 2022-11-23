HBV.IANIGLA
================

<!-- badges: start -->

[![CRAN
release](https://www.r-pkg.org/badges/version/HBV.IANIGLA?color=orange)](https://cran.r-project.org/package=HBV.IANIGLA)
[![monthly_download](http://cranlogs.r-pkg.org/badges/last-month/HBV.IANIGLA?color=green)](https://cran.r-project.org/package=HBV.IANIGLA)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
<!-- badges: end -->

# Publications

Tashie, A., Pavelsky, T., & Kumar, M. (2022). A Calibration-Free
Groundwater Module for Improving Predictions of Low Flows. Water
Resources Research, 58(3), e2021WR030800.
<https://doi.org/10.1029/2021WR030800>

Toum, E., Masiokas, M. H., Villalba, R., Pitte, P., & Ruiz, L. (2021).
The HBV.IANIGLA Hydrological Model. The R Journal, 13(1), 378-395.
<https://doi.org/10.32614/RJ-2021-059>

Astagneau, P. C., Thirel, G., Delaigue, O., Guillaume, J. H. A.,
Parajka, J., Brauer, C. C., Viglione, A., Buytaert, W., & Beven, K. J.
(2021). Technical note: Hydrology modelling R packages – a unified
analysis of models and practicalities from a user perspective. Hydrology
and Earth System Sciences, 25(7), 3937-3973.
<https://doi.org/10.5194/hess-25-3937-2021>

# Overview

This package contains the HBV hydrological model but in modules,
allowing the user to build his/her own HBV model. I developed it as part
of my PhD thesis at **IANIGLA-CONICET** (Instituto Argentino de
Nivología Glaciología y Ciencias Ambientales - Consejo Nacional de
Investigaciones Científicas y Técnicas) for hydroclimatic studies in the
Andes. **HBV.IANIGLA** incorporates modules for clean and debris covered
glacier melt estimations.

## Installation instructions

### Dependencies

**HBV.IANIGLA** just depends on: - Rcpp (\>= 0.12.0)

To install the dependencies, you can use the menu command **Packages \|
Install** in Rstudio, or just type:

    install.packages("Rcpp")

### Installing HBV.IANIGLA

You can download the complete package from CRAN repository as usual:

    install.packages("HBV.IANIGLA")

## Examples

I strongly recommend to read the **HBV.IANIGLA** vignettes
(`vignette(package = "HBV.IANIGLA")`). This documents will help you to
understand how the model works.
