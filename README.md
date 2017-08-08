# DEploid-ShinyApp

[![License (GPL version 3)](https://img.shields.io/badge/license-GPL%20version%203-brightgreen.svg)](http://opensource.org/licenses/GPL-3.0)

DEploid-ShinyApp is an interactive web interface for [DEploid](https://github.com/mcveanlab/DEploid) (also available in [R](cran.r-project.org/package=DEploid)). DEploid is designed for deconvoluting mixed genomes with unknown proportions. DEploid-ShinyApp provides a user friendly interface to interact the deconvoluted haplotypes with specific population groups, and look up for existing drug resistance mutations and etc.

![logo](https://github.com/shajoezhu/DEploid-ShinyApp/raw/master/www/screenShot.png "Screenshots")

Installation
------------

Make sure you have the following `R` packages installed.

```R
install.packages(c("shinythemes", "shinyjs", "shiny", "dplyr", "quantmod",
    "RCurl", "plotly", "ggplot2", "dygraphs", "DEploid", "leaflet", "stringr"))
```

Download `DEploid-ShinyApp` from the git repository,

```bash
$ git clone git@github.com:shajoezhu/DEploid-ShinyApp.git
$ cd DEploid-ShinyApp
```

launch `R` under the `DEploid-ShinyApp` directory and type the following:

```R
shiny::runApp()
```

VCF file requirement
--------------------
DEploid-ShinyApp require user to provide a VCF file. Assume all variants are `PASS` in the `QUAL` column, the VCF file also reqires the `AD` field. In the current implementation, `DEploid-ShinyApp` only take the first sample in the VCF file. `DEploid-ShinyApp` DO NOT handle multi-allelic variants, nor indels. The `FILTER` column will not be used.

Licence
-------

You can freely use all code in this project under the conditions of the GNU GPL Version 3 or later.


Citation
--------

If you use `dEploid` in your work, please cite the program:

Zhu, J. S. J. Almagro-Garcia G. McVean. (2017) Deconvoluting multiple infections in *Plasmodium falciparum* from high throughput sequencing data. *bioRxiv* 099499. doi: https://doi.org/10.1101/099499
