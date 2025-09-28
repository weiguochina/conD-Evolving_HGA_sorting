# Supporting Documents for "The Effects of Coastal Defense Structures on Residential Sorting and Welfare under Climate Change"
This package provides the replication code for the main results of the economic sorting model in the paper.
The folder ./program contains all code files. Running the programs requires access to proprietary **CoreLogic home sales data**.

## Replication Code

All replication code is located in the ./program folder.

### Prerequisite

The analysis was run on a server with 16 cores, 32 threads, a 3.10GHz CPU, and 256 GB memory.

Software requirements:

1. R version 4.3.1 or later
2. The following R packages (with tested versions):
leafem 0.2.0, colorspace 2.0-3, deldir 1.0-6, class 7.3-20, leaflet 2.2.0, satellite 1.0.4, base64enc 0.1-3, fs 1.5.2, rstudioapi 0.14, proxy 0.4-27, roxygen2 7.2.1, listenv 0.8.0, hexbin 1.28.2, remotes 2.4.2, fansi 1.0.3, xml2 1.3.3, codetools 0.2-18, doParallel 1.0.17, cachem 1.0.6, knitr 1.40, pkgload 1.3.0, jsonlite 1.8.2, png 0.1-7, shiny 1.7.2, BiocManager 1.30.22, compiler 4.2.1, assertthat 0.2.1, fastmap 1.1.0, cli 3.6.1, later 1.3.0, htmltools 0.5.3, prettyunits 1.1.1, tools 4.2.1, glue 1.6.2, vctrs 0.6.1, iterators 1.0.14, crosstalk 1.2.0, lwgeom 0.2-8, xfun 0.33, stringr 1.4.1, globals 0.16.1, ps 1.7.1, mime 0.12, miniUI 0.1.1.1, lifecycle 1.0.3, devtools 2.4.5, terra 1.5-34, zoo 1.8-11, hms 1.1.3, promises 1.2.0.1, parallel 4.2.1, RColorBrewer 1.1-3, yaml 2.3.5, memoise 2.0.1, latticeExtra 0.6-30, stringi 1.7.8, e1071 1.7-11, pkgbuild 1.3.1, rlang 1.1.0, pkgconfig 2.0.3, evaluate 0.17, purrr 0.3.5, htmlwidgets 1.5.4, processx 3.7.0, tidyselect 1.2.0, parallelly 1.32.1, magrittr 2.0.3, R6 2.5.1, generics 0.1.3, profvis 0.3.7, DBI 1.1.3, pillar 1.8.1, units 0.8-0, tibble 3.1.8, crayon 1.5.2, interp 1.1-3, KernSmooth 2.23-20, utf8 1.2.2, rmarkdown 2.17, urlchecker 1.0.1, jpeg 0.1-10, progress 1.2.2, usethis 2.1.6, grid 4.2.1, callr 3.7.2, digest 0.6.31, classInt 0.4-7, webshot 0.5.4, xtable 1.8-4, httpuv 1.6.6, stats4 4.2.1, munsell 0.5.0, viridisLite 0.4.1, sessioninfo 1.2.2.

### Data Pre-processing

Because CoreLogic data are proprietary, we cannot share them. Note that the raw data may contain inaccurate property coordinates. We therefore conduct a geocoding process to improve spatial accuracy.

After geocoding, save the output as property_sales_2010_2022.dta before proceeding to the analysis.

### Performing Sorting Model

Run the following programs in sequence to replicate the results:

1. 1_data_process.R – Initializes the property transaction data. Generates: final_sample_2010_2022.fst (neighborhood matrix) and choice_tract_year_2010_2022.fst (choice matrix)
2. 2_first_stage_distbin.R – Performs the first-stage analysis, estimating heterogeneity parameters and mean utilities. Calls the function first_stage_foreach_year, which optimizes a conditional logit model via the maxLik package.
3. 3_generate_iv_year.R – Generates IVs for prices for use in the second stage. Produces IV_year.csv.
4. 4_summ_1_2_stages.R – Produces descriptive statistics, tables, and figures.
5. 5_hedonic.R – Runs the hedonic analysis.
6. 6_asmt_merge_floodplain_flooded_submerged_combined.R – Merges SFHA and projected floodplain spatial data with assessment data.
7. 7_agg_asmt_floodplain.R – Aggregates welfare outcomes by climate scenario.
8. 8_plot_WTP.R – Produces figures summarizing welfare change outcomes.

## Contact

Please contact Wei Guo (wei.guo1@ucr.edu) for any questions regarding the code or data files.

Please see the paper for more information on the codes and data. If you use these codes files or data, please **CITE** this paper as the source.

