This document contains replication information for "Complementarity in Alliance: How strategic compatibility and hierarchy promote efficient cooperation in international security"

Author: J Andres Gannon (juan.gannon@vanderbilt.edu)

**Files**
- 03_df-full.rds - R data file which must be read in to run the code in the accompanying qmd script

- atop5_1m.csv - data from ATOP that includes information about each state’s alliance commitments in a given year. See http://www.atopdata.org/

- atop-sscore.tab - data from ATOP that includes s-scores for dyads based on ATOP data. See Chiba et al (2015)

- DoDL_codebook.docx - codebook describing the variable names and attributes in the primary data file

- DoDL_replication.qmd - replication code written in R that will reproduce the figures and tables in the main text. Each output is noted under a different header

- dyad_threat.rds - dyad-year dataframe of each country's threat environment used to generate the strategic compatibility variable

- rDMC_long_v1.rds - long version of the rDMC dataset used to generate the dependent variable. See militarycapabilities.com for more information

- rDMC_raw_v1.rds - raw version of the rDMC dataset used to generate the dependent variable. See militarycapabilities.com for more information

- rDMC_wide_v1.rds - wide version of the rDMC dataset used to generate the dependent variable. See militarycapabilities.com for more information

- sipri_dyad-year.rds - data from SIPRI about annual dyadic arms sales. See https://www.sipri.org/databases/armstransfers

**Replication instructions**
Run the code in DoDL_replication.qmd to reproduce all tables and figures in the manuscript. Each table/figure is noted in a separate code chunk.
Run the code in DoDL_replication-appendix.qmd to reproduce all tables and figures in the appendix. Each table/figure is noted in a separate code chunk.

Please note that more complete replication material, including all the code used to pre-process, clean, and merge the data, is available at https://github.com/jandresgannon/DoDL.

Data source citation:

Chiba, Daina, Jesse C. Johnson, and Brett Ashley Leeds. 2015. "Careful Commitments: Democratic States and Alliance Design." Journal of Politics 77 (4): 968-982.

Leeds, Brett Ashley, Jeffrey Ritter, Sara Mitchell, and Andrew Long. 2002. “Alliance Treaty Obligations and Provisions, 1815-1944.” International Interactions 28 (3): 237–60. https://doi.org/10.1080/03050620213653.

Gannon, J Andrés. 2023. “Planes, Trains, and Armored Mobiles: Introducing a Dataset of the Global Distribution of Military Capabilities.” International Studies Quarterly 67 (4): 1–12. https://doi.org/10.1093/isq/sqad081.

Computing environment used:

sysname "Linux"
release "6.11.0-18-generic"
version "#18-Ubuntu SMP PREEMPT_DYNAMIC Fri Feb 7 22:34:25 UTC 2025"
Platform: x86_64-pc-linux-gnu
Running under: Ubuntu 24.10

Statistical software used:
RStudio 2024.12.1+563 "Kousa Dogwood" Release (27771613951643d8987af2b2fb0c752081a3a853, 2025-02-02) for Ubuntu Jammy
Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) rstudio/2024.12.1+563 Chrome/126.0.6478.234 Electron/31.7.7 Safari/537.36, Quarto 1.6.42 (/opt/quarto/bin/quarto)

Duration of time to process: 13.4 seconds

Required packages:

attached base packages:
[1] stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] tidygraph_1.3.1     ggraph_2.2.1        igraph_2.0.2        rstan_2.32.6        StanHeaders_2.32.10 magrittr_2.0.3      ggtext_0.1.2       
 [8] ggplot2_3.5.0       kableExtra_1.4.0    knitr_1.45         

loaded via a namespace (and not attached):
  [1] countrycode_1.6.0    splines_4.3.1        tinytable_0.7.0      tibble_3.2.1         R.oo_1.27.0          polyclip_1.10-6     
  [7] brms_2.22.0          datawizard_0.9.1     lifecycle_1.0.4      sf_1.0-19            rprojroot_2.0.3      globals_0.16.2      
 [13] processx_3.8.3       lattice_0.21-8       MASS_7.3-60          insight_1.0.2        backports_1.4.1      rmarkdown_2.25      
 [19] modelsummary_1.4.5   pkgbuild_1.4.3       DBI_1.2.3            minqa_1.2.6          abind_1.4-5          pkgload_1.3.4       
 [25] purrr_1.0.2          R.utils_2.12.3       tensorA_0.36.2.1     tweenr_2.0.3         sandwich_3.1-1       dreamerr_1.4.0      
 [31] ggrepel_0.9.5        inline_0.3.21        listenv_0.9.1        units_0.8-5          performance_0.10.9   bridgesampling_1.1-2
 [37] ordbetareg_0.7.2     parallelly_1.36.0    svglite_2.1.3        codetools_0.2-19     xml2_1.3.6           ggforce_0.4.2       
 [43] tidyselect_1.2.0     bayesplot_1.11.1     farver_2.1.1         lme4_1.1-35.1        viridis_0.6.5        matrixStats_1.2.0   
 [49] stats4_4.3.1         jsonlite_1.8.7       centiserve_1.0.0     e1071_1.7-14         Formula_1.2-5        emmeans_1.10.0      
 [55] systemfonts_1.2.1    tictoc_1.2.1         tools_4.3.1          progress_1.2.3       rio_1.2.3            Rcpp_1.0.11         
 [61] glue_1.7.0           gridExtra_2.3        geomtextpath_0.1.5   xfun_0.42            here_1.0.1           cmdstanr_0.8.1      
 [67] distributional_0.5.0 dplyr_1.1.4          loo_2.8.0            withr_2.5.1          numDeriv_2016.8-1.1  fastmap_1.1.1       
 [73] boot_1.3-28.1        fansi_1.0.6          digest_0.6.34        R6_2.5.1             estimability_1.5     textshaping_1.0.0   
 [79] colorspace_2.1-0     lpSolve_5.6.20       R.methodsS3_1.8.2    utf8_1.2.4           tidyr_1.3.1          generics_0.1.3      
 [85] data.table_1.17.0    class_7.3-22         prettyunits_1.2.0    graphlayouts_1.1.1   parameters_0.21.5    pkgconfig_2.0.3     
 [91] gtable_0.3.4         htmltools_0.5.7      scales_1.3.0         posterior_1.6.0      rstudioapi_0.15.0    coda_0.19-4.1       
 [97] checkmate_2.3.1      nlme_3.1-162         curl_5.2.0           nloptr_2.0.3         proxy_0.4-27         cachem_1.0.8        
[103] zoo_1.8-12           stringr_1.5.1        KernSmooth_2.23-21   parallel_4.3.1       pillar_1.9.0         grid_4.3.1          
[109] stringmagic_1.1.2    vctrs_0.6.5          fixest_0.12.1        xtable_1.8-4         evaluate_0.23        magick_2.8.3        
[115] mvtnorm_1.2-4        transformr_0.1.5     cli_3.6.2            compiler_4.3.1       rlang_1.1.1          crayon_1.5.2        
[121] rstantools_2.4.0     future.apply_1.11.1  labeling_0.4.3       classInt_0.4-10      gganimate_1.0.9      ps_1.7.6            
[127] stringi_1.8.3        viridisLite_0.4.2    QuickJSR_1.4.0       tables_0.9.17        lmerTest_3.1-3       munsell_0.5.0       
[133] Brobdingnag_1.2-9    bayestestR_0.13.2    V8_4.4.2             Matrix_1.6-5         hms_1.1.3            future_1.33.1       
[139] gridtext_0.1.5       memoise_2.0.1        RcppParallel_5.1.7  
