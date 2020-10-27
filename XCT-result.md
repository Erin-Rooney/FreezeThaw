XCT results
================
Erin C Rooney
10/26/2020

## Measuring the Soil Pore Network response to freeze/thaw in permafrost soil aggregates using X-Ray Computed Tomography

## Pore Throat Diameters

<details>

<summary>click to open stats</summary>

### Statistics

    #> [1] "after"   "before"  "before "

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  d
    #> W = 0.84236, p-value = 2.295e-08

    #> 
    #>  Wilcoxon signed rank test with continuity correction
    #> 
    #> data:  breadth_dist by trmt
    #> V = 1387.5, p-value = 0.09974
    #> alternative hypothesis: true location shift is less than 0

    #> [1] 0.09974333

</details>

<details>

<summary>click to open figures</summary>

### Figures

    #> [1] "28_38_12" "28_38_28" "40_50_16" "40_50_28" "41_50_16" "41_50_28"

    #> 'data.frame':    180 obs. of  8 variables:
    #>  $ site        : chr  "tool" "tool" "tool" "tool" ...
    #>  $ trmt        : chr  "after" "after" "after" "after" ...
    #>  $ sample      : chr  "28_38_12" "28_38_12" "28_38_12" "28_38_12" ...
    #>  $ bin         : chr  "a" "b" "c" "d" ...
    #>  $ breadth_mm  : num  0.15 0.14 0.13 0.12 0.11 0.1 0.09 0.08 0.07 0.06 ...
    #>  $ breadth_um  : int  150 140 130 120 110 100 90 80 70 60 ...
    #>  $ breadth_freq: int  3 0 0 4 3 4 7 6 15 3 ...
    #>  $ breadth_dist: num  0.0297 0 0 0.0396 0.0297 ...

![](images/Pore%20Throat%20Distributions-1.png)<!-- -->

</details>

## Pore Coordination Numbers

<details>

<summary>click to open stats</summary>

### Statistics

    #>  [1] "1"  "2"  "3"  "4"  "5"  "6"  "7"  "8"  "9"  "10" "11" "12" "13" "14" "15"
    #> [16] "16"

    #> [1] "after"  "before"

    #> 
    #>  Shapiro-Wilk normality test
    #> 
    #> data:  d
    #> W = 0.76374, p-value = 4.011e-11

    #> 
    #>  Wilcoxon signed rank test with continuity correction
    #> 
    #> data:  freq by trmt
    #> V = 660, p-value = 0.3589
    #> alternative hypothesis: true location shift is not equal to 0

    #> [1] 0.3588726

</details>

<details>

<summary>click to open figures</summary>

### Figures

![](images/Pore%20Coordination%20Numbers-1.png)<!-- -->

</details>

## Air and Water-filled Pore Volumes

<details>

<summary>click to open stats</summary>

### Statistics

    #>             Df   Sum Sq   Mean Sq F value Pr(>F)  
    #> trmt         1 0.002641 0.0026411   6.285 0.0311 *
    #> Residuals   10 0.004202 0.0004202                 
    #> ---
    #> Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

    #> $statistics
    #>        MSerror Df     Mean       CV        MSD
    #>   0.0004202165 10 0.038384 53.40554 0.02637049
    #> 
    #> $parameters
    #>    test name.t ntr StudentizedRange alpha
    #>   Tukey   trmt   2         3.151064  0.05
    #> 
    #> $means
    #>        conn_water_perc        std r      Min      Max        Q25       Q50
    #> after        0.0235485 0.01006972 6 0.007835 0.032096 0.01765550 0.0274340
    #> before       0.0532195 0.02718517 6 0.014306 0.094367 0.04357925 0.0486655
    #>               Q75
    #> after  0.03093050
    #> before 0.06573825
    #> 
    #> $comparison
    #> NULL
    #> 
    #> $groups
    #>        conn_water_perc groups
    #> before       0.0532195      a
    #> after        0.0235485      b
    #> 
    #> attr(,"class")
    #> [1] "group"

    #>             Df    Sum Sq   Mean Sq F value Pr(>F)
    #> trmt         1 7.610e-06 7.615e-06   1.025  0.335
    #> Residuals   10 7.427e-05 7.427e-06

    #>             Df   Sum Sq   Mean Sq F value Pr(>F)
    #> trmt         1 0.000357 0.0003569   0.425  0.529
    #> Residuals   10 0.008406 0.0008406

    #>             Df    Sum Sq   Mean Sq F value Pr(>F)
    #> trmt         1 9.450e-06 9.447e-06   0.586  0.462
    #> Residuals   10 1.613e-04 1.613e-05

</details>

<details>

<summary>click to open figures</summary>

### Figures

![](images/Pore%20volumes-1.png)<!-- -->

</details>

### Session Info

<details>

<summary>Session Info</summary>

    #> [1] "2020-10-26"

    #> R version 4.0.1 (2020-06-06)
    #> Platform: x86_64-w64-mingw32/x64 (64-bit)
    #> Running under: Windows 10 x64 (build 19041)
    #> 
    #> Matrix products: default
    #> 
    #> locale:
    #> [1] LC_COLLATE=English_United States.1252 
    #> [2] LC_CTYPE=English_United States.1252   
    #> [3] LC_MONETARY=English_United States.1252
    #> [4] LC_NUMERIC=C                          
    #> [5] LC_TIME=English_United States.1252    
    #> 
    #> attached base packages:
    #> [1] stats     graphics  grDevices utils     datasets  methods   base     
    #> 
    #> other attached packages:
    #>  [1] patchwork_1.0.1  PNWColors_0.1.0  ggpubr_0.4.0     PairedData_1.1.1
    #>  [5] lattice_0.20-41  mvtnorm_1.1-1    gld_2.6.2        MASS_7.3-51.6   
    #>  [9] forcats_0.5.0    stringr_1.4.0    dplyr_1.0.0      purrr_0.3.4     
    #> [13] readr_1.3.1      tidyr_1.1.0      tibble_3.0.1     ggplot2_3.3.2   
    #> [17] tidyverse_1.3.0  agricolae_1.3-3 
    #> 
    #> loaded via a namespace (and not attached):
    #>  [1] nlme_3.1-148      fs_1.4.2          lubridate_1.7.9   httr_1.4.1       
    #>  [5] tools_4.0.1       backports_1.1.7   R6_2.4.1          AlgDesign_1.2.0  
    #>  [9] DBI_1.1.0         questionr_0.7.1   colorspace_1.4-1  withr_2.2.0      
    #> [13] tidyselect_1.1.0  klaR_0.6-15       curl_4.3          compiler_4.0.1   
    #> [17] cli_2.0.2         rvest_0.3.5       xml2_1.3.2        labeling_0.3     
    #> [21] scales_1.1.1      digest_0.6.25     foreign_0.8-80    rmarkdown_2.3    
    #> [25] rio_0.5.16        pkgconfig_2.0.3   htmltools_0.5.0   labelled_2.5.0   
    #> [29] dbplyr_1.4.4      fastmap_1.0.1     highr_0.8         rlang_0.4.7      
    #> [33] readxl_1.3.1      rstudioapi_0.11   shiny_1.5.0       farver_2.0.3     
    #> [37] generics_0.0.2    combinat_0.0-8    jsonlite_1.7.0    zip_2.1.1        
    #> [41] car_3.0-10        magrittr_1.5      Rcpp_1.0.4.6      munsell_0.5.0    
    #> [45] fansi_0.4.1       abind_1.4-5       lifecycle_0.2.0   stringi_1.4.6    
    #> [49] yaml_2.2.1        carData_3.0-4     grid_4.0.1        blob_1.2.1       
    #> [53] promises_1.1.1    crayon_1.3.4      lmom_2.8          miniUI_0.1.1.1   
    #> [57] haven_2.3.1       hms_0.5.3         knitr_1.29        pillar_1.4.6     
    #> [61] ggsignif_0.6.0    reprex_0.3.0      glue_1.4.1        evaluate_0.14    
    #> [65] data.table_1.12.8 modelr_0.1.8      vctrs_0.3.2       httpuv_1.5.4     
    #> [69] cellranger_1.1.0  gtable_0.3.0      assertthat_0.2.1  openxlsx_4.2.2   
    #> [73] xfun_0.15         mime_0.9          xtable_1.8-4      broom_0.7.0      
    #> [77] e1071_1.7-3       rstatix_0.6.0     later_1.1.0.1     class_7.3-17     
    #> [81] cluster_2.1.0     ellipsis_0.3.1

</details>
