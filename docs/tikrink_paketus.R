
bs_check_packages <- function() {
    
    message(Sys.time())
    chk_versija <- "\n'R' versijos ir idiegtu paketu patikra (v1.2, 2019-04-10)\n\n"
    cat(chk_versija)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    recommended_ok <- TRUE
    
    recommended_pkgs <- 
        c("remotes",
          "devtools",
          "ctv", 
          
          "ggplot2", 

          "data.table",  
          "BBmisc",  
          "checkmate", 
          "infer", 
          "aplpack",
          "arm", 
          "coda",
          "leaps",
          "lmtest", 
          "matrixcalc",
          "sem", 
          "TH.data",
          "rgl",
          "mi", 
          "multcomp",
          "multcompView",
          "mvtnorm", 
          "car", 
          "carData",
          "RCurl", 
          "openxlsx", 
          "tidyselect",
          "readxl",
          "forcats", 
          "stringi", 
          "stringr",
          "dplyr",
          "tidyr",
          "broom",
          "tidyverse",
          "sigr", 
          "fs",
          "shiny", 
          "PMCMR", 
          "PMCMRplus",
          
          "officer", 
          "flextable",
          "rcompanion",
          "rvg",
          
          "vcd",
          "vcdExtra",
          "fitdistrplus",
          
          "descriptr",
          "userfriendlyscience",
          "DescTools",
          "skimr", 
          
          "ggthemes", 
          "ggpubr",
          "ggrepel", 
          "ggmosaic",
          "cowplot",
          "qqplotr", 
          "plotly", 
          "ggstatsplot", 
          'latex2exp',
          
          "Rcmdr", 
          "RcmdrMisc", 
          "RcmdrPlugin.EZR",
          "RcmdrPlugin.KMggplot2",
          "RcmdrPlugin.EZR.as.menu",
          "RcmdrPlugin.biostat",
          
          "addin.tools", 
          "addins.rmd", 
          "addins.rs", 
          
          "pander",
          "knitr",
          "rmarkdown"
          
        ) 
    
    
    different_installation_code <-    read.table(header = TRUE, sep = "\t", 
                                                 stringsAsFactors = FALSE, 
                                                 quote = "'",
                                                 text = 
                                                     
                                                     'paketas	 diegimo_kodas
latex2exp	            remotes::install_github("stefano-meschiari/latex2exp", upgrade = TRUE)
skimr	                remotes::install_github("ropenscilabs/skimr", ref = "v2", upgrade = TRUE)
pander	                remotes::install_github("Rapporter/pander", upgrade = TRUE)
RcmdrPlugin.EZR.as.menu	remotes::install_github("GegznaV/RcmdrPlugin.EZR@ezr_as_menu", upgrade = TRUE)
RcmdrPlugin.biostat	    remotes::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat19_not_final", upgrade = TRUE)
addin.tools	            remotes::install_github("GegznaV/addin.tools", upgrade = TRUE)
addins.rmd	            remotes::install_github("GegznaV/addins.rmd", upgrade = TRUE)
addins.rs	            remotes::install_github("GegznaV/addins.rs", upgrade = TRUE)
')
    
    
    
    DF <- read.table(header = TRUE, sep = "\t", 
                     stringsAsFactors = FALSE, 
                     quote = "'",
                     text = 
                         
                         'paketas	            reikiama_versija 
dplyr	                0.8.0
skimr	                2.0
pander	                0.6.3
latex2exp	            0.4.0
addin.tools	            0.0.4
addins.rmd	            0.0.6
addins.rs	            0.0.5
RcmdrPlugin.EZR.as.menu	1.38
RcmdrPlugin.biostat	    0.0.26
')
    
    inst_pkgs <- as.data.frame(installed.packages()[, c("Package", "Version")])
    
    is_present_pkg <- recommended_pkgs %in% inst_pkgs$Package
    
    missing_pkgs <- recommended_pkgs[!is_present_pkg]
    
    
    if (length(missing_pkgs) > 0) {
        recommended_ok <- FALSE
        
        cat("\n--- Paketai, kuriuos rekomenduojama idiegti: -----------------------\n\n")
        cat(paste0(missing_pkgs, sep = "\n"), sep = "")
        # cat("\n--------------------------------------------------------------------\n")
    }
    
    
    # Remove not installed
    DF <- DF[DF$paketas %in% inst_pkgs$Package, ]
    
    if (nrow(DF) > 0) {
        DF$idiegta_versija = Reduce(c, lapply(DF$paketas, packageVersion))
        
        DF$busena <- NA_integer_
        
        for (i in 1:nrow(DF)) {
            DF$busena[i] <-  compareVersion(
                as.character(DF[i, "idiegta_versija"]),
                as.character(DF[i, "reikiama_versija"]))
        }
        
        DF$busena <- ifelse(DF$busena < 0, "<--- Reikia atnaujinti", "OK")
        
        needs_update <- DF$busena != "OK"
        
        if (any(needs_update)) {
            recommended_ok <- FALSE
            
            cat("\n--- Paketai, kuriuos reikia atnaujinti: ----------------------------\n\n")
            print(DF[needs_update, ])
            # cat("\n-------------------------------------------------------------------\n")
        }
        
    }
    
    cat("\n--- Patikra baigta: ---------------------------------------------- \n")
    
    
    current_r_version <- paste0(R.version$major, ".", R.version$minor)
    
    if (compareVersion(current_r_version, "3.5.3") < 0) {
        
        cat("\n   Pirmiausia rekomenduojama atnaujinti 'R' \n\n",
            "   ", current_r_version, " - dabartine 'R' versija jusu kompiuteryje. \n", 
            "   3.5.3 - rekomenduojama 'R' versija. Ja galite atsisiusti is https://cran.r-project.org/\n\n",
            "     - [Windows] https://cran.r-project.org/bin/windows/base/R-3.5.3-win.exe\n",
            "     - [Mac]     https://cran.r-project.org/bin/macosx/R-3.5.3.pkg \n\n", sep = "")
    }
    
    
    if (recommended_ok) {
        cat("\n   Rekomenduojami paketai jusu kompiuteryje jau yra.\n")
        
    } else {
        
        DF_missing_pkgs <- data.frame(paketas = missing_pkgs, stringsAsFactors = FALSE)
        
        DF_missing_pkgs <-
            merge(DF_missing_pkgs, DF[needs_update, ],
                  all = TRUE,
                  sort = FALSE)
        
        tmp_df <- 
            merge(DF_missing_pkgs, different_installation_code,
                  all.x = TRUE,
                  sort = FALSE)
        
        installation_code <- 
            ifelse(is.na(tmp_df$diegimo_kodas), 
                   paste0('install.packages("', tmp_df$paketas, '")'),
                   trimws(tmp_df$diegimo_kodas))
        
        
        cat(
            "\n   Rekomenduojama:\n",
            "     1. isjungti 'RStudio' projekta, \n",
            "     2. perkrauti, 'R'                            <--- \n",
            "     3. tik tada atnaujinti paketus naudojant koda: \n\n", sep = "")
        
        cat(installation_code, sep = "\n")
    }
}

bs_check_packages()


