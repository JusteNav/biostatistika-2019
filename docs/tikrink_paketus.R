
bs_check_packages <- function() {
    # v2.0
    message(Sys.time())
    chk_versija <- "\n'R' versijos ir idiegtu paketu patikra \n    (patikros kodas atnaujintas 2019-04-11)\n\n"
    cat(chk_versija)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_pkgs_installed <- function() {
        pkgs_existing <- installed.packages()[, c("Package", "Version")]
        rownames(pkgs_existing) <- NULL
        as.data.frame(pkgs_existing, stringsAsFactors = FALSE)
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_pkgs_recommended <- function() {
        pkgs_vec <-
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
              
              "addin.tools", 
              "addins.rmd", 
              "addins.rs", 
              
              "pander",
              "knitr",
              "rmarkdown",
              
              "Rcmdr", 
              "RcmdrMisc", 
              "RcmdrPlugin.EZR",
              "RcmdrPlugin.KMggplot2",
              "RcmdrPlugin.EZR.as.menu",
              "RcmdrPlugin.biostat"
            ) 
        
        data.frame(nr = seq_along(pkgs_vec),
                   paketas = pkgs_vec,
                   stringsAsFactors = FALSE)
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_pkgs_req_version <- function() {
        read.table(
            header = TRUE, sep = "\t", stringsAsFactors = FALSE, quote = "'",
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
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_pkgs_installation_code <- function() {
        read.table(
            header = TRUE, sep = "\t", stringsAsFactors = FALSE, quote = "'",
            text = 
                'paketas	            diegimo_kodas
latex2exp	            remotes::install_github("stefano-meschiari/latex2exp", upgrade = TRUE)
skimr	                remotes::install_github("ropenscilabs/skimr", ref = "v2", upgrade = TRUE)
pander	                remotes::install_github("Rapporter/pander", upgrade = TRUE)
RcmdrPlugin.EZR.as.menu	remotes::install_github("GegznaV/RcmdrPlugin.EZR@ezr_as_menu", upgrade = TRUE)
RcmdrPlugin.biostat	    remotes::install_github("GegznaV/RcmdrPlugin.biostat", ref = "biostat19_not_final", upgrade = TRUE)
addin.tools	            remotes::install_github("GegznaV/addin.tools", upgrade = TRUE)
addins.rmd	            remotes::install_github("GegznaV/addins.rmd", upgrade = TRUE)
addins.rs	            remotes::install_github("GegznaV/addins.rs", upgrade = TRUE)
')
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_r_version <- function(recommended_version) {
        # recommended_version <- "3.5.3"
        
        current_r_version <- paste0(R.version$major, ".", R.version$minor)
        
        if (compareVersion(current_r_version, recommended_version) < 0) {
            
            cat("\n\n   Bet pirmiausia rekomenduojama atnaujinti programa 'R': \n\n",
                "   ", current_r_version, " - dabartine 'R' versija jusu kompiuteryje. \n", 
                "   ", recommended_version, " - rekomenduojama 'R' versija. Ja galite atsisiusti is https://cran.r-project.org/\n\n",
                "     - [Windows] https://cran.r-project.org/bin/windows/base/R-", recommended_version, "-win.exe\n",
                "     - [Mac]     https://cran.r-project.org/bin/macosx/R-", recommended_version, ".pkg \n\n", sep = "")
        } 
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    package_installation_code <- function(pkgs) {
        
        pkgs_df <- data.frame(paketas = pkgs, l = seq_along(pkgs), stringsAsFactors = FALSE)
        
        pkgs_code <- get_pkgs_installation_code()
        
        tmp_df <- merge(
            pkgs_df, pkgs_code,
            all.x = TRUE,
            sort = FALSE)
        
        # Sort
        tmp_df <- tmp_df[order(tmp_df$l), ]
        
        installation_code <- 
            ifelse(is.na(tmp_df$diegimo_kodas), 
                   paste0('install.packages("', tmp_df$paketas, '")'),
                   trimws(tmp_df$diegimo_kodas))
        
        installation_code
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    compare_version <- function(v_installed, v_required) {
        
        busena <- numeric(length(v_installed))
        
        v_installed <- as.character(v_installed)
        v_required  <- as.character(v_required)
        
        for (i in seq_along(busena)) {
            busena[i] <- compareVersion(v_installed[i], v_required[i])
        }
        busena
    }
    
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    recommended_ok <- TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    tmp <- merge(get_pkgs_recommended(), get_pkgs_installed(), by.x = "paketas", by.y = "Package", all.x = TRUE)
    tmp <- merge(tmp, get_pkgs_req_version(), by = "paketas", all.x = TRUE)
    
    tmp$up_to_date <- with(tmp, compare_version(Version, reikiama_versija))
    tmp$code <- package_installation_code(pkgs = tmp$paketas)
    
    tmp <- tmp[order(tmp$nr), ]
    
    tmp2 <- tmp[tmp$up_to_date < 0, ]
    
    installation_code <- tmp2$code
    
    tmp3 <- tmp2[, c(-2, -5, -6)]
    tmp3 <- setNames(tmp3, c("Paketas", "Idiegta_versija", "Reikiama_min_versija"))
    tmp3$Idiegta_versija[is.na(tmp3$Idiegta_versija)] <- "[ neidiegta ]"    
    tmp3$Reikiama_min_versija[is.na(tmp3$Reikiama_min_versija)] <- ""    
    rownames(tmp3) <- NULL
    
    if (nrow(tmp3) > 0) {
        recommended_ok <- FALSE
        
        cat("\n--- Paketai, kuriuos rekomenduojama idiegti arba atnaujinti: ----------\n\n")
        print(tmp3)
        # cat("\n--------------------------------------------------------------------\n")
    }
    
    cat("\n--- Patikra baigta: --------------------------------------------------- \n")

    if (recommended_ok) {
        cat("\n   Rekomenduojami paketai jusu kompiuteryje jau yra.\n")
        
    } else {

        cat(
            "\n   Rekomenduojama atnaujinti/idiegti paketus :\n",
            "     1. isjunkite 'RStudio' projekta, \n",
            "     2. perkraukite 'R',                                    <--- \n",
            "     3. tik tada naudodami si koda idiekite paketus: \n\n", sep = "")
        
        cat(installation_code, sep = "\n")
    }
    
    check_r_version(recommended_version = "3.5.3")
    
}

bs_check_packages()


