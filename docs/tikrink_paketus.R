
bs_check_packages <- function(clear_console = FALSE) {
    
    # v2.0
    
    if (clear_console == TRUE) {
        cat("\014")
        
    } else {
        cat("\n\n")
    }
    
    line <- function() {
        cat("\n-----------------------------------------------------------------------------\n")
        
    }
    
    # Initial message --------------------------------------------------------
    line()
    cat(as.character(Sys.time()), "\n")
    
    chk_versija <- "\n\n         Programos 'R' ir idiegtu paketu patikra (versija 2019-04-28)\n\n"
    cat(chk_versija)
    
    # Functions --------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_pkgs_req_version <- function() {
        read.table(
            header = TRUE, sep = "|", stringsAsFactors = FALSE, quote = "'",
            strip.white = TRUE, text = 
                
                'paketas                    | reikiama_versija 
dplyr                       | 0.8.0
skimr                       | 2.0
pander                      | 0.6.3
Rcmdr                       | 2.5-3
car                         | 3.0-3
data.table                  | 1.12.2
ggstatsplot                 | 0.0.10
latex2exp                   | 0.4.0
readxl                      | 1.3.1
addin.tools                 | 0.0.4
addins.rmd                  | 0.0.6
addins.rs                   | 0.0.5
RcmdrPlugin.EZR.as.menu     | 1.38
RcmdrPlugin.biostat         | 0.0.35
ggstatsplot                 | 0.0.10.9000
') 
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_pkgs_installation_code <- function() {
        read.table(
            header = TRUE, sep = "|", stringsAsFactors = FALSE, quote = "'",
            strip.white = TRUE, text = 
                'paketas                | diegimo_kodas
latex2exp               | remotes::install_github("stefano-meschiari/latex2exp", upgrade = TRUE)
addin.tools             | remotes::install_github("GegznaV/addin.tools", upgrade = TRUE)
addins.rmd              | remotes::install_github("GegznaV/addins.rmd", upgrade = TRUE)
addins.rs               | remotes::install_github("GegznaV/addins.rs", upgrade = TRUE)
car                     | install.packages("car", repos = "http://R-Forge.R-project.org")
skimr                   | remotes::install_github("ropenscilabs/skimr", ref = "v2", upgrade = TRUE)
pander                  | remotes::install_github("Rapporter/pander", upgrade = TRUE)
RcmdrPlugin.EZR.as.menu | remotes::install_github("GegznaV/RcmdrPlugin.EZR@ezr_as_menu", upgrade = TRUE)
RcmdrPlugin.biostat     | remotes::install_github("GegznaV/RcmdrPlugin.biostat", upgrade = TRUE)
ggstatsplot             | remotes::install_github("IndrajeetPatil/ggstatsplot", upgrade = TRUE)
')
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_pkgs_installed <- function() {
        pkgs_existing <- installed.packages()[, c("Package", "Version")]
        rownames(pkgs_existing) <- NULL
        as.data.frame(pkgs_existing, stringsAsFactors = FALSE)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    optimize_order_of_packages <- function(
        pkgs_vec = get_pkgs_recommended()$paketas,
        recursive_dependencies = TRUE) {
        # Helpeer function that suggest how to optimize order of packages in the 
        # vector of packages in order not to repeat installation of the same
        # packages.
        
        # recursive_dependencies = TRUE requires internet connection.
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        # All packages in the list before the current one:
        # pkgs_vec[1:which(pkgs_vec == "cowplot")]
        
        # All packages in the list after the current one:
        # rev(pkgs_vec)[1:which(rev(pkgs_vec) == "cowplot")]
        
        library(tidyverse)
        
        list_after <- function(which, list) {
            rev(rev(list)[1:which(rev(list) == which)])
        }
        
        list_before <- function(which, list) {
            list[1:which(list == which)]
        }
        
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        deps <- 
            tools::package_dependencies(
                pkgs_vec, which = c("Depends", "Imports"), 
                reverse = FALSE, 
                recursive = recursive_dependencies
            )
        
        rev_deps <- 
            tools::package_dependencies(
                pkgs_vec, which = c("Depends", "Imports"),
                reverse = TRUE, 
                recursive = recursive_dependencies
            )
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        move_after <-
            imap(deps, ~ str_c(.x[.x %in% list_after(.y, list = pkgs_vec)])) %>% 
            discard(~length(.) == 0) %>% 
            imap_chr(~str_c(str_pad(.y, 20), " (move after): ",
                            str_c(.x, collapse = ", "))) %>% 
            map_chr(structure, class = "glue") %>% 
            unname()
        
        
        move_before <- 
            imap(rev_deps, ~ str_c(.x[.x %in% list_before(.y, list = pkgs_vec)])) %>% 
            discard(~length(.) == 0) %>% 
            imap_chr(~str_c(str_pad(.y, 20), " (move before): ", 
                            str_c(.x, collapse = ", "))) %>% 
            map_chr(structure, class = "glue") %>% 
            unname()
        
        
        list(move_before = move_before, move_after = move_after)
        
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_pkgs_recommended <- function() {
        pkgs_vec <- c(
            "remotes", 
            "fs",
            "devtools",
            "ctv", 
            
            "data.table",  
            "checkmate", 
            # "BBmisc", 
            "shiny", 
            
            "aplpack",
            "coda",
            "arm", 
            "leaps",
            "lmtest", 
            "matrixcalc",
            "mi", 
            "sem", 
            "TH.data",
            "mvtnorm", 
            "multcomp",
            "multcompView",
            
            "openxlsx", 
            "readxl",
            "forcats", 
            
            "carData",
            "car", 
            "RCurl", 
            
            "tidyselect",
            "stringi", 
            "stringr",
            "dplyr",
            "tidyr",
            "ggplot2", 
            "broom",
            
            "knitr",
            "rmarkdown",
            "rgl",
            
            "tidyverse",
            
            # "DT",
            "sigr", 
            "PMCMR", 
            "PMCMRplus",
            
            "officer", 
            "flextable",
            "rvg",
            
            "vcd",
            # "vcdExtra",
            # "fitdistrplus",
            
            "plotly", 
            "ggthemes", 
            "ggrepel", 
            # "cowplot",
            "qqplotr", 
            "ggmosaic",
            "ggpubr",
            "latex2exp",
            
            "XNomial",
            
            "addin.tools", 
            "addins.rmd", 
            "addins.rs", 
            
            "pander",
            
            # "descriptr",
            
            "chemCal",
            "userfriendlyscience",
            "DescTools",
            "rcompanion",
            "skimr", 
            # "infer",
            
            "RcmdrMisc", 
            "Rcmdr", 
            "RcmdrPlugin.EZR",
            "RcmdrPlugin.KMggplot2",
            "RcmdrPlugin.EZR.as.menu",
            "RcmdrPlugin.biostat",
            
            "AMR",
            "RVAideMemoire",
            "EMT",
            "XNomial",
            
            "ggstatsplot", # Package has a lot of dependencies
            NULL
        ) 
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        # Check, if the order of packages is optimal.
        # character(0) indicates that nothing must be moved.
        
        # optimize_order_of_packages(pkgs_vec, recursive_dependencies = FALSE)
        # optimize_order_of_packages(pkgs_vec, recursive_dependencies = TRUE)
        # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
        
        data.frame(nr = seq_along(pkgs_vec),
                   paketas = pkgs_vec,
                   stringsAsFactors = FALSE)
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_r_version <- function(recommended_r_version, packages_ok = TRUE) {
        # recommended_r_version <- "3.5.3"
        
        current_r_version <- paste0(R.version$major, ".", R.version$minor)
        
        if (compareVersion(current_r_version, recommended_r_version) < 0) {
            line()
            cat("\nPirmiausia rekomenduojama atnaujinti programa 'R': \n\n",
                "   ", current_r_version, " - dabartine 'R' versija jusu kompiuteryje. \n", 
                "   ", recommended_r_version, " - rekomenduojama 'R' versija. Ja galite atsisiusti is:\n\n",
                
                "               https://cran.r-project.org/\n\n",
                
                "   - [Windows] https://cran.r-project.org/bin/windows/base/old/3.5.3/ \n",
                "   - [Mac]     https://cran.r-project.org/bin/macosx/R-3.5.3.pkg      \n",
                "   - [Linux]   https://cran.r-project.org/bin/linux/                  \n",
                
                "\n\n", 
                
                "PASTABA: balandį bus isleista 'R' versija 3.6.0. Jei isidiegsite ja, \n",
                "         visus paketus reikes atsisiusti is naujo.\n",
                sep = "")
            # "     - [Windows] https://cran.r-project.org/bin/windows/base/R-", recommended_r_version, "-win.exe\n",
            # "     - [Mac]     https://cran.r-project.org/bin/macosx/R-", recommended_r_version, ".pkg \n", sep = "")
            return(TRUE)
        }
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_rs_version <- function(recommended_version) {
        # recommended_version <- "1.2.1335"
        
        if (.Platform$GUI != "RStudio") {
            
            line()
            cat("\nRekomenduojama isidiegti programa 'RStudio': \n\n",
                "   https://www.rstudio.com/products/rstudio/download/\n\n",
                sep = "")
            return(TRUE)
            
        } else {
            
            current_rs_version <- RStudio.Version()$version
            
            if (current_rs_version < recommended_version) {
                
                line()
                cat("\nRekomenduojama atnaujinti programa 'RStudio': \n\n",
                    "   ", as.character(current_rs_version), " - dabartine 'RStudio' versija jusu kompiuteryje. \n", 
                    "   ", recommended_version, " - rekomenduojama si arba naujesne versija, nurodyta tinklapyje: \n\n",
                    "   https://www.rstudio.com/products/rstudio/download/\n\n",
                    sep = "")
                return(TRUE)
            }
        }
    }    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    package_installation_code <- function(pkgs) {
        
        pkgs_df   <- data.frame(paketas = pkgs, l = seq_along(pkgs), stringsAsFactors = FALSE)
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
    
    # Main calculations for packages -----------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    recommended_ok <- TRUE
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    tmp <- merge(get_pkgs_recommended(), get_pkgs_installed(),
                 by.x = "paketas", by.y = "Package", all.x = TRUE)
    
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
    
    # Print output (required packages) ---------------------------------------
    if (nrow(tmp3) > 0) {
        recommended_ok <- FALSE
        
    }
    
    cat("\n\n___ Patikros ataskaita: _____________________________________________________ \n")
    
    # Check R version --------------------------------------------------------
    chk_r <- check_r_version(recommended_r_version = "3.5.3")
    
    
    # Check RStudio version --------------------------------------------------
    chk_rs <- check_rs_version(recommended_version = "1.2.1335")
    
    
    # Check packages ---------------------------------------------------------
    if (recommended_ok) {
        if (isTRUE(chk_r) || isTRUE(chk_rs)) {
            line()   
        }
        cat("\n   Rekomenduojamos minimalios paketu versijos jusu kompiuteryje jau yra.\n")
        line()
        
    } else {
        line()
        
        cat("\n--- Paketai, kuriuos rekomenduojama idiegti arba atnaujinti: ----------------\n\n")
        print(tmp3)
        
        cat(
            "\n\n   Pries diegdami (atnaujindami) paketus:\n",
            "     1. uzdarykite 'RStudio' projekta (jei dirbate projekte); \n",
            "     2. perkraukite 'R';                                                         <--- \n",
            "     3. ir tik tada idiekite paketus: nukopijuokite zemiau nurodyta koda i langa \n",
            "        'Console' ir paspauskite 'Enter' klavisa: \n\n", sep = "")
        
        cat(installation_code, sep = "\n")
        
        line()
        
        cat("\n [!] SVARBU: Atidziai nuo pradziu perskaitykite VISA patikros ataskaita!!! \n",
            "            Jei reikia, pirmiausia atnaujinkite programas.\n",
            "            Isidiege nurodytus paketus sia patikra pakartokite is naujo.")
    }
    
}

bs_check_packages(clear_console = FALSE)


