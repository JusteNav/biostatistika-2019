initial_objects <- ls()

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
optimize_order_of_packages <- function(pkgs_vec = get_pkgs_recommended()$paketas, 
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
        "curl",
        "stringi", 
        "stringr",
        "devtools",
        "ctv", 
        "BiocManager",
        "conflicted",
        
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
        "alr4",
        "RCurl", 
        
        "tidyselect",
        "dplyr",
        "tidyr",
        "ggplot2", 
        "broom",
        
        "knitr",
        "rmarkdown",
        "bookdown",
        "blogdown",
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
        "cowplot",
        "qqplotr", 
        "ggmosaic",
        "ggpubr",
        "latex2exp",
        
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
        # "report",
        # "infer",
        
        "RcmdrMisc", 
        "Rcmdr", 
        "RcmdrPlugin.EZR",
        "RcmdrPlugin.KMggplot2",
        "RcmdrPlugin.EZR.as.menu",
        "RcmdrPlugin.biostat",
        
        "AMR",
        "RVAideMemoire",
        # "EMT",  # dependency of rcompanion 
        "XNomial",
        
        
        # "import",
        "adventr",
        "gghighlight",
        
        "mlr",
        
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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_pkgs_req_version <- function() {
    read.table(
        header = TRUE, sep = "|", stringsAsFactors = FALSE, quote = "'",
        strip.white = TRUE, text = '
paketas                     | reikiama_versija 
dplyr                       | 0.8.0
skimr                       | 2.0
pander                      | 0.6.3
Rcmdr                       | 2.5-3
car                         | 3.0-4
data.table                  | 1.12.2
latex2exp                   | 0.4.0
readxl                      | 1.3.1
addin.tools                 | 0.0.4
addins.rmd                  | 0.0.6
addins.rs                   | 0.0.5
RcmdrPlugin.EZR.as.menu     | 1.38
RcmdrPlugin.biostat         | 0.0.41
ggstatsplot                 | 0.0.11
gghighlight                 | 0.1.0.9000
') 
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
get_pkgs_installation_code <- function() {
    read.table(
        header = TRUE, sep = "|", stringsAsFactors = FALSE, quote = "'",
        strip.white = TRUE, text = '
paketas                 | diegimo_kodas
latex2exp               | remotes::install_github("stefano-meschiari/latex2exp", upgrade = TRUE)
addin.tools             | remotes::install_github("GegznaV/addin.tools", upgrade = TRUE)
addins.rmd              | remotes::install_github("GegznaV/addins.rmd", upgrade = TRUE)
addins.rs               | remotes::install_github("GegznaV/addins.rs", upgrade = TRUE)
car                     | install.packages("car", repos = "http://R-Forge.R-project.org", dependencies = TRUE)
skimr                   | remotes::install_github("ropenscilabs/skimr", ref = "v2", upgrade = TRUE)
pander                  | remotes::install_github("Rapporter/pander", upgrade = TRUE)
RcmdrPlugin.EZR.as.menu | remotes::install_github("GegznaV/RcmdrPlugin.EZR@ezr_as_menu", upgrade = TRUE)
RcmdrPlugin.biostat     | remotes::install_github("GegznaV/RcmdrPlugin.biostat", upgrade = TRUE)
gghighlight             | remotes::install_github("yutannihilation/gghighlight", upgrade = TRUE)
adventr                 | remotes::install_github("profandyfield/adventr", upgrade = TRUE)
mlr                     | install.packages("mlr", dependencies = TRUE)
')
}
# ggstatsplot             | remotes::install_github("IndrajeetPatil/ggstatsplot", upgrade = TRUE)
# report                  | remotes::install_github("easystats/report", upgrade = TRUE)

# ============================================================================

bs_check_packages <- function(clear_console = NULL, 
                              recommended_r_version  = "3.5.3",
                              recommended_rs_version = "1.2.1335") {
    
    data         <- "2019-07-24"
    kodo_versija <- "v2.2"
    
    if (Sys.info()[["sysname"]] == "Windows") {
        Sys.setlocale(locale = "Lithuanian")
    }
    # ------------------------------------------------------------------------
    if (is.null(clear_console)) {
        cat("Ar norite išvalyti komandų langą (Console)? \n(Įrašykite varianto pasirinkimo numerį)\n")
        ans <- utils::menu(c("Taip", "Ne")) 
        if (ans == 1) {
            cat("\014")
        } else {
            cat("\n\n")
        }

    } else if (clear_console == TRUE) {
        cat("\014")
        
    } else {
        cat("\n\n")
    }
    
    draw_line <- function() {
        cat("\n-----------------------------------------------------------------------------\n")
    }
    
    # Initial message --------------------------------------------------------
    # cat("PRADŽIA ")
    
    cat(as.character(Sys.time()), "\n")
    chk_versija <- paste0(
        "\n\n         Reikiamų programų ir paketų patikra (versija ", data, ")\n\n")
    
    cat(chk_versija)
    cat("\n\n___ Patikros ataskaita: _____________________________________________________ \n")
    
    # cat("\nTechninė informacija: \n\n")
    cat("\n")
    cat(sessionInfo()$running)
    cat("\n")
    
    " " <- Sys.getenv(c("USERNAME", "HOME", "R_USER", "R_HOME", "R_LIBS_USER"))
    print(as.data.frame(` `))
    
    draw_line()
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # IF path with non-enlgish letters ---------------------------------------
    if (grepl("\\?", Sys.getenv("R_LIBS_USER"))) {
        
        cat("\n",
            'Aplanko, į kurį norima diegti paketus (t.y., bibliotekos, žymima \n',
            '„R_LIBS_USER“), pavadinime yra neangliškų simbolių, kurių programa \n',
            'nesupranta ir užrašo klaustukais („?“). Dėl to nepavyks įsidiegti \n',
            'programos R papildinių, vadinamų paketais. Vieną iš galimų sprendimo \n',
            'variantų galite rasti šiame tinklapyje:\n\n',
            
            '   https://mokymai.github.io/biostatistika-2019/diegimas.html#diegti-r-sprendimai',
            '\n\n\n',
        
            "Atlikę nurodymus patikrą pakartokite iš naujo.",
            
            sep = "")
        
        draw_line()
        return()
    }
    
    # ------------------------------------------------------------------------
    # Functions --------------------------------------------------------------
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    get_pkgs_installed <- function() {
        pkgs_existing <- installed.packages()[, c("Package", "Version")]
        rownames(pkgs_existing) <- NULL
        as.data.frame(pkgs_existing, stringsAsFactors = FALSE)
    }
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_r_version <- function(recommended_r_version, packages_ok = TRUE) {
        # recommended_r_version <- "3.5.3"
        
        current_r_version <- paste0(R.version$major, ".", R.version$minor)
        
        if (compareVersion(current_r_version, recommended_r_version) < 0) {
            draw_line()
            cat("\nPirmiausia rekomenduojama atnaujinti programą 'R': \n\n",
                "   ", current_r_version, " - dabartinė 'R' versija jūsų kompiuteryje. \n", 
                "   ", recommended_r_version, " (arba naujesnė) - rekomenduojama 'R' versija. Ją galite atsisiųsti iš:\n\n",
                
                "               https://cran.r-project.org/\n\n",
                
                "   - [Windows] https://cran.r-project.org/bin/windows/base/old/3.5.3/ \n",
                "   - [Mac]     https://cran.r-project.org/bin/macosx/R-3.5.3.pkg      \n",
                "   - [Linux]   https://cran.r-project.org/bin/linux/                  \n",
                
                "\n\n", 
                
                "PASTABA: Jei įsidiegsite 'R' versiją 3.6.0 ar naujesnę, \n",
                "         visus reikiamus paketus teks atsisiųsti iš naujo.\n",
                sep = "")
            # "     - [Windows] https://cran.r-project.org/bin/windows/base/R-", recommended_r_version, "-win.exe\n",
            # "     - [Mac]     https://cran.r-project.org/bin/macosx/R-", recommended_r_version, ".pkg \n", sep = "")
            return(TRUE)
        }
    }
    
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    check_rs_version <- function(recommended_rs_version) {
        # recommended_rs_version <- "1.2.1335"
        
        arch_bits <- Sys.info()[["machine"]]

        if (arch_bits == "x86-32") {
            # For 32 bits system:
            recommended_rs_version <- "1.1.463"
            rs_download_url <- paste0(
                "   Versiją 1.1.463 skirtą 32 bitų (senesnėms) sistemoms galite atsisiųsti iš svetainės \n",
                "   https://support.rstudio.com/hc/en-us/articles/206569407-Older-Versions-of-RStudio \n\n")
            
        } else {
            # For 64 bits system
            recommended_rs_version <- recommended_rs_version
            rs_download_url <- paste0(
                "   Naujausią versiją galite atsisiųsti iš svetainės \n",
                "   https://www.rstudio.com/products/rstudio/download/\n\n"
            )
        }
        
        if (.Platform$GUI != "RStudio") {
            
            draw_line()
            cat("\nRekomenduojama įsidiegti programą 'RStudio', jei jos dar neturite. \n\n",
                rs_download_url,
                sep = "")
            return(TRUE)
            
        } else {
            
            current_rs_version <- RStudio.Version()$version
            
            if (current_rs_version < recommended_rs_version) {
                
                draw_line()
                cat("\nRekomenduojama atnaujinti programą 'RStudio': \n\n",
                    "   ", as.character(current_rs_version), " - dabartinė 'RStudio' versija jūsų kompiuteryje. \n", 
                    "   ", recommended_rs_version, " - rekomenduojama minimali versija. \n\n",
                    rs_download_url,
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
    
    # Check R version --------------------------------------------------------
    chk_r <- check_r_version(recommended_r_version = recommended_r_version)
    
    
    # Check RStudio version --------------------------------------------------
    chk_rs <- check_rs_version(recommended_rs_version = recommended_rs_version)
    
    
    # Check packages ---------------------------------------------------------
    if (recommended_ok) {
        if (isTRUE(chk_r) || isTRUE(chk_rs)) {
            draw_line()   
        }
        cat("\n   Rekomenduojamos minimalios paketų versijos jūsų kompiuteryje jau yra.")
        cat("\n")
        draw_line()
        
    } else {
        draw_line()
        
        cat("\n--- Paketai, kuriuos rekomenduojama įdiegti arba atnaujinti: ----------------\n\n")
        print(tmp3)
        
        cat(
            "\n\n   Prieš diegdami (atnaujindami) paketus:\n",
            "     1. uždarykite 'RStudio' projektą (jei dirbate projekte); \n",
            "     2. perkraukite 'R';                                                         <--- \n",
            "     3. ir tik tada įdiekite paketus: nukopijuokite žemiau nurodytą kodą į langą \n",
            "        'Console' ir paspauskite 'Enter' klavišą: \n\n", sep = "")
        
        cat(installation_code, sep = "\n")
        
        draw_line()
        
        cat("\n [!] SVARBU: \n",
            "            Atidžiai nuo pradžių perskaitykite VISĄ patikros ataskaitą!!! \n",
            "            Jei reikia, pirmiausia atnaujinkite programas. \n",
            "            Įsidiegę nurodytus paketus šią patikrą pakartokite iš1 naujo.")
    }
    
}

bs_check_packages()

# Clean up
remove(list = setdiff(ls(), initial_objects))

