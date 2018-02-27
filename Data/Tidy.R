library(dplyr)
library(readr)
library(forcats)
library(stringr)

# LOADING DATA
DF <- read_csv("323_db.csv")

# TIDY DATA
DF <- DF %>%
    mutate(
        tma = factor(tma, ordered = TRUE),
        caseid = factor(caseid, ordered = TRUE),
        subtype = fct_infreq(str_to_title(subtype)),
        figure = factor(figure),
        p53_v = as.numeric(p53_v),
        p53_d = as.numeric(p53_d),
        ki67_v = as.numeric(ki67_v),
        cycd1_v = as.numeric(cycd1_v),
        mdm2_v = as.numeric(mdm2_v)
    ) %>%
    # Traduciendo los nombres de las columnas al español
    rename(
        caso = caseid,
        foco = spot,
        subtipo = subtype,
        grado = grade
    ) %>%
    # Traduciendo los nombres de los subtipos al español
    mutate(
        subtipo = fct_recode(
            subtipo,
            "Basaloide" = "Basaloid",
            "Condilomatoso-Basaloide" = "Warty-Basaloid",
            "Verrucoso" = "Verrucous",
            "Papilar" = "Papillary",
            "Condilomatoso" = "Warty",
            NULL = "Giant Condyloma",
            "Sarcomatoide" = "Sarcomatoid"
        )
    ) %>%
    filter(
        !is.na(subtipo), !is.na(grado)
    )

# TRANSFORM
DF_sum <- DF %>%
    group_by(caso) %>%
    summarize(
        subtipo = unique(subtipo),
        grado = max(grado, na.rm = TRUE),
        p53_v_max = max(p53_v, na.rm = TRUE),
        p53_d_max = max(p53_d, na.rm = TRUE),
        ki67_v_max = max(ki67_v, na.rm = TRUE),
        ki67_d_max = max(ki67_d, na.rm = TRUE),
        cycd1_v_max = max(cycd1_v, na.rm = TRUE),
        cycd1_d_max = max(cycd1_d, na.rm = TRUE),
        mdm2_v_max = max(mdm2_v, na.rm = TRUE),
        mdm2_d_max = max(mdm2_d, na.rm = TRUE)
    )

# Ordenando los marcos de datos
DF <- DF %>%
    mutate(
        grado = fct_recode(
            factor(grado, ordered = TRUE),
            "Grado 1" = "1",
            "Grado 2" = "2",
            "Grado 3" = "3"
        )
    ) %>%
    rename(
        p53v = p53_v,
        p53d = p53_d,
        k67v = ki67_v,
        k67d = ki67_d,
        cd1v = cycd1_v,
        cd1d = cycd1_d,
        m2v = mdm2_v,
        m2d = mdm2_d
    )

DF_sum <- DF_sum %>%
    mutate(
        grado = fct_recode(
            factor(grado, ordered = TRUE),
            "Grado 1" = "1",
            "Grado 2" = "2",
            "Grado 3" = "3"
        )
    ) %>%
    rename(
        p53v = p53_v_max,
        p53d = p53_d_max,
        k67v = ki67_v_max,
        k67d = ki67_d_max,
        cd1v = cycd1_v_max,
        cd1d = cycd1_d_max,
        m2v = mdm2_v_max,
        m2d = mdm2_d_max
    )

# Guardando los marcos de datos
save(DF, DF_sum, file = "323.Rdata")
