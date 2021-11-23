## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Script:            Political Favoritism in Colombia (estimations)
##
## Author:            Carlos A. Toruño Paniagua
##
## Email:             c.toruopaniagua@stud.uni-goettingen.de
##
## Creation date:     June, 2021
##
## This version:      August 21th, 2021
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
## Outline:                                                                                                 ----
##                1.  FIGURE: GEOGRAPHICAL DISTRIBUTION OF SAMPLE
##                2.  TABLE: BALANCE TESTS
##                3.  FIGURE: McCRARY DENSITY TEST + STRUCTURAL ADV.
##                4.  TABLE: YEARLY OUTCOME EVOLUTION
##                5.  FIGURE: RDD ESTIMATION - GLOBAL
##                6.  ESTIMATIONS PREPARATIONS
##                7.  TABLE: RDD ESTIMATIONS - MAIN REGS
##                8.  FIGURE: RDD LOCAL ESTIMATION
##                9.  FIGURE: ROBUSTNESS CHECKS - BANDWIDTH
##                10. FIGURE: ROBUSTNESS CHECKS - FAKE CUT-OFF
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Working directory set (when working outside project)
# setwd("/Users/carlostorunopaniagua/Documents/MA in Development Economics/Thesis/Data")
rm(list=ls())

# Required packages
lapply(list("haven", "estimatr", "patchwork", "modelsummary", "stargazer", "sf", "knitr", "modelsummary",
            "kableExtra", "rdd", "rdrobust", "plm", "rmapshaper", "wesanderson", "tidyverse"), 
       library, character.only = T)

# Default option for kable
options(knitr.table.format = "latex")


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                1. FIGURE: GEOGRAPHICAL DISTRIBUTION OF SAMPLE                                            ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading base boundary data
colombia_bounds.sf <- st_read("./Data/Data - Raw/DANE GEODATA/MGN_MPIO_POLITICO.shp", quiet=T) %>%
  ms_simplify(., keep = 0.10) %>%
  mutate(id_dane = as.numeric(MPIO_CDPMP)) %>%
  select(id_dane)

# Opening data
balance.sf <- read_delim("./Data/Data - Processed/colombia_balance.csv", delim = ";") %>%
  left_join(colombia_bounds.sf, by = "id_dane") %>%
  st_as_sf(.)

# Drawing maps for 2011 and 2015 elections
maps.ls <- lapply(list("2011" = 2011, "2015" = 2015), function(x) {
  algn_var <- paste("polalgn_", x, sep = "")
  running_var <- paste("margin_", x, sep = "")
  
  plot <- ggplot(data = colombia_bounds.sf) +
    geom_sf(fill = "gray92") +
    geom_sf(data = balance.sf %>% 
              mutate(new_algn = as.factor(if_else(abs(get(running_var)) > 10 | is.na(get(running_var)),
                                                        3, get(algn_var)))),
            aes(fill = new_algn)) +
    ggtitle(paste("Local elections", x, sep = " ")) +
    theme_bw() +
    coord_sf() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          panel.border = element_blank(),
          panel.background = element_rect(fill = "white")) +
    scale_fill_manual("Municipality:", labels = c("Opposition", "Rulling party", "Not in sample"), 
                      values = c("#7294D4", "#D67236", "gray92")) +
    scale_x_continuous(expand=c(0, 0)) + 
    scale_y_continuous(expand=c(0, 0))
})


# Multipanel map of sample
(maps.ls[["2011"]] | maps.ls[["2015"]]) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom",
        text = element_text(size = 13, family = "Ledger"),
        plot.title = element_text(size = 18))
ggsave("./Plots and tables/sample_map.png", scale = 1.5)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                2. TABLE: BALANCE TESTS                                                                   ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Covariates names
balance_vars <- c("Total revenues (millions COP)", "Total SGP transfers (millions COP)", "Population", 
                  "Rurality index", "Distance to state capital (km)", "Total (sq. km)", "Poverty index",
                  "Gini Coeff.", "GDP (constant million COP)", "GDP per capita (constant thousand COP)",
                  "Rural GDP (million COP)", "Literate index", "Avg. years of study", "Child mortality rate",
                  "Attacks agains civilian pop. (FARC)", "Attacks agains civilian pop. (ELN)",
                  "Attacks agains civilian pop. (Unkown)", "Attacks agains civilian pop. (AUC)",
                  "Demobilized", "Displaced pop. (sending)", "Displaced pop. (reception)")

# T-test diff in means (baseline covariates) for bandwidth_pp: 10pp
diffmeans_balance.ls <- lapply(list("2011" = 2011, "2015" = 2015), function(x) {
  
  # Setting character vector for the t-test diff in means (baseline covariates):
  test_vars <- names(balance.sf %>% 
                       select(-(1:5), -16, -(25:28), -(32:37)) %>% 
                       st_drop_geometry())
  
  # Writing the formulas for t-tests
  test_formulas <- paste(test_vars, " ~ polalgn_", x, sep = "") %>%
    map(as.formula) %>%
    set_names(test_vars)
  
  # Performing t-test
  balance_tests <- map_df(test_formulas, 
                          ~ broom::tidy(t.test(formula = ., 
                                               data = balance.sf %>% 
                                                 filter(get(paste("sample_", x, sep = "")) == 1 & 
                                                          abs(get(paste("margin_", x, sep = "")) <= 10)))),
                          .id = "variable") %>%
    mutate(across(c(2:3), round, 2),
           across(c(4:6), round, 3)) %>%
    select(variable, estimate1, estimate2, estimate, statistic, p.value)
  balance_tests$variable <- balance_vars
  return(balance_tests)
})

# RDD estimates for balance tests
RDD_balance.ls <- lapply(list("2011" = 2011, "2015" = 2015), function(x) {
  # Setting character vector for the t-test diff in means (baseline covariates):
  test_vars <- names(balance.sf %>%
                       select(-(1:5), -16, -(25:28), -(32:37)) %>% 
                       st_drop_geometry())
  
  rdd_formulas <- paste(test_vars, " ~ margin_", x, sep = "") %>%
    map(as.formula) %>%
    set_names(test_vars)
  
  rdd_function <- function(v) {
    sample <- RDestimate(formula = v, 
                         data = subset(balance.sf, get(paste("sample_", x, sep = "")) == 1),
                         cutpoint = 0, bw = 10, verbose = T, model = T,
                         cluster = subset(balance.sf, get(paste("sample_", x, sep = "")) == 1)$state)
    sample <- sample$model[[1]]
        # I just need the lm object for the bandwidth I specified == 5
    return(sample)
  }
  
  rdd_estimates <- map_df(rdd_formulas, 
                          ~ broom::tidy(rdd_function(.),
                          .id = "variable"))%>%
    filter(term == "Tr") %>%
    select(-c(term, std.error)) %>%
    mutate(across(c(1), round, 2),
           across(c(2:3), round, 3))
})

# Creating and exporting table
balance.kbl <- diffmeans_balance.ls[["2011"]] %>% 
  bind_cols(RDD_balance.ls[["2011"]] %>% 
              rename(rdd.estimate = estimate,
              rdd.statistic = statistic,
              rdd.pvalue = p.value)) %>%
  bind_rows(diffmeans_balance.ls[["2015"]] %>% 
              bind_cols(RDD_balance.ls[["2015"]] %>%
                          rename(rdd.estimate = estimate,
                                 rdd.statistic = statistic,
                                 rdd.pvalue = p.value))) %>%
  kbl(booktabs = T, format = "latex",
      col.names = c("Variable", "Opposition", "Aligned", "Difference", "t-stat", 
                    "p-value", "Estimate", "t-stat", "p-value")) %>%
  add_header_above(c(" " = 1, "Mean values" = 2, "Diff. in Means" = 3, "RDD estimation" = 3), bold = T) %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down"),
                font_size = 9) %>%
  pack_rows("Panel A: 2011 Local Elections", 1, 21) %>%
  pack_rows("Panel B: 2015 Local Elections", 22, 42) %>%
  row_spec(0, bold = TRUE) %>%
  footnote(general_title = "",
           general = paste("\\\\textit{Mean values for each group represent the average yearly value of the",
                           "correspondent variable during the baseline period (2000-2009).",
                           "Opposition-controlled and politically aligned groups in each panel are defined",
                           "as the result of the local elections in 2011 and 2015, respectively.",
                           "Tests of difference in means are based on a two samples Welch test using a",
                           "two-sided alternative hypothesis. RDD estimates are based on a local linear",
                           "estimation using a 10 percentage points as bandwidth on observations weigthed",
                           "using a triangular kernel.}"),
           threeparttable = T,
           escape = F) %>%
  save_kable("./Plots and tables/balance.tex", float = F)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                3. FIGURE: McCRARY DENSITY TEST + STRUCTURAL ADV.                                         ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Loading data
mun_epanel.df <- read_delim("./Data/Data - Processed/colombia_panel.csv", delim = ";") %>%
  mutate(across(ends_with("_pc"), function(x) {x*1000}))

# Importing a personalize version of DCdensity function
devtools::source_url(paste("https://gist.githubusercontent.com/mikedecr/",
                           "6ae9c63b6d28c43b068ddc0d85e8897b/raw/6b4b7425b8f1d4089",
                           "b2a34a3beadba214b195ee0/McCrary_Test.R", sep = ""))$value

# Performing McCrary Density Test
mctest_output.ls <- mccrary(mun_epanel.df["margin_pres"][mun_epanel.df["year"] == 2014 | 
                                                           mun_epanel.df["year"] == 2016], 
                            verbose = T, bw = 10, ext.out = T)
mctest.df <- bind_cols(mctest_output.ls$data %>% rename(treatment = force) ,
                       mctest_output.ls$estimates %>% select(1:3))

# Loading data for Structural advantage
mun_edata.df <- read_delim("./Data/Data - Processed/mun_edata.csv", delim = ";") %>%
  filter(edata_year<2019) %>%
  mutate(margin_victory = floor(porc_part_mesa_1-porc_part_mesa_2)) %>%
  rename(share_1 = porc_part_mesa_1,
         share_2 = porc_part_mesa_2,
         desc_1 = desc_pp_1,
         desc_2 = desc_pp_2) %>%
  select(-c(10,11)) %>%
  pivot_longer(6:9,
               names_to = c(".value", "place"),
               names_sep = "_",
               values_drop_na = T) %>%
  mutate(margin_victory = if_else(place == 2, margin_victory*-1, margin_victory),
         pres_pp = if_else(pres_pp_bwon == 1 & place == 1, 1, 
                           if_else(pres_pp_blost == 1 & place == 2, 1, 0)))

# Structural Advantage calculation
summary(rdrobust(mun_edata.df$pres_pp, mun_edata.df$margin_victory,
                 kernel = "triangular",
                 h = 10, rho = 1, c = 0, p = 1, 
                 vce = "hc2", masspoints = "off"))
stradv.ls <- rdplot(mun_edata.df$pres_pp, mun_edata.df$margin_victory,
                    binselect = "esmvpr", x.lim = c(-40, 40), p = 4,
                    ci = 95)

# Plotting results McCrary test + Structural adavantage
dist_plot.ls <- list(
  "McCrary" = ggplot(data = mctest.df, aes(x = cellmp)) +
    geom_point(aes(y = cellval), color = "#046C9A") +
    geom_line(data = mctest.df %>% filter(treatment == 0), aes(y = dhat)) +
    geom_line(data = mctest.df %>% filter(treatment == 0), aes(y = dupper), linetype = "dashed") +
    geom_line(data = mctest.df %>% filter(treatment == 0), aes(y = dlower), linetype = "dashed") +
    geom_line(data = mctest.df %>% filter(treatment == 1), aes(y = dhat)) +
    geom_line(data = mctest.df %>% filter(treatment == 1), aes(y = dupper), linetype = "dashed") +
    geom_line(data = mctest.df %>% filter(treatment == 1), aes(y = dlower), linetype = "dashed") +
    geom_vline(xintercept = 0) +
    theme_bw() +
    labs(x = "Percentage points from cut-off", y = "Density") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none",
          text = element_text(size = 16, family = "Ledger")) +
    scale_x_continuous(limits = c(-40,40),
                       breaks = seq(-40,40,10)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,0.035),
                       breaks = seq(0.005,0.035,0.01)),
  
  "Structural" = ggplot() +
    geom_point(data = stradv.ls$vars_bins, 
               aes(x = rdplot_mean_x, y = rdplot_mean_y, size = rdplot_N), 
               shape = 20, color = "#046C9A", show.legend = F) + 
    geom_line(data = stradv.ls$vars_poly %>% filter(rdplot_x < 0),
              aes(x = rdplot_x, y = rdplot_y), 
              color = "#02401B", linetype = "dashed") +
    geom_line(data = stradv.ls$vars_poly %>% filter(rdplot_x > 0),
              aes(x = rdplot_x, y = rdplot_y), 
              color = "#02401B", linetype = "dashed") +
    geom_vline(xintercept = 0, linetype = "solid") +
    theme_bw() +
    labs(x = "Percentage points from cut-off", y = "Share of candidates with elec. advantage") +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          axis.line = element_line(colour = "black"),
          legend.position = "none",
          text = element_text(size = 16, family = "Ledger")) +
    scale_x_continuous(limits = c(-40, 40)) +
    scale_y_continuous(limits = c(0, 0.5)))

# Multipanel plot
dist_plot.ls$Adds[[1]] <- ggplot() + 
  annotate(geom = 'text', x = 1, y = 1, label = "(a) McCrary test", size = 6, family = "Ledger") + 
  theme_void()
dist_plot.ls$Adds[[2]] <- ggplot() + 
  annotate(geom = 'text', x = 1, y = 1, label = "(b) Structural advantage", size = 6, family = "Ledger") + 
  theme_void()
dist_plot.ls$McCrary+ dist_plot.ls$Structural +
  dist_plot.ls$Adds[[1]] + dist_plot.ls$Adds[[2]] +
  plot_layout(guides = "collect",
              heights = c(9,1),
              widths = 8,
              ncol = 2) +
  theme(legend.position = "none",
        text = element_text(size = 18, family = "Ledger"),
        plot.title = element_text(size = 20))
ggsave("./Plots and tables/dist_plot.png", height = 7, width = 14)
  

## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                4. TABLE: YEARLY OUTCOME EVOLUTION                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Summarizing info
outcomes_evol.kbl <- mun_epanel.df %>%
  group_by(year) %>%
  select(starts_with("MFA_"), -ends_with("_PER")) %>%
  rename(MFA_TOTAL_INV = MFA_INV,
         MFA_TOTAL_FAM = MFA_FAM,
         MFA_TOTAL_pc = MFA_pc) %>%
  summarise(across(-ends_with("_pc"), sum, na.rm = T),
            across(ends_with("_pc"), mean, na.rm = T)) %>%
  pivot_longer(cols = !year, names_to = c("program", "Outcome"),
               names_pattern = "MFA_(.*)_(.*)") %>%
  pivot_wider(names_from = year, values_from = value) %>%
  mutate(program = dplyr::recode(program, "TOTAL" = 1, "SIS" = 2, "DES" = 3, "UNI" = 4),
         Outcome = dplyr::recode(Outcome, "INV" = "Transfer (million COP)", "FAM" = "Families", 
                                 "pc" = "Avg. transfer p.c. (thousand COP)")) %>%
  arrange(program) %>%
  select(-program) %>%
  kbl(booktabs = T, format = "latex", digits = 2,
      format.args = list(decimal.mark = ".", big.mark = ",")) %>%
  kable_styling(latex_options = c("HOLD_position", "scale_down"),
                font_size = 9) %>%
  row_spec(0, bold = TRUE) %>%
  pack_rows("Total MFA", 1, 3) %>%
  pack_rows("SISBEN", 4, 6) %>%
  pack_rows("Displaced", 7, 9) %>%
  pack_rows("UNIDOS", 10, 12) %>%
  footnote(general_title = "",
           general = paste("\\\\textit{Notes: The average transfer per capita is calculated by dividing the",
                           "total amount of the transfer to the municipalities by the number of families that",
                           "are registered as beneficiaries and using the size of the respective household",
                           "as weights.}"),
           threeparttable = T,
           escape = F) %>%
  save_kable("./Plots and tables/outcome_evol.tex", float = F)

  
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                5. FIGURE: RDD ESTIMATION - GLOBAL                                                        ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Plotting RD discontinuities
RDDplots_global.ls <- lapply(list("MFA" = "", "SISBEN" = "SIS_", "Displaced" = "DES_", "UNIDOS" = "UNI_"), 
                             function(prog) {
                               imap(list("Transfer" = "pc", "Families" = "FAM"), function(outcome, oname) {
                                 
                                 # Defining outcome variable
                                 outcome_var <- mun_epanel.df %>% pull(paste("MFA_", prog, outcome, sep = ""))
                                 rvar <- mun_epanel.df$margin_pres
                                 
                                 # Plotting global fit
                                 praw <- rdplot(log(outcome_var), rvar,
                                                p = 4, binselect = "esmvpr", x.lim = c(-40, 40),
                                                subset = c(!is.na(rvar) & 
                                                             outcome_var > 0 &
                                                             !is.na(outcome_var)),
                                                masspoints = "off", hide = T)
                                 
                                 plot <- ggplot() +
                                   geom_point(data = praw$vars_bins, 
                                              aes(x = rdplot_mean_x,
                                                  y = rdplot_mean_y,
                                                  size = rdplot_N), shape = 20, 
                                              color = "#046C9A", show.legend = F) + 
                                   geom_line(data = praw$vars_poly %>% filter(rdplot_x < 0),
                                             aes(x = rdplot_x, y = rdplot_y), color = "#02401B") +
                                   geom_line(data = praw$vars_poly %>% filter(rdplot_x > 0),
                                             aes(x = rdplot_x, y = rdplot_y), color = "#02401B") +
                                   geom_vline(xintercept = 0, linetype = "dashed") +
                                   geom_vline(xintercept = -10, linetype = "dotted") +
                                   geom_vline(xintercept = 10, linetype = "dotted") +
                                   theme_bw() +
                                   labs(y = "Yearly transfer (millions of colombian pesos)") +
                                   theme(panel.grid.major = element_blank(),
                                         panel.grid.minor = element_blank(),
                                         panel.border = element_blank(),
                                         axis.line = element_line(colour = "black"),
                                         legend.position = "none",
                                         text = element_text(size = 16, family = "Ledger")) +
                                   scale_x_continuous(limits = c(-40,40),
                                                      breaks = seq(-40,40,10)) +
                                   scale_y_continuous(expand = c(0, 0))
                                 if (prog == "UNI_") {
                                   plot <- plot + labs(x = "Percentage points from cut-off",
                                                       y = paste("log(", oname, ")", sep = ""))
                                 } else {
                                   plot <- plot + theme(axis.title.x = element_blank()) +
                                     labs(y = paste("log(", oname, ")", sep = ""))
                                   
                                 }
                                 return(plot)
                               })
                             })

# Multipanel plot
RDDplots_global.ls$Adds[[1]] <- ggplot() + 
  annotate(geom = 'text', x = 1, y = 1, label = "(a) Transfer per capita", size = 6, family = "Ledger") + 
  theme_void()
RDDplots_global.ls$Adds[[2]] <- ggplot() + 
  annotate(geom = 'text', x = 1, y = 1, label = "(b) Families", size = 6, family = "Ledger") + 
  theme_void()
RDDplots_global.ls$SISBEN$Transfer + RDDplots_global.ls$SISBEN$Families +
  RDDplots_global.ls$Displaced$Transfer + RDDplots_global.ls$Displaced$Families +
  RDDplots_global.ls$UNIDOS$Transfer + RDDplots_global.ls$UNIDOS$Families +
  RDDplots_global.ls$Adds[[1]] + RDDplots_global.ls$Adds[[2]] +
  plot_layout(guides = "collect",
              heights = c(9,9,9,1),
              widths = 8,
              ncol = 2) +
  plot_annotation(tag_levels = list(c("SISBEN", "",
                                      "Displaced", "",
                                      "UNIDOS", ""))) +
  theme(legend.position = "none",
        text = element_text(size = 18, family = "Ledger"),
        plot.title = element_text(size = 20))
ggsave("./Plots and tables/RDDplot_global.png", height = 14, width = 16)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                6. ESTIMATIONS PREPARATIONS                                                               ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Setting a regresion dataset
reg_panel.df <- mun_epanel.df %>%
  filter(!is.na(margin_pres)) %>%
  left_join(balance.sf %>%
              mutate(ntpobc = tpobc_FARC+tpobc_ELN+tpobc_DESC+tpobc_AUC) %>%
              select(id_dane, pib_percapita_cons, ntpobc, tpobc_FARC, tpobc_ELN, tpobc_AUC,
                     desplazados_recepcion, anos_est_mun) %>%
              st_drop_geometry(),
            by = "id_dane") %>%
  mutate(dummy = 1, d = year) %>%
  spread(key = d,
         value = dummy,
         sep = ".",
         fill = 0) %>%
  mutate(depto = as.factor(depto),
         year = as.factor(year),
         across(starts_with("d.20"), function(dyear) {dyear*pib_percapita_cons}, 
                .names = "fe{.col}_y"),
         # across(starts_with("d.20"), function(dyear) {dyear*tpobc_FARC},
         #        .names = "fe{.col}_farc"),
         # across(starts_with("d.20"), function(dyear) {dyear*tpobc_ELN},
         #        .names = "fe{.col}_eln"),
         # across(starts_with("d.20"), function(dyear) {dyear*tpobc_AUC},
         #        .names = "fe{.col}_auc"),
         across(starts_with("d.20"), function(dyear) {dyear*ntpobc},
                .names = "fe{.col}_att"),
         # across(starts_with("d.20"), function(dyear) {dyear*anos_est_mun},
         #        .names = "fe{.col}_des"),
         sample_coa = if_else((pres_pp_bwon == 1 & coa_pp_blost == 1) | 
                                (pres_pp_blost == 1 & coa_pp_bwon == 1), 0, 1)) %>%
  select(-starts_with("d.20"))

# Defining controls
fe <- cbind(factor(reg_panel.df$depto), factor(reg_panel.df$year))
controls <- factor(reg_panel.df$depto)
for (i in 1:length(reg_panel.df %>% select(starts_with("fed.20")))) {
  controls <- cbind(controls, reg_panel.df %>% select(starts_with("fed.20")) %>% pull(i))
}

# Defining info extracting functions
tidy.rdrobust <- function(model, ...) {
  tidy_data <- data.frame(term = row.names(model$coef)[1],
                          estimate = model$coef[1,1],
                          std.error = model$se[1,1],
                          z = model$z[3,1],
                          p.value = model$pv[3,1])
  row.names(tidy_data) <- NULL
  tidy_data
}

glance.rdrobust <- function(model, ...){
  glance_data <- data.frame(Observations = paste(model$N_h[1], model$N_h[2], sep = " - "),
                            Bandwidth = model$bws[1,1])
  glance_data
}

# Defining estimates exporting functions:
rdd2latex <- function(models_list = NULL, n.specs = NULL, spec.labels = NULL, spec.seq = NULL) {
  ncols <- 3+(2*n.specs)

  # Preparing panels for table
  added_panels <- lapply(list("panel_B" = c(models_list$SISBEN$INVESTMENT, 
                                            models_list$SISBEN$FAMILIES),
                              "panel_C" = c(models_list$Displaced$INVESTMENT, 
                                            models_list$Displaced$FAMILIES),
                              "panel_D" = c(models_list$UNIDOS$INVESTMENT, 
                                            models_list$UNIDOS$FAMILIES)),
                         function(models) {
                           added_panels <- modelsummary(models, statistic = "z", fmt = 3,
                                                        coef_rename=c("Conventional" = "Estimate"),
                                                        stars = c("*" = .05, "**" = .01, "***" = .001), 
                                                        output = "data.frame")[, c(2, 4:ncols)]
                           added_panels[2,1] <- ""
                           return(added_panels)
                         }) %>%
    bind_rows()
  
  # Building table
  results.df <- modelsummary(c(models_list$MFA$INVESTMENT, models_list$MFA$FAMILIES),
                             coef_rename=c("Conventional"="Estimate"), statistic = "z", fmt = 3,
                             stars = c("*" = .05, "**" = .01, "***" = .001), 
                             add_rows = added_panels, output = "data.frame") %>%
    mutate(term = if_else(statistic == "modelsummary_tmp2", "", term)) %>%
    select(-c(part, statistic)) %>% 
    rename(" " = term)
  
  # Adding bottom panel info
  bottom.panel <- as.data.frame(cbind(spec.labels, spec.seq, spec.seq))
  names(bottom.panel) <- names(results.df)
  results.df <- results.df %>%
    bind_rows(bottom.panel)
  
  # Exporting table
  colheads <- c("", paste("(", c(seq(1, ncol(results.df)-1)),")", sep = ""))
  kable <- results.df %>%
    kbl(booktabs = T, format = "latex",
        col.names = colheads) %>%
    add_header_above(c(" " = 1, "log(Avg. p.c. transfer)" = n.specs, "log(Families)" = n.specs), bold = T) %>%
    kable_styling(latex_options = c("HOLD_position"),
                  font_size = 8) %>%
    row_spec(0, align = "c", bold = T) %>%
    pack_rows("Panel A: Total investment", 1, 4) %>%
    pack_rows("Panel B: SISBEN", 5, 8) %>%
    pack_rows("Panel C: Displaced", 9, 12) %>%
    pack_rows("Panel D: UNIDOS", 13, 16) %>%
    row_spec(16, hline_after = T) %>%
    row_spec(1:(16+length(spec.labels)), align = "c")
}

# Optimal bandwidth
opbands_mse.ls <- lapply(list("Pres" = 1, "Coa" = 2), function(esample) {
  lapply(as.list(reg_panel.df %>% select(ends_with(c("_pc", "_FAM")))), function(var) {
    if (esample == 1) {
      subset.v <- c(var > 0 & !is.na(var))
    } else {
      subset.v <- c(var > 0 & !is.na(var) & reg_panel.df$sample_coa == 1)
    }
    
    rdbwselect(log(var), reg_panel.df$margin_pres,
               kernel = "tri", bwselect = "mserd", masspoints = "off",
               vce = "hc2", cluster = reg_panel.df$id_dane, subset = subset.v)
  })
})


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                7. TABLE: RDD ESTIMATIONS - MAIN REGS                                                     ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Running estimations
RDD_main.ls <- lapply(list("Pres" = 1, "Coa" = 2), function(esample) {
  lapply(list("Linear" = 1, "Quadratic" = 2), function(poly) { 
    lapply(list("MFA" = "", "SISBEN" = "SIS_", "Displaced" = "DES_", "UNIDOS" = "UNI_"), 
           function(prog) {
             lapply(list("INVESTMENT" = "pc", "FAMILIES" = "FAM"), function(outcome) {
               
               # Defining outcome variable
               outcome_var <- reg_panel.df %>% pull(paste("MFA_", prog, outcome, sep = ""))
               rvar <- reg_panel.df$margin_pres
               
               if (esample == 1) {
                 subset.v <- c(outcome_var > 0 & !is.na(outcome_var))
                 opband.mse <- opbands_mse.ls[["Pres"]][[paste("MFA_", prog, outcome, sep = "")]][["bws"]][1]
               } else {
                 subset.v <- c(outcome_var > 0 & !is.na(outcome_var) & reg_panel.df$sample_coa == 1)
                 opband.mse <- opbands_mse.ls[["Coa"]][[paste("MFA_", prog, outcome, sep = "")]][["bws"]][1]
               }
               
               # Running regression
               lapply(list("(1)" = NULL,
                           "(2)" = fe,
                           "(3)" = controls),
                      function(spec) {
                        rdd <- rdrobust(log(outcome_var), rvar,
                                        covs = spec, kernel = "triangular",
                                        h = opband.mse, rho = 1, c = 0, p = poly, 
                                        vce = "hc2", cluster =  reg_panel.df$id_dane, 
                                        masspoints = "off",
                                        subset = subset.v)
                        tidy.rdrobust(rdd)
                        glance.rdrobust(rdd)
                        return(rdd)
                      })
             })
           })
  })
})

# Exporting results
imap(list("pres" = "Pres", "coa" = "Coa"), function(esample, name1) {
  imap(list( "lin" = "Linear",  "qua" = "Quadratic"), function(poly, name2) { 
    model.list = RDD_main.ls[[esample]][[poly]]
    
    rdd2latex(models_list = model.list, n.specs = 3,
              spec.labels = c("State FE", "Time FE", "Time FE x controls"),
              spec.seq = rbind(c("No", "Yes", "Yes"),
                               c("No", "Yes", "No"),
                               c("No" , "No", "Yes"))) %>%
      footnote(general_title = "Notes: ",
               general = paste("Table displays the results of estimating the local average treatment effect",
                               "of political alignment on selected outcomes of the MFA using a RD approach.",
                               "Perfomed estimations fit a", tolower(poly), 
                               "polygon for observations within a local neighborhood.",
                               "Observations are weighted using a triangular kernel and std. errors are",
                               "adjusted using heteroskedasticity-consistent estimators clustered",
                               "at the municipality level. Robust bias-corrected confidence intervals",
                               "were calculated, for which t-stats are shown within parentheses.",
                               "$*$ p $<$ 0.05, $**$ p $<$ 0.01, $***$ p $<$ 0.001."),
               threeparttable = T, escape = F) %>%
      save_kable(paste("./Plots and tables/rddregs_", name2, name1, ".tex", sep = ""), float = F)
  })
})



## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                8. FIGURE: RDD LOCAL ESTIMATION                                                           ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Plotting estimations
RDDplots_local.ls <- lapply(list("Pres" = 1, "Coa" = 2), function(esample) {
  lapply(list("MFA" = "", "SISBEN" = "SIS_", "Displaced" = "DES_", "UNIDOS" = "UNI_"), 
         function(prog) {
           imap(list("Transfer" = "pc", "Families" = "FAM"), function(outcome, oname) {
             
             # Defining outcome and running variable + op. band 
             outcome_var <- reg_panel.df %>% pull(paste("MFA_", prog, outcome, sep = ""))
             rvar <- reg_panel.df$margin_pres
             
             if (esample == 1) {
               opband.mse <- opbands_mse.ls[["Pres"]][[paste("MFA_", prog, outcome, sep = "")]][["bws"]][1]
               subset.v <- c(abs(rvar) <= opband.mse & outcome_var > 0 & !is.na(outcome_var))
             } else {
               opband.mse <- opbands_mse.ls[["Coa"]][[paste("MFA_", prog, outcome, sep = "")]][["bws"]][1]
               subset.v <- c(abs(rvar) <= opband.mse & outcome_var > 0 & !is.na(outcome_var) & 
                               reg_panel.df$sample_coa == 1)
             }
             
             print(paste("Program =", prog,
                         "+ Outcome =", outcome,
                         "+ Sample = ", esample,
                         "& Band = ", opband.mse))
             
             # Plotting estimation
             praw.1 <- rdplot(log(outcome_var), rvar, p = 1, binselect = "esmvpr", subset = subset.v,
                              kernel = "triangular", covs = controls, x.lim = c(-opband.mse, opband.mse), 
                              masspoints = "off", hide = T)
             
             praw.2 <- rdplot(log(outcome_var), rvar, p = 2, binselect = "esmvpr", subset = subset.v,
                              kernel = "triangular", covs = controls, x.lim = c(-opband.mse, opband.mse),
                              masspoints = "off", hide = T)
             
             plot <- ggplot() +
               geom_point(data = praw.1$vars_bins, 
                          aes(x = rdplot_mean_x, y = rdplot_mean_y, size = rdplot_N), 
                          shape = 20, color = "#046C9A", show.legend = F) + 
               geom_line(data = praw.1$vars_poly %>% filter(rdplot_x < 0),
                         aes(x = rdplot_x, y = rdplot_y), 
                         color = "#02401B", linetype = "dashed") +
               geom_line(data = praw.1$vars_poly %>% filter(rdplot_x > 0),
                         aes(x = rdplot_x, y = rdplot_y), 
                         color = "#02401B", linetype = "dashed") +
               geom_line(data = praw.2$vars_poly %>% filter(rdplot_x < 0),
                         aes(x = rdplot_x, y = rdplot_y), 
                         color = "#02401B", linetype = "dotted") +
               geom_line(data = praw.2$vars_poly %>% filter(rdplot_x > 0),
                         aes(x = rdplot_x, y = rdplot_y), 
                         color = "#02401B", linetype = "dotted") +
               geom_vline(xintercept = 0, linetype = "solid") +
               theme_bw() +
               theme(panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(),
                     panel.border = element_blank(),
                     axis.line = element_line(colour = "black"),
                     legend.position = "none",
                     text = element_text(size = 16, family = "Ledger")) +
               scale_x_continuous(limits = c(-floor(opband.mse), ceiling(opband.mse)))
             if (prog == "UNI_") {
               plot <- plot + labs(x = "Percentage points from cut-off",
                                   y = paste("log(", oname, ")", sep = ""))
             } else {
               plot <- plot + theme(axis.title.x = element_blank()) +
                 labs(y = paste("log(", oname, ")", sep = ""))
               
             }
             return(plot)
           })
         })
})
  

# Exporting multipanel plots
imap(list("pres" = "Pres", "coa" = "Coa"), function(esample, oname) {
  plots.ls <- RDDplots_local.ls[[esample]]
  
  plots.ls$Adds[[1]] <- ggplot() + 
    annotate(geom = 'text', x = 1, y = 1, label = "(a) Transfer per capita", size = 6, family = "Ledger") + 
    theme_void()
  plots.ls$Adds[[2]] <- ggplot() + 
    annotate(geom = 'text', x = 1, y = 1, label = "(b) Families", size = 6, family = "Ledger") + 
    theme_void()
  plots.ls$SISBEN$Transfer      + plots.ls$SISBEN$Families     +
    plots.ls$Displaced$Transfer + plots.ls$Displaced$Families  +
    plots.ls$UNIDOS$Transfer    + plots.ls$UNIDOS$Families     +
    plots.ls$Adds[[1]]          + plots.ls$Adds[[2]]           +
    plot_layout(guides = "collect",
                heights = c(9,9,9,1),
                widths = 8,
                ncol = 2) +
    plot_annotation(tag_levels = list(c("SISBEN", "",
                                        "Displaced", "",
                                        "UNIDOS", ""))) +
    theme(legend.position = "none",
          text = element_text(size = 18, family = "Ledger"),
          plot.title = element_text(size = 20))
  ggsave(paste("./Plots and tables/RDDplot_local_", oname, ".png", sep = ""), 
         height = 14, width = 16)
})


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                9. FIGURE: ROBUSTNESS CHECKS - BANDWIDTH                                                  ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Running estimations
Rchecks_bands.ls <- 
  lapply(list("MFA" = "", "SISBEN" = "SIS_", "Displaced" = "DES_", "UNIDOS" = "UNI_"), function(prog) {
    imap(list("Transfer" = "pc", "Families" = "FAM"), 
         function(outcome, oname) {
           
           # Defining outcome and running variable
           outcome_var <- reg_panel.df %>% pull(paste("MFA_", prog, outcome, sep = ""))
           rvar <- reg_panel.df$margin_pres
           subset.v <- c(outcome_var > 0 & !is.na(outcome_var) & reg_panel.df$sample_coa == 1)
           
           # Running regression
           out <- lapply(c(7:15),
                         function(band) {
                           rdd <- rdrobust(log(outcome_var), rvar, 
                                           covs = fe,
                                           h = band, rho = 1, c = 0, p = 1,
                                           kernel = "triangular", vce ="hc2", cluster = reg_panel.df$id_dane,
                                           masspoints = "off", subset = subset.v)
                           
                           # Extracting info for data frame
                           info_rdd <- cbind(rdd[["bws"]][1,1],
                                             rdd[["coef"]][1,1],
                                             t(rdd[["ci"]][3,]))
                         }) 
           df <- as.data.frame(do.call(rbind, out))
           names(df) <- c("band", "coef", "ci_lower", "ci_upper")
           
           # Plotting results
           plot <- ggplot(df, aes(x = band, y = coef)) +
             geom_point() +
             geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, width = 0.2)) +
             geom_hline(yintercept = 0, linetype="dashed", color = "#C27D38") +
             theme_bw() +
             labs(x = "Bandwidth", y = "Coefficient") +
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none",
                   text = element_text(size = 15, family = "Ledger")) +
             scale_x_continuous(breaks = seq(7, 15, 1))
         }) 
  })

# Multipanel plot
Rchecks_bands.ls$Adds[[1]] <- ggplot() + 
  annotate(geom = 'text', x = 1, y = 1, label = "(a) Transfer per capita", size = 6, family = "Ledger") + 
  theme_void()
Rchecks_bands.ls$Adds[[2]] <- ggplot() + 
  annotate(geom = 'text', x = 1, y = 1, label = "(b) Families", size = 6, family = "Ledger") + 
  theme_void()
Rchecks_bands.ls$SISBEN$Transfer + Rchecks_bands.ls$SISBEN$Families +
  Rchecks_bands.ls$Displaced$Transfer + Rchecks_bands.ls$Displaced$Families +
  Rchecks_bands.ls$UNIDOS$Transfer + Rchecks_bands.ls$UNIDOS$Families +
  Rchecks_bands.ls$Adds[[1]] + Rchecks_bands.ls$Adds[[2]] +
  plot_layout(guides = "collect",
              heights = c(9,9,9,1),
              widths = 8,
              ncol = 2) +
  plot_annotation(tag_levels = list(c("SISBEN", "",
                                      "Displaced", "",
                                      "UNIDOS", ""))) +
  theme(legend.position = "none",
        text = element_text(size = 18, family = "Ledger"),
        plot.title = element_text(size = 20))
ggsave("./Plots and tables/Rchecks_bands.png", height = 14, width = 16)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                10. FIGURE: ROBUSTNESS CHECKS - FAKE CUT-OFF                                              ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Running estimations
Rchecks_cuts.ls <- 
  lapply(list("MFA" = "", "SISBEN" = "SIS_", "Displaced" = "DES_", "UNIDOS" = "UNI_"), function(prog) {
    imap(list("Transfer" = "pc", "Families" = "FAM"), 
         function(outcome, oname) {
           
           # Defining outcome and running variable + op. band 
           outcome_var <- reg_panel.df %>% pull(paste("MFA_", prog, outcome, sep = ""))
           opband.mse <- opbands_mse.ls[["Coa"]][[paste("MFA_", prog, outcome, sep = "")]][["bws"]][1]
           rvar <- reg_panel.df$margin_pres
           subset.v <- c(outcome_var > 0 & !is.na(outcome_var) & reg_panel.df$sample_coa == 1)
           
           # Running regression
           out <- lapply(c(-7:7),
                         function(cuts) {
                           rdd <- rdrobust(log(outcome_var), rvar, 
                                           covs = fe,
                                           h = opband.mse, rho = 1, c = cuts, p = 1, 
                                           kernel = "triangular", vce ="hc2", cluster =  reg_panel.df$id_dane,
                                           masspoints = "off", subset = subset.v)
                           
                           # Extracting info for data frame
                           info_rdd <- cbind(rdd[["c"]],
                                             rdd[["coef"]][1,1],
                                             t(rdd[["ci"]][3,]))
                         }) 
           df <- as.data.frame(do.call(rbind, out))
           names(df) <- c("cutoff", "coef", "ci_lower", "ci_upper")
           
           # Plotting results
           plot <- 
             ggplot(df, aes(x = cutoff, y = coef)) +
             geom_point() +
             geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper, width = 0.2)) +
             geom_hline(yintercept = 0, linetype="dashed", color = "#C27D38") +
             theme_bw() +
             labs(x = "Cut-off from true winning margin", y = "Coefficient") +
             theme(panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank(),
                   panel.border = element_blank(),
                   axis.line = element_line(colour = "black"),
                   legend.position = "none",
                   text = element_text(size = 15, family = "Ledger")) +
             scale_x_continuous(breaks = seq(-7, 7, 1))
         }) 
  })

# Multipanel plot
Rchecks_cuts.ls$Adds[[1]] <- ggplot() + 
  annotate(geom = 'text', x = 1, y = 1, label = "(a) Transfer per capita", size = 6, family = "Ledger") + 
  theme_void()
Rchecks_cuts.ls$Adds[[2]] <- ggplot() + 
  annotate(geom = 'text', x = 1, y = 1, label = "(b) Families", size = 6, family = "Ledger") + 
  theme_void()
Rchecks_cuts.ls$SISBEN$Transfer + Rchecks_cuts.ls$SISBEN$Families +
  Rchecks_cuts.ls$Displaced$Transfer + Rchecks_cuts.ls$Displaced$Families +
  Rchecks_cuts.ls$UNIDOS$Transfer + Rchecks_cuts.ls$UNIDOS$Families +
  Rchecks_cuts.ls$Adds[[1]] + Rchecks_cuts.ls$Adds[[2]] +
  plot_layout(guides = "collect",
              heights = c(9,9,9,1),
              widths = 8,
              ncol = 2) +
  plot_annotation(tag_levels = list(c("SISBEN", "",
                                      "Displaced", "",
                                      "UNIDOS", ""))) +
  theme(legend.position = "none",
        text = element_text(size = 18, family = "Ledger"),
        plot.title = element_text(size = 20))
ggsave("./Plots and tables/Rchecks_cuts.png", height = 14, width = 16)


## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
##
##                A. RANDOM CHECKS                                                                          ----
##
## +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# sample1 <- rdrobust(log(reg_panel.df$MFA_DES_FAM), reg_panel.df$margin_pres,
#                     covs = fe, kernel = "triangular",
#                     h = 12.9, rho = 1, c = 0, p = 2,
#                     vce = "hc2", cluster =  reg_panel.df$id_dane,
#                     masspoints = "off",
#                     subset = c(reg_panel.df$MFA_DES_FAM > 0 & !is.na(reg_panel.df$MFA_DES_FAM) &
#                                  reg_panel.df$sample_coa == 1))
# summary(sample1)
# 
# rdplot(log(reg_panel.df$MFA_DES_FAM), reg_panel.df$margin_pres, p = 2,
#        binselect = "esmvpr", kernel = "triangular", covs = fe, masspoints = "off",
#        subset = c(abs(reg_panel.df$margin_pres) <= 13.186 & reg_panel.df$MFA_DES_FAM > 0 & 
#                     !is.na(reg_panel.df$MFA_DES_FAM) & reg_panel.df$sample_coa == 1),
#        x.lim = c(-13.186 , 13.186 ))
