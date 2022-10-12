# Long-term Traffic-related Air Pollutant Exposure and Amyotrophic Lateral Sclerosis Diagnosis in Denmark: A Bayesian Hierarchical Analysis
Parks, Robbie M.; Nunez, Yanelli; Balalian, Arin A; Gibson, Elizabeth A; Hansen, Johnnie; Raaschou-Nielsen, Ole; Ketzel, Matthias; Khan, Jibran; Brandt, Jørgen; Vermeulen, Roel; Peters, Susan; Goldsmith, Jeff; Re, Diane B; Weisskopf, Marc G; Kioumourtzoglou, Marianthi-Annaa. Long-term Traffic-related Air Pollutant Exposure and Amyotrophic Lateral Sclerosis Diagnosis in Denmark: A Bayesian Hierarchical Analysis. Epidemiology: November 2022 - Volume 33 - Issue 6 - p 757-766 doi: 10.1097/EDE.0000000000001536

Code used for analysis for Parks et al. Epidemiology 2022

note: 0_00_create_folder_structure.R is in all of the files to create list of file locations.

Contents of project:

1. Data preparation (data_prep) list: (cannot be run as data cannot be shared but can be seen in knitted files)

a_01_participant_data               - preparing participant data for joining to data later\
a_02_combining_poll_hosp_data       - combines all pollution and diagnosis data into a single dataset\
a_03_windows_exposure               - create data for 1-, 5-, and 10-year exposure windows beginning 1 year pre-diagnosis\
a_04_averages                       - find overall averages of the exposure windows

2. Data exploration (data_exploration) list: (cannot be run but can be seen in knitted files)

b_01_table_1                        - Table 1 of demographic characteristics of cases and controls\
b_02_table_2                        - Table 2 pollutant summaries\
b_03_table_2_third_revision         - Table 2 pollutant summaries from third revision\
b_04_table_3                        - Table 3 pollutant summaries by covariate tertile\
b_05_figure_1                       - Figure 1 of pollutant correlations\
b_06_figure_1_first_revision        - Figure 1 from first revision\
b_07_explore_parish_ses             - Exploring parish ses to see what it looks like

3. Model running (models) list (cannot run but R-STAN output saved in Output folder):

c_00_data_loading                   - Script to load processed data files\
c_00_data_loading_non_standardised  - Script to load processed data files without standardizing exposures\
c_00_multipollutant_running_code    - multi-pollutant code running functions\
c_01_run_models                     - run full models\
c_02_run_models_no2                 - run full models but with NOX swapped out for NO2\
c_03_greenland_sensitivity_check    - run quick model check with or without Greenland residents

R-STAN models (in stan folder):

  fml101.stan - main model\
  fml102.stan - main model (fml101) with O3\
  fml103.stan - NOX single traffic pollutant + PM2.5 model\
  fml104.stan - CO single traffic pollutant + PM2.5 model\
  fml105.stan - EC single traffic pollutant + PM2.5 model\
  fml106.stan - main model (fml101) with adjusted prior on omega lkj_corr(2)\
  fml107.stan - main model (fml101) with adjusted prior on omega lkj_corr(0.5)\
  fml108.stan - main model (fml101) with adjusted prior on betas normal(0,100)\
  fml109.stan - main model (fml101) with adjusted prior on betas normal (0,1)\
  fml110.stan - main model (fml101) with adjusted prior on sigma_lambda and tau cauchy(0,100)\
  fml111.stan - main model (fml101) with adjusted prior on sigma_lambda and tau cauchy(0,1)\
  fml112.stan - main model (fml101) with adjusted prior on lambda normal(0,100)\
  fml113.stan - main model (fml101) with adjusted prior on lambda normal(0,1)\
  fml114.stan - main model (fml101) with 5,000 warm-up and 10,000 iterations\
  fml115.stan - main model (fml101) with adjusted prior on lambda normal(0,100) and 5,000 warm-up and 10,000 iterations\
  fml116.stan - main model (fml101) with natural spline for parish SES (aborted)

4. Model processing (model_processing) list:

d_00_model_processing_functions - functions to process various program results\
d_01_process_model_results - processing compelete data results\
d_02_process_model_results_no2 - processing compelete data results for models with NOX swapped out for NO2

5. Plotting model (model_plotting) results list:

e_01_figure2_and_eFigure - plot Figure 2 and eFigure\
e_02_analyse_model_fit_with_shinystan - Analyse fit R-STAN model fits with shinystan
e_03_figure2_compare_nox_no2 - compare model output estimates when swapping NOX with NO2
