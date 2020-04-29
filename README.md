# Modelling the COVID-19 pandemic in Chicago

For more information on Covid in Chicago visit the (Chicago Covid Coalition website)[https://sites.google.com/view/nu-covid19-landing-page/home?authuser=0]

## 1. File and folder structure
In general we keep code on GitHub, files on Box, and use slack to communicate with each other. 

### 1.1. Data and documents (Box) 
The Box has a one project folder  "covid-chicago" and two data folders "covid-chicago" and "covid_IDPH". 
Some folder locations to point out: 
- `covid_chicago\cms_sim` includes files related to the modelling input and output (note the project covid-chicago folder) 
- `covid_chicago\project_notes` includes a description of the simulation, a collection of shared slide decks and figures
- `covid_IDPH`  includes data files used for fitting the EMS areas and is almost daily updated
- `covid_chicago\civis` includes files related to deliverables and requests of the model 
- `covid_chicago\Plots + Graphs` from data analysis

### 1.2. Code (GitHub)
- All the code is included in this gitrepository.  
To contribute to the gitrepository, you need a git account and then fork the git repository to your personal git account. 
Then clone the repository using https://github.com/numalariamodeling/covid-chicago.git to your local machine or your home directory on quest. 
To upload code, push to the cloned repository, and create a pull request to include your changes to the main repository. 
This documentation includes a description of the files used in this gitrepository. 

### 1.3. Communication (Slack)
There are two main channels on slack one "covid-chicago" and one "w7-epidemiological modelling" 
- "covid-chicago": is used for the day to day updates on the workflow, technical details, managing modelling taks and sharing updates and issues.
-  "w7-epidemiological modelling": is used for 'higher level' communication around modelling and the data requirements from collaborators. 

## 2. Software used
- Modified SEIR model using Institute for Disease Modeling (IDM's)
[Compartmental Modeling Software (CMS)](https://idmod.org/docs/cms/index.html)
- [input](https://idmod.org/docs/cms/input-files.html) configuration file (cfg)
- [input](https://idmod.org/docs/cms/input-files.html)  model file (emodl)
- [output](https://idmod.org/docs/cms/output.html?searchText=output): trajectories.csv (optionally define prefix or suffix)

## 2.2. Compartmental model structure (emodl file)
### Simple model
The "simplemodel" includes only the basic S-E-I-R compartments. 
Go to the related [emodl file here](https://github.com/numalariamodeling/covid-chicago/blob/master/emodl/simplemodel_testing.emodl)

### Extended model
The "extendedmodel" imclides additional compartments for asymptomatics, symptomatics, hospitalization, progression to critical and deaths. In addition the detections are tracked as a sum of detected asymptomatics, symptomatics,hospitalized, critical and deaths with group specific detection rates. 
Go to the related [emodl file here](https://github.com/numalariamodeling/covid-chicago/blob/master/emodl/extendedmodel_covid.emodl)

### "Extended_cobey" model
Latest version of the model, including modifications in alignment with the Covid model developed by Sarah Cobeys Team at University of Chicago. 
Go to the related [emodl file here](https://github.com/numalariamodeling/covid-chicago/blob/master/emodl/extendedmodel_cobey.emodl)

## 3. Model types
## 3.1. Base model
Assumes one well mixed homogeneous population 

## 3.2. Age-structured model 
The "age_model" duplicates each compartment of the simple or the extended model for n age groups. To allow the age groups to get in contact with each other at different rates, the Ki (contact rate * probability of transmission) needs to be specified for a all age-combinations. 

### 3.2.1. Age groups
- Four age grouos: "0to19", "20to39", "40to59", "60to100" 
[look at the 4grp emodl here](https://github.com/numalariamodeling/covid-chicago/blob/master/age_model/emodl/extendedmodel_cobey_age_4grp.emodl)
-  Eight age groups: "0to9", "10to19", "20to29", "30to39", "40to49", "50to59", "60to69", "70to100" 
[look at the 8grp emodl here](https://github.com/numalariamodeling/covid-chicago/blob/master/age_model/emodl/extendedmodel_cobey_age_8grp.emodl)
To generate or modify the emodl files use the [age specific emmodl generator](https://github.com/numalariamodeling/covid-chicago/blob/master/age_model/emodl_generator_cobey_contact_mix.py) 

### 3.2.2. Contact matrix
The contacts between age groups were previously extracted for running an [EMOD model](https://idmod.org/documentation) from [Prem et al 2017](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1005697). [Script that extracts the contact matrix values](https://github.com/numalariamodeling/covid-chicago/blob/master/age_model/age_contact/age_matrix_reducer.py). 

## 3.3. Spatial model
The "spatial_model" uses a special syntax as described [here](https://idmod.org/docs/cms/create-spatial-model.html?searchText=spatial). 
To generate or modify the emodl files use the [locale specific emmodl generator](https://github.com/numalariamodeling/covid-chicago/blob/master/spatial_model/locale_emodl_generator_extendedModel.py )
- View the [EMS specific emodl](https://github.com/numalariamodeling/covid-chicago/blob/master/emodl/extendedmodel_cobey_locale_EMS.emodl)
- View a [simple text emodl file](https://github.com/numalariamodeling/covid-chicago/blob/master/emodl/example_locale.emodl)


## 3.4. Spatial age-structured model
A test verion is available under [emodl file](https://github.com/numalariamodeling/covid-chicago/blob/master/spatial_age_model/emodl/extendedmodel_cobey_locale_EMS_2grptest1.emodl).
To generate or modify the emodl files use the [locale-age specific emmodl generator](https://github.com/numalariamodeling/covid-chicago/blob/master/spatial_age_model/extended_cobey_age_locale_emodl_generator.py )

## 4. Running simulations and analyzing predictions

### 4.1. Run single simulations
To run a single simulation: 
- Via termianl /bat
On Windows a single simulation can be run in the terminal or via batch file (i.e. as in [here](https://github.com/numalariamodeling/covid-chicago/blob/master/runModel_testing.bat)
- Via python (including plotting)
The [run_and_plot_testing.py](https://github.com/numalariamodeling/covid-chicago/blob/master/run_and_plot_testing.py) file runs the emodl simulations and prododuces a simple plot of the observed channels. 

### 4.2.1. Run scenarios (multiple simulations)
The [extendedcobey_runScenarios.py](extendedcobey_runScenarios.py) 
- takes one emodl, 
- optionally replaces parameters if @param@ placeholders are found, 
- optionally runs for multiple samples per parameter
- combines multiple trajectories.csv files produced into a trajectoriesDat.csv, that is used for postprocessing. 

### 4.2.2. Run scenarios (generic)
[runScenarios.py](runScenarios.py) is used to run multiple simulations
given a configuration file with the parameters. The script builds off
a default configuration file [extendedcobey.yaml](extendedcobey.yaml)
and substitutes parameters with the values/functions in the
user-provided configuration file using the `@param@` placeholder. As with
[extendedcobey_runScenarios.py](extendedcobey_runScenarios.py), it combines multiple trajectories.csv files produced into a trajectoriesDat.csv, that is used for postprocessing.


#### Configuration file:
The configuration file is in YAML format and is divided into 5
blocks: `experiment_setup_parameters`,
`fixed_parameters_region_specific`, `fixed_parameters_global`,
`sampled_parameters`, `fitted_parameters`. The sampled parameters need
the sampling function as well as the arguments to pass into that
function (`function_kwargs`). Currently, only a few
sampling/calculation functions are supported. More can be added by
allowing for more libraries in `generateParameterSamples` of [runScenarios.py](runScenarios.py).

Note that the user-supplied configuration file is used to provide
*additional* or *updated* parameters from the base configutation file.

#### Inputs:
- Running location: Where the simulation is being run (either `Local`
  or `NUCLUSTER`)
- Region: The region of interest. (e.g. `EMS_1`)
- Configuration file: The configuration file with the parameters to
  use for the simulation. If a parameter is not provided, the value in
  the default configuration will be used. (e.g. [sample_experiment.yaml](sample_experiment.yaml))
- Emodl template (optional): The template emodl file to substitute in
  parameter values. The default is [extendedmodel_cobey.emodl](extendedmodel_cobey.emodl). emodl
  files are in the `./emodl` directory.
- Suffix for experiment name added as name_suffix (optional): The template emodl file to substitute in
  parameter values. The default is test_randomnumber (e.g. `20200417_EMS_10_test_rn29`)
  
### Usage examples:
- Using the default emodl template: `python runScenarios.py
  --running_location Local --region IL  --experiment_config sample_experiment.yaml`
- Using a different emodl template: `python runScenarios.py
  --running_location Local  --region IL  --experiment_config sample_experiment.yaml --emodl_template simplemodel_testing.emodl`
- Specifying experiment name suffix and changing running_location : `python runScenarios.py
  --running_location NUCLUSTER --region IL --experiment_config extendedcobey.yaml --emodl_template simplemodel_testing.emodl --name_suffix "testrun_userinitials"`
- Specifiying cms configuration file and solver:`python runScenarios.py
  --running_location Local --region IL  --experiment_config sample_experiment.yaml --emodl_template simplemodel_testing.emodl --cfg_template model_Tau.cfg`

#### Specifiy solver 
If not specified the [Tau leaping](https://idmod.org/docs/cms/tau-leaping.html) will be used as default. 
CMS configuration files are in the [cfg](https://github.com/numalariamodeling/covid-chicago/tree/master/cfg) folder. 

### 4.2.3. Define age or region specific inputs 
View [EMSscenario_submission_template.txt](https://github.com/numalariamodeling/covid-chicago/blob/master/EMSscenario_submission_template.txt) for current custom scenarios that are being used. 

#### Emodl and yaml files for parameter fitting
Varying sample parameters for all regions are included in  `extendedcobey_200428.yaml`, for fitting purposes
a separate yaml file is used with wider `extendedcobey_forFitting.yaml`

#### Region specific sample parameters (i.e. using estimates parameters per EMS regions)
-  `EMSspecific_sample_parameters.yaml`  (needs to be updated with fitted parameter estimates)

####  Age extension and age specific parameters 
- `sample_age4grp_experiment.yaml` and  `sample_age8grp_experiment.yaml`


### 4.5. Local environment setup
Use a `.env` file in the same directory as your `runScenarios` script to define paths to directories and files on your own computer.
Copy the `sample.env` file to `.env` and edit so that paths point as needed for your system.

### 4.6. Running on OS X or Linux
The CMS software is provided as a compiled Windows executable, but can be run on Unix-like systems via [`wine`](https://www.winehq.org/).
If you do not have `wine` installed on your system, you can use the provided [Dockerfile](Dockerfile), which has `wine` baked in.
To build the Docker image, run `docker build -t cms`. Set `DOCKER_IMAGE=cms` in your environment or your `.env` file to use it.

### 4.7 Running on Quest (NUCLUSTER) 
A cloned version of the git repository can be found under `/projects/p30781/covidproject/covid-chicago/`.

#### Requirements on quest 
All the modules need to be installed on the personal quest environment 
- use pip install ... in your terminal 
- install `dotenv` and `yamlordereddictloader`
`conda create --name dotenv-py37 -c conda-forge python-yamlordereddictloader python=3.7 --yes`
`source activate dotenv-py37`
`conda install -c conda-forge yamlordereddictloader`

The `source activate dotenv-py37` needs to be run before runnung the `runScenarios.py`

#### Submit job 
`cd /projects/p30781/covidproject/covid-chicago/`
`python runScenarios.py --running_location NUCLUSTER --region EMS_11 --experiment_config extendedcobey_200428.yaml --emodl_template extendedmodel_cobey.emodl --name_suffix "quest_run_<your initial>"`

The experiments will go to the _temp folder on the quest gitrepository. To avoid confusion on owner of the simulations it is recommended to include the initials in the experiment name using the name_suffix argument

Next step copy the content of the submit_runSimulations.sh (should be a simple txt file) to the terminal to run:
- `cd /projects/p30781/covidproject/covid-chicago/_temp/<exp_name>/trajectories/`
- `dos2unix runSimulations.sh`  # converts windows format to linux format
- `sbatch runSimulations.sh`  # submits the simulations as an array job , note maximum array <5000 scenarios. 

## 5 Postprocessing 

### 5.1 Visualizing results

-  [process_for_civis.py](https://github.com/numalariamodeling/covid-chicago/blob/master/plotters/process_for_civis.py)
The stem argument allows to process only experiments that contain this string in the name.
`python plotters/process_for_civis.py --stem 'test'`

- [extended_model_postprocessing.py](https://github.com/numalariamodeling/covid-chicago/blob/master/archive_not_used/extended_model_postprocessing.py) (deprecated)
Postprocessing file that calculates incidences for extended SEIR model 

- [data_comparison.py](https://github.com/numalariamodeling/covid-chicago/blob/master/plotters/data_comparison.py) 
compares the predicted number of new detected hospitalized cases, cumulative detections of hospitalized cases, total number of case hospitalizations and number of critical cases to hospital data and case reports.  

- [plot_split_by_param](https://github.com/numalariamodeling/covid-chicago/blob/master/plotters/plot_split_by_param.py)
Takes two experiment simulations and compares generates a plot comparing the trajectories of both

- [combineEMS_process_for_civis.py](https://github.com/numalariamodeling/covid-chicago/blob/master/plotters/combineEMS_process_for_civis.py)
Reads in several EMS specific simulation experiments and sums the trajectores to be representative for the total area.
Initially designed to produce one trajectoriesDat for IL for 3 defined scenarios.

- [combine_csv.py](https://github.com/numalariamodeling/covid-chicago/blob/master/plotters/combine_csv.py)
Reads in trajectoriesDat from several simulation experiments and appends the dataframe without aggregating them. 

- [EMS_combo_plotter.py](https://github.com/numalariamodeling/covid-chicago/blob/master/plotters/EMS_combo_plotter.py)
Generates a plot showing the trajectories for all EMS together.

### 5.2 Fitting to data
The starting date and intervention effect size are fixed and the transmission parameter "Ki"are fitted to the data.
[in process]

## 6. Data sources
- IDPH
- NMH
- City of Chicago
- ...

## 7. Model parameters and uncertainity
To account for uncertainity and heterogeneity in transmission and disease parameters, all the parameters are sampled from a distribution informed by literature. 

### 7.1. List of parameters
Updated list on [Box](https://northwestern.app.box.com/file/656414061983)! 

| parameter | name                                                                         | 
|-----------|------------------------------------------------------------------------------|
| Ki        | Transmission rate (contact rate * infection  probability)                    | 
| Ks        | Progression to presymtomatic ( fraction_symptomatic /  time_to_infectious))       |  
| Kl        | Progression to asymptomatic ((1 - fraction_symptomatic ) /   time_to_infectious)) | 
| dAs       | detection rate of asymptomatic infections                                    |   
| dSym      | detection rate of mild symptomatic infections                                |  
| dSys      | Detection rate of severe symptomatic infections                              | 
| Ksym      | Progression to mild symptoms                                                 |  
| Ksys      | Progression to severe status ( fraction_severe * (1 / time_to_symptoms))     | 
| Kh        | Hospitalization rate                                                         |  
| Kr_a      | Recovery rate of asymptomatic infections                                     |  
| Kr_m      | Recovery rate mild symptomatic infections                                    |  
| Kr_h      | Recovery rate of hospitalized cases                                          |  
| Kr_c      | Recovery rate of critical cases                                              |  
| Kc        | Progression to critical                                                      |  
| Km        | Deaths                                                                      |   


### 7.1. Time-varying parameters
The [time-event](https://idmod.org/docs/cms/model-file.html?searchText=time-event) option in cms allows to change a paramter at a given time point (days) (which will stay constant afterwards if not resetted using a second time-event).
Time-event are used to define reduction in the transmission rate, reflecting a decrease in contact rates due to social distancing interventions (i.e. stay-at-home order). 
The time event can also be used to reflect increasing testing rates by increasing the detection of cases (i.e. dSym and dSys for increased testing at health facilities, or dAs and dSym for contact tracing)

### 7.3. Intervention scenarios
Current scenarios include:
- Scenario 1: Stop stay-at-home order  ‘next Friday’ 
- Scenario 2: No stay-at-home (counterfactual)
- Scenario 3: Continue stay-at-home


