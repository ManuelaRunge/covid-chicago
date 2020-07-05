
## Contact tracing analysis files in R

R script that analyses contact tracing simulations.
Simulations are stored by date under the "contact_tracing" folder in simulation_output
Contact tracing simulations are specified by having varying detection and isolation parameters for As+P and/or Sym
To avoid noise of sample parameters, most simulations are run with fixed sample parameters, only varying the contact tracing specific parameters.
Per experiment design, multiple separate simulation experiments are run that results might need to be combined to be compared. 

### contactTracing_master.R
- loads directories, custom functions and navigates which analysis steps to run per experiment 

- 1 describeTrajectoriesDat.R  (optional)

- 2 get_Rt_from_contactTracingSimulations.R  (optional)

- 3 ct_estimatedRT.R (optional)

- 4 heatmap_loess_contactTracing.R (required)
	output: contour plots for each EMS (or super region) separately

- 5 combined_exp_summary_plot_IL.R  (optional)
	output: 
	- one multi-facet plot with thresholds per EMS  
	- pointrange plot with thresholds aggregated per IL and compared for varying experiments




