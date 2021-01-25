import os
import sys
import re

sys.path.append('../')
from load_paths import load_box_paths

datapath, projectpath, wdir, exe_dir, git_dir = load_box_paths()
emodl_dir = os.path.join(git_dir, 'emodl')


def write_speciesaccine(grp, expandModel=None):
    grp = str(grp)
    species_str = """
(species S::{grp} @speciesS_{grp}@)
(species As::{grp} 0)
(species E::{grp} 0)
(species As_det1::{grp} 0)
(species P::{grp} 0)
(species P_det::{grp} 0)
(species Sym::{grp} 0)
(species Sym_det2::{grp} 0)
(species Sys::{grp} 0)
(species Sys_det3::{grp} 0)
(species H1::{grp} 0)
(species H2::{grp} 0)
(species H3::{grp} 0)
(species H1_det3::{grp} 0)
(species H2_det3::{grp} 0)
(species H3_det3::{grp} 0)
(species C2::{grp} 0)
(species C3::{grp} 0)
(species C2_det3::{grp} 0)
(species C3_det3::{grp} 0)
(species D3::{grp} 0)
(species D3_det3::{grp} 0)
(species RAs::{grp} 0)
(species RAs_det1::{grp} 0)
(species RSym::{grp} 0)
(species RSym_det2::{grp} 0)
(species RH1::{grp} 0)
(species RH1_det3::{grp} 0)
(species RC2::{grp} 0)
(species RC2_det3::{grp} 0)
""".format(grp=grp)
    species_str = species_str.replace("  ", " ")

    expand_testDelay_AsSymSys_str = """
(species As_preD::{grp} 0)
(species Sym_preD::{grp} 0)
(species Sym_det2a::{grp} 0)
(species Sym_det2b::{grp} 0)
(species Sys_preD::{grp} 0)
(species Sys_det3a::{grp} 0)
(species Sys_det3b::{grp} 0)
""".format(grp=grp)

    species_str = species_str + expand_testDelay_AsSymSys_str

    return (species_str)


def write_species_vaccine(grp, expandModel=None):
    grp = str(grp)
    species_str = """
(species S_V::{grp} 0)
(species As_V::{grp} 0)
(species E_V::{grp} 0)
(species As_det1_V::{grp} 0)
(species P_V::{grp} 0)
(species P_det_V::{grp} 0)
(species Sym_V::{grp} 0)
(species Sym_det2_V::{grp} 0)
(species Sys_V::{grp} 0)
(species Sys_det3_V::{grp} 0)
(species H1_V::{grp} 0)
(species H2_V::{grp} 0)
(species H3_V::{grp} 0)
(species H1_det3_V::{grp} 0)
(species H2_det3_V::{grp} 0)
(species H3_det3_V::{grp} 0)
(species C2_V::{grp} 0)
(species C3_V::{grp} 0)
(species C2_det3_V::{grp} 0)
(species C3_det3_V::{grp} 0)
(species D3_V::{grp} 0)
(species D3_det3_V::{grp} 0)
(species RAs_V::{grp} 0)
(species RAs_det1_V::{grp} 0)
(species RSym_V::{grp} 0)
(species RSym_det2_V::{grp} 0)
(species RH1_V::{grp} 0)
(species RH1_det3_V::{grp} 0)
(species RC2_V::{grp} 0)
(species RC2_det3_V::{grp} 0)
""".format(grp=grp)
    species_str = species_str.replace("  ", " ")

    expand_testDelay_AsSymSys_str = """
(species As_preD_V::{grp} 0)
(species Sym_preD_V::{grp} 0)
(species Sym_det2a_V::{grp} 0)
(species Sym_det2b_V::{grp} 0)
(species Sys_preD_V::{grp} 0)
(species Sys_det3a_V::{grp} 0)
(species Sys_det3b_V::{grp} 0)
""".format(grp=grp)

    species_str = species_str + expand_testDelay_AsSymSys_str

    return (species_str)



## For postprocessing that splits by '_', it is easier if EMS are names EMS-1 not EMS_1
## This might change depending on the postprocessing
def sub(x):
    xout = re.sub('_', '-', str(x), count=1)
    return (xout)


def write_observe_novaccine(grp, observeLevel='primary'):
    grp = str(grp)
    grpout = sub(grp)

    observe_primary_channels_str = """
(observe susceptible_noV_{grpout} S::{grp})
(observe infected_noV_{grpout} infected_{grp})
(observe recovered_noV_{grpout} recovered_{grp})
(observe infected_cumul_noV_{grpout} infected_cumul_{grp})

(observe asymp_cumul_noV_{grpout} asymp_cumul_{grp} )
(observe asymp_det_cumul_noV_{grpout} asymp_det_cumul_{grp})
(observe symptomatic_mild_noV_{grpout} symptomatic_mild_{grp})
(observe symptomatic_severe_noV_{grpout} symptomatic_severe_{grp})
(observe symp_mild_cumul_noV_{grpout} symp_mild_cumul_{grp})
(observe symp_severe_cumul_noV_{grpout} symp_severe_cumul_{grp})
(observe symp_mild_det_cumul_noV_{grpout} symp_mild_det_cumul_{grp})
(observe symp_severe_det_cumul_noV_{grpout} symp_severe_det_cumul_{grp})

(observe hosp_det_cumul_noV_{grpout} hosp_det_cumul_{grp} )
(observe hosp_cumul_noV_{grpout} hosp_cumul_{grp})
(observe detected_cumul_noV_{grpout} detected_cumul_{grp} )

(observe crit_cumul_noV_{grpout} crit_cumul_{grp})
(observe crit_det_cumul_noV_{grpout} crit_det_cumul_{grp})
(observe death_det_cumul_noV_{grpout} death_det_cumul_{grp} )

(observe deaths_det_noV_{grpout} D3_det3::{grp})
(observe deaths_noV_{grpout} deaths_{grp})

(observe crit_det_noV_{grpout} crit_det_{grp})
(observe critical_noV_{grpout} critical_{grp})
(observe hosp_det_noV_{grpout} hosp_det_{grp})
(observe hospitalized_noV_{grpout} hospitalized_{grp})
""".format(grpout=grpout, grp=grp)

    observe_secondary_channels_str = """
(observe exposed_noV_{grpout} E::{grp})

(observe asymptomatic_det_noV_{grpout} As_det1::{grp})
(observe asymptomatic_noV_{grpout} asymptomatic_{grp})

(observe presymptomatic_noV_{grpout} presymptomatic_{grp})
(observe presymptomatic_det_noV_{grpout} P_det::{grp} )

(observe detected_noV_{grpout} detected_{grp})

(observe symptomatic_mild_det_noV_{grpout} symptomatic_mild_det_{grp})
(observe symptomatic_severe_det_noV_{grpout} symptomatic_severe_det_{grp})
(observe recovered_det_noV_{grpout} recovered_det_{grp})
""".format(grpout=grpout, grp=grp)

    observe_tertiary_channels_str = """
(observe infectious_undet_noV_{grpout} infectious_undet_{grp})
(observe infectious_det_noV_{grpout} infectious_det_{grp})
(observe infectious_det_symp_noV_{grpout} infectious_det_symp_{grp})
(observe infectious_det_AsP_noV_{grpout} infectious_det_AsP_{grp})
""".format(grpout=grpout, grp=grp)

    if observeLevel == 'primary':
        observe_str = observe_primary_channels_str
    if observeLevel == 'secondary':
        observe_str = observe_primary_channels_str + observe_secondary_channels_str
    if observeLevel == 'tertiary':
        observe_str = observe_primary_channels_str + observe_tertiary_channels_str
    if observeLevel == 'all':
        observe_str = observe_primary_channels_str + observe_secondary_channels_str + observe_tertiary_channels_str

    observe_str = observe_str.replace("  ", " ")
    return (observe_str)

def write_observe_vaccine(grp, observeLevel='primary'):
    grp = str(grp)
    grpout = sub(grp)

    observe_primary_channels_str = """
(observe susceptible_V_{grpout} S_V::{grp})
(observe infected_V_{grpout} infected_V_{grp})
(observe recovered_V_{grpout} recovered_V_{grp})
(observe infected_cumul_V_{grpout} infected_cumul_V_{grp})

(observe asymp_cumul_V_{grpout} asymp_cumul_V_{grp} )
(observe asymp_det_cumul_V_{grpout} asymp_det_cumul_V_{grp})
(observe symptomatic_mild_V_{grpout} symptomatic_mild_V_{grp})
(observe symptomatic_severe_V_{grpout} symptomatic_severe_V_{grp})
(observe symp_mild_cumul_V_{grpout} symp_mild_cumul_V_{grp})
(observe symp_severe_cumul_V_{grpout} symp_severe_cumul_V_{grp})
(observe symp_mild_det_cumul_V_{grpout} symp_mild_det_cumul_V_{grp})
(observe symp_severe_det_cumul_V_{grpout} symp_severe_det_cumul_V_{grp})

(observe hosp_det_cumul_V_{grpout} hosp_det_cumul_V_{grp} )
(observe hosp_cumul_V_{grpout} hosp_cumul_V_{grp})
(observe detected_cumul_V_{grpout} detected_cumul_V_{grp} )

(observe crit_cumul_V_{grpout} crit_cumul_V_{grp})
(observe crit_det_cumul_V_{grpout} crit_det_cumul_V_{grp})
(observe death_det_cumul_V_{grpout} death_det_cumul_V_{grp} )

(observe deaths_det_V_{grpout} D3_det3_V::{grp})
(observe deaths_V_{grpout} deaths_V_{grp})

(observe crit_det_V_{grpout} crit_det_V_{grp})
(observe critical_V_{grpout} critical_V_{grp})
(observe hosp_det_V_{grpout} hosp_det_V_{grp})
(observe hospitalized_V_{grpout} hospitalized_V_{grp})
""".format(grpout=grpout, grp=grp)

    observe_secondary_channels_str = """
(observe exposed_V_{grpout} E_V::{grp})

(observe asymptomatic_det_V_{grpout} As_det1_V::{grp})
(observe asymptomatic_V_{grpout} asymptomatic_V_{grp})

(observe presymptomatic_V_{grpout} presymptomatic_V_{grp})
(observe presymptomatic_det_V_{grpout} P_det_V::{grp} )

(observe detected_V_{grpout} detected_V_{grp})

(observe symptomatic_mild_det_V_{grpout} symptomatic_mild_det_V_{grp})
(observe symptomatic_severe_det_V_{grpout} symptomatic_severe_det_V_{grp})
(observe recovered_det_V_{grpout} recovered_det_V_{grp})
""".format(grpout=grpout, grp=grp)

    observe_tertiary_channels_str = """
(observe infectious_undet_V_{grpout} infectious_undet_V_{grp})
(observe infectious_det_V_{grpout} infectious_det_V_{grp})
(observe infectious_det_symp_V_{grpout} infectious_det_symp_V_{grp})
(observe infectious_det_AsP_V_{grpout} infectious_det_AsP_V_{grp})
""".format(grpout=grpout, grp=grp)

    if observeLevel == 'primary':
        observe_str = observe_primary_channels_str
    if observeLevel == 'secondary':
        observe_str = observe_primary_channels_str + observe_secondary_channels_str
    if observeLevel == 'tertiary':
        observe_str = observe_primary_channels_str + observe_tertiary_channels_str
    if observeLevel == 'all':
        observe_str = observe_primary_channels_str + observe_secondary_channels_str + observe_tertiary_channels_str

    observe_str = observe_str.replace("  ", " ")
    return (observe_str)

def write_observe(grp, observeLevel='primary'):
    grp = str(grp)
    grpout = sub(grp)

    observe_primary_channels_str = """
(observe susceptible_{grpout} (+ S::{grp} S_V::{grp}))
(observe infected_{grpout} (+ infected_{grp} infected_V_{grp}))
(observe recovered_{grpout} (+ recovered_{grp} recovered_V_{grp}))
(observe infected_cumul_{grpout} (+ infected_cumul_{grp} infected_cumul_V_{grp}))

(observe asymp_cumul_{grpout} (+ asymp_cumul_{grp}  asymp_cumul_V_{grp}))
(observe asymp_det_cumul_{grpout} (+ asymp_det_cumul_{grp} asymp_det_cumul_V_{grp}))
(observe symptomatic_mild_{grpout} (+ symptomatic_mild_{grp} symptomatic_mild_V_{grp}))
(observe symptomatic_severe_{grpout} (+ symptomatic_severe_{grp} symptomatic_severe_V_{grp}))
(observe symp_mild_cumul_{grpout} (+ symp_mild_cumul_{grp} symp_mild_cumul_V_{grp}))
(observe symp_severe_cumul_{grpout} (+ symp_severe_cumul_{grp} symp_severe_cumul_V_{grp}))
(observe symp_mild_det_cumul_{grpout} (+ symp_mild_det_cumul_{grp} symp_mild_det_cumul_V_{grp}))
(observe symp_severe_det_cumul_{grpout} (+ symp_severe_det_cumul_{grp} symp_severe_det_cumul_V_{grp}))

(observe hosp_det_cumul_{grpout} (+ hosp_det_cumul_{grp}  hosp_det_cumul_V_{grp}))
(observe hosp_cumul_{grpout} (+ hosp_cumul_{grp} hosp_cumul_V_{grp}))
(observe detected_cumul_{grpout} (+ detected_cumul_{grp}  detected_cumul_V_{grp}))

(observe crit_cumul_{grpout} (+ crit_cumul_{grp} crit_cumul_V_{grp}))
(observe crit_det_cumul_{grpout} (+ crit_det_cumul_{grp} crit_det_cumul_V_{grp}))
(observe death_det_cumul_{grpout} (+ death_det_cumul_{grp}  death_det_cumul_V_{grp}))

(observe deaths_det_{grpout} (+ D3_det3::{grp} D3_det3_V::{grp}))
(observe deaths_{grpout} (+ deaths_{grp} deaths_V_{grp}))

(observe crit_det_{grpout} (+ crit_det_{grp} crit_det_V_{grp}))
(observe critical_{grpout} (+ critical_{grp} critical_V_{grp}))
(observe hosp_det_{grpout} (+ hosp_det_{grp} hosp_det_V_{grp}))
(observe hospitalized_{grpout} (+ hospitalized_{grp} hospitalized_V_{grp}))
""".format(grpout=grpout, grp=grp)

    observe_secondary_channels_str = """
(observe exposed_{grpout} (+ E::{grp} E_V::{grp}))

(observe asymptomatic_det_{grpout} (+ As_det1::{grp} As_det1_V::{grp}))
(observe asymptomatic_{grpout} (+ asymptomatic_{grp} asymptomatic_V_{grp}))

(observe presymptomatic_{grpout} (+ presymptomatic_{grp} presymptomatic_V_{grp}))
(observe presymptomatic_det_{grpout} (+ P_det::{grp}  P_det_V::{grp}))

(observe detected_{grpout} (+ detected_{grp} detected_V_{grp}))

(observe symptomatic_mild_det_{grpout} (+ symptomatic_mild_det_{grp} symptomatic_mild_det_V_{grp}))
(observe symptomatic_severe_det_{grpout} (+ symptomatic_severe_det_{grp} symptomatic_severe_det_V_{grp}))
(observe recovered_det_{grpout} (+ recovered_det_{grp} recovered_det_V_{grp}))
""".format(grpout=grpout, grp=grp)

    observe_tertiary_channels_str = """
(observe infectious_undet_{grpout} (+ infectious_undet_{grp} infectious_undet_V_{grp}))
(observe infectious_det_{grpout} (+ infectious_det_{grp} infectious_det_V_{grp}))
(observe infectious_det_symp_{grpout} (+ infectious_det_symp_{grp} infectious_det_symp_V_{grp}))
(observe infectious_det_AsP_{grpout} (+ infectious_det_AsP_{grp} infectious_det_AsP_V_{grp}))
""".format(grpout=grpout, grp=grp)

    if observeLevel == 'primary':
        observe_str = observe_primary_channels_str
    if observeLevel == 'secondary':
        observe_str = observe_primary_channels_str + observe_secondary_channels_str
    if observeLevel == 'tertiary':
        observe_str = observe_primary_channels_str + observe_tertiary_channels_str
    if observeLevel == 'all':
        observe_str = observe_primary_channels_str + observe_secondary_channels_str + observe_tertiary_channels_str

    observe_str = observe_str.replace("  ", " ")
    return (observe_str)

def write_functionsaccine(grp):
    grp = str(grp)
    functions_str = """
(func presymptomatic_{grp}  (+ P::{grp} P_det::{grp}))

(func hospitalized_{grp}  (+ H1::{grp} H2::{grp} H3::{grp} H1_det3::{grp} H2_det3::{grp} H3_det3::{grp}))
(func hosp_det_{grp}  (+ H1_det3::{grp} H2_det3::{grp} H3_det3::{grp}))
(func critical_{grp} (+ C2::{grp} C3::{grp} C2_det3::{grp} C3_det3::{grp}))
(func crit_det_{grp} (+ C2_det3::{grp} C3_det3::{grp}))
(func deaths_{grp} (+ D3::{grp} D3_det3::{grp}))
(func recovered_{grp} (+ RAs::{grp} RSym::{grp} RH1::{grp} RC2::{grp} RAs_det1::{grp} RSym_det2::{grp} RH1_det3::{grp} RC2_det3::{grp}))
(func recovered_det_{grp} (+ RAs_det1::{grp} RSym_det2::{grp} RH1_det3::{grp} RC2_det3::{grp}))

(func asymp_cumul_{grp} (+ asymptomatic_{grp} RAs::{grp} RAs_det1::{grp} ))
(func asymp_det_cumul_{grp} (+ As_det1::{grp} RAs_det1::{grp}))

(func symp_mild_cumul_{grp} (+ symptomatic_mild_{grp} RSym::{grp} RSym_det2::{grp}))
(func symp_mild_det_cumul_{grp} (+ symptomatic_mild_det_{grp} RSym_det2::{grp} ))

(func symp_severe_cumul_{grp} (+ symptomatic_severe_{grp} hospitalized_{grp} critical_{grp} deaths_{grp} RH1::{grp} RC2::{grp} RH1_det3::{grp} RC2_det3::{grp}))
(func symp_severe_det_cumul_{grp} (+ symptomatic_severe_det_{grp} hosp_det_{grp} crit_det_{grp} D3_det3::{grp}  RH1_det3::{grp} RC2_det3::{grp}))

(func hosp_cumul_{grp} (+ hospitalized_{grp} critical_{grp} deaths_{grp} RH1::{grp} RC2::{grp} RH1_det3::{grp} RC2_det3::{grp}))
(func hosp_det_cumul_{grp} (+ H1_det3::{grp} H2_det3::{grp} H3_det3::{grp} C2_det3::{grp} C3_det3::{grp} D3_det3::{grp}  RH1_det3::{grp}  RC2_det3::{grp}))
(func crit_cumul_{grp} (+ deaths_{grp} critical_{grp} RC2::{grp} RC2_det3::{grp}))
(func crit_det_cumul_{grp} (+ C2_det3::{grp} C3_det3::{grp} D3_det3::{grp} RC2_det3::{grp}))
(func detected_cumul_{grp} (+ As_det1::{grp} Sym_det2::{grp} Sys_det3::{grp} H1_det3::{grp} H2_det3::{grp} C2_det3::{grp} C3_det3::{grp} RAs_det1::{grp} RSym_det2::{grp} RH1_det3::{grp} RC2_det3::{grp} D3_det3::{grp}))
(func death_det_cumul_{grp} D3_det3::{grp} )

(func infected_{grp} (+ infectious_det_{grp} infectious_undet_{grp} H1_det3::{grp} H2_det3::{grp} H3_det3::{grp} C2_det3::{grp} C3_det3::{grp}))
(func infected_det_{grp} (+ infectious_det_{grp} H1_det3::{grp} H2_det3::{grp} H3_det3::{grp} C2_det3::{grp} C3_det3::{grp}))
(func infected_cumul_{grp} (+ infected_{grp} recovered_{grp} deaths_{grp}))    
""".format(grp=grp)

    expand_base_str = """
(func asymptomatic_{grp}  (+ As::{grp} As_det1::{grp}))

(func symptomatic_mild_{grp}  (+ Sym::{grp} Sym_det2::{grp}))
(func symptomatic_mild_det_{grp}  ( Sym_det2::{grp}))

(func symptomatic_severe_{grp}  (+ Sys::{grp} Sys_det3::{grp}))
(func symptomatic_severe_det_{grp}   ( Sys_det3::{grp}))

(func detected_{grp} (+ As_det1::{grp} Sym_det2::{grp} Sys_det3::{grp} H1_det3::{grp} H2_det3::{grp} H3_det3::{grp} C2_det3::{grp} C3_det3::{grp}))

(func infectious_undet_{grp} (+ As::{grp} P::{grp} Sym::{grp} Sys::{grp} H1::{grp} H2::{grp} H3::{grp} C2::{grp} C3::{grp}))
(func infectious_det_{grp} (+ As_det1::{grp} P_det::{grp} Sym_det2::{grp} Sys_det3::{grp} ))

(func infectious_det_symp_{grp} (+ Sym_det2::{grp} Sys_det3::{grp} ))
(func infectious_det_AsP_{grp} (+ As_det1::{grp} P_det::{grp}))
""".format(grp=grp)

    expand_testDelay_AsSymSys_str = """
(func asymptomatic_{grp}  (+ As_preD::{grp} As::{grp} As_det1::{grp}))

(func symptomatic_mild_{grp}  (+ Sym::{grp} Sym_preD::{grp} Sym_det2a::{grp} Sym_det2b::{grp}))
(func symptomatic_mild_det_{grp}  (+ Sym_preD::{grp} Sym_det2a::{grp} Sym_det2b::{grp}))

(func symptomatic_severe_{grp}  (+ Sys::{grp} Sys_preD::{grp} Sys_det3a::{grp} Sys_det3b::{grp}))
(func symptomatic_severe_det_{grp}  (+ Sys_preD::{grp} Sys_det3a::{grp} Sys_det3b::{grp}))

(func detected_{grp} (+ As_det1::{grp} Sym_det2a::{grp} Sym_det2b::{grp} Sys_det3a::{grp} Sys_det3b::{grp} H1_det3::{grp} H2_det3::{grp} H3_det3::{grp} C2_det3::{grp} C3_det3::{grp}))

(func infectious_undet_{grp} (+ As_preD::{grp} As::{grp} P::{grp} Sym::{grp} Sym_preD::{grp} Sys::{grp} Sys_preD::{grp} H1::{grp} H2::{grp} H3::{grp} C2::{grp} C3::{grp}))
(func infectious_det_{grp} (+ As_det1::{grp} P_det::{grp} Sym_det2a::{grp} Sym_det2b::{grp} Sys_det3a::{grp} Sys_det3b::{grp}))

(func infectious_det_symp_{grp} (+ Sym_det2a::{grp} Sym_det2b::{grp} Sys_det3a::{grp} Sys_det3b::{grp} ))
(func infectious_det_AsP_{grp} (+ As_det1::{grp} P_det::{grp}))
""".format(grp=grp)

    functions_str = expand_testDelay_AsSymSys_str + functions_str

    functions_str = functions_str.replace("  ", " ")
    return (functions_str)


def write_functions_vaccine(grp):
    grp = str(grp)
    functions_str = """
(func presymptomatic_V_{grp}  (+ P_V::{grp} P_det_V::{grp}))

(func hospitalized_V_{grp}  (+ H1_V::{grp} H2_V::{grp} H3_V::{grp} H1_det3_V::{grp} H2_det3_V::{grp} H3_det3_V::{grp}))
(func hosp_det_V_{grp}  (+ H1_det3_V::{grp} H2_det3_V::{grp} H3_det3_V::{grp}))
(func critical_V_{grp} (+ C2_V::{grp} C3_V::{grp} C2_det3_V::{grp} C3_det3_V::{grp}))
(func crit_det_V_{grp} (+ C2_det3_V::{grp} C3_det3_V::{grp}))
(func deaths_V_{grp} (+ D3_V::{grp} D3_det3_V::{grp}))
(func recovered_V_{grp} (+ RAs_V::{grp} RSym_V::{grp} RH1_V::{grp} RC2_V::{grp} RAs_det1_V::{grp} RSym_det2_V::{grp} RH1_det3_V::{grp} RC2_det3_V::{grp}))
(func recovered_det_V_{grp} (+ RAs_det1_V::{grp} RSym_det2_V::{grp} RH1_det3_V::{grp} RC2_det3_V::{grp}))

(func asymp_cumul_V_{grp} (+ asymptomatic_V_{grp} RAs_V::{grp} RAs_det1_V::{grp} ))
(func asymp_det_cumul_V_{grp} (+ As_det1_V::{grp} RAs_det1_V::{grp}))

(func symp_mild_cumul_V_{grp} (+ symptomatic_mild_V_{grp} RSym_V::{grp} RSym_det2_V::{grp}))
(func symp_mild_det_cumul_V_{grp} (+ symptomatic_mild_det_V_{grp} RSym_det2_V::{grp} ))

(func symp_severe_cumul_V_{grp} (+ symptomatic_severe_V_{grp} hospitalized_V_{grp} critical_V_{grp} deaths_V_{grp} RH1_V::{grp} RC2_V::{grp} RH1_det3_V::{grp} RC2_det3_V::{grp}))
(func symp_severe_det_cumul_V_{grp} (+ symptomatic_severe_det_V_{grp} hosp_det_V_{grp} crit_det_V_{grp} D3_det3_V::{grp}  RH1_det3_V::{grp} RC2_det3_V::{grp}))

(func hosp_cumul_V_{grp} (+ hospitalized_V_{grp} critical_V_{grp} deaths_V_{grp} RH1_V::{grp} RC2_V::{grp} RH1_det3_V::{grp} RC2_det3_V::{grp}))
(func hosp_det_cumul_V_{grp} (+ H1_det3_V::{grp} H2_det3_V::{grp} H3_det3_V::{grp} C2_det3_V::{grp} C3_det3_V::{grp} D3_det3_V::{grp}  RH1_det3_V::{grp}  RC2_det3_V::{grp}))
(func crit_cumul_V_{grp} (+ deaths_V_{grp} critical_V_{grp} RC2_V::{grp} RC2_det3_V::{grp}))
(func crit_det_cumul_V_{grp} (+ C2_det3_V::{grp} C3_det3_V::{grp} D3_det3_V::{grp} RC2_det3_V::{grp}))
(func detected_cumul_V_{grp} (+ As_det1_V::{grp} Sym_det2_V::{grp} Sys_det3_V::{grp} H1_det3_V::{grp} H2_det3_V::{grp} C2_det3_V::{grp} C3_det3_V::{grp} RAs_det1_V::{grp} RSym_det2_V::{grp} RH1_det3_V::{grp} RC2_det3_V::{grp} D3_det3_V::{grp}))
(func death_det_cumul_V_{grp} D3_det3_V::{grp} )

(func infected_V_{grp} (+ infectious_det_V_{grp} infectious_undet_V_{grp} H1_det3_V::{grp} H2_det3_V::{grp} H3_det3_V::{grp} C2_det3_V::{grp} C3_det3_V::{grp}))
(func infected_det_V_{grp} (+ infectious_det_V_{grp} H1_det3_V::{grp} H2_det3_V::{grp} H3_det3_V::{grp} C2_det3_V::{grp} C3_det3_V::{grp}))
(func infected_cumul_V_{grp} (+ infected_V_{grp} recovered_V_{grp} deaths_V_{grp}))    
""".format(grp=grp)

    expand_base_str = """
(func asymptomatic_V_{grp}  (+ As_V::{grp} As_det1_V::{grp}))

(func symptomatic_mild_V_{grp}  (+ Sym_V::{grp} Sym_det2_V::{grp}))
(func symptomatic_mild_det_V_{grp}  ( Sym_det2_V::{grp}))

(func symptomatic_severe_V_{grp}  (+ Sys_V::{grp} Sys_det3_V::{grp}))
(func symptomatic_severe_det_V_{grp}   ( Sys_det3_V::{grp}))

(func detected_V_{grp} (+ As_det1_V::{grp} Sym_det2_V::{grp} Sys_det3_V::{grp} H1_det3_V::{grp} H2_det3_V::{grp} H3_det3_V::{grp} C2_det3_V::{grp} C3_det3_V::{grp}))

(func infectious_undet_V_{grp} (+ As_V::{grp} P_V::{grp} Sym_V::{grp} Sys_V::{grp} H1_V::{grp} H2_V::{grp} H3_V::{grp} C2_V::{grp} C3_V::{grp}))
(func infectious_det_V_{grp} (+ As_det1_V::{grp} P_det_V::{grp} Sym_det2_V::{grp} Sys_det3_V::{grp} ))

(func infectious_det_symp_V_{grp} (+ Sym_det2_V::{grp} Sys_det3_V::{grp} ))
(func infectious_det_AsP_V_{grp} (+ As_det1_V::{grp} P_det_V::{grp}))
""".format(grp=grp)

    expand_testDelay_AsSymSys_str = """
(func asymptomatic_V_{grp}  (+ As_preD_V::{grp} As_V::{grp} As_det1_V::{grp}))

(func symptomatic_mild_V_{grp}  (+ Sym_V::{grp} Sym_preD_V::{grp} Sym_det2a_V::{grp} Sym_det2b_V::{grp}))
(func symptomatic_mild_det_V_{grp}  (+ Sym_preD_V::{grp} Sym_det2a_V::{grp} Sym_det2b_V::{grp}))

(func symptomatic_severe_V_{grp}  (+ Sys_V::{grp} Sys_preD_V::{grp} Sys_det3a_V::{grp} Sys_det3b_V::{grp}))
(func symptomatic_severe_det_V_{grp}  (+ Sys_preD_V::{grp} Sys_det3a_V::{grp} Sys_det3b_V::{grp}))

(func detected_V_{grp} (+ As_det1_V::{grp} Sym_det2a_V::{grp} Sym_det2b_V::{grp} Sys_det3a_V::{grp} Sys_det3b_V::{grp} H1_det3_V::{grp} H2_det3_V::{grp} H3_det3_V::{grp} C2_det3_V::{grp} C3_det3_V::{grp}))

(func infectious_undet_V_{grp} (+ As_preD_V::{grp} As_V::{grp} P_V::{grp} Sym_V::{grp} Sym_preD_V::{grp} Sys_V::{grp} Sys_preD_V::{grp} H1_V::{grp} H2_V::{grp} H3_V::{grp} C2_V::{grp} C3_V::{grp}))
(func infectious_det_V_{grp} (+ As_det1_V::{grp} P_det_V::{grp} Sym_det2a_V::{grp} Sym_det2b_V::{grp} Sys_det3a_V::{grp} Sys_det3b_V::{grp}))

(func infectious_det_symp_V_{grp} (+ Sym_det2a_V::{grp} Sym_det2b_V::{grp} Sys_det3a_V::{grp} Sys_det3b_V::{grp} ))
(func infectious_det_AsP_V_{grp} (+ As_det1_V::{grp} P_det_V::{grp}))
""".format(grp=grp)

    functions_str = expand_testDelay_AsSymSys_str + functions_str

    functions_str = functions_str.replace("  ", " ")
    return (functions_str)

###

###
def write_paramsaccine():
    params_str = """
(param time_to_infectious @time_to_infectious@)
(param time_to_symptoms @time_to_symptoms@)
(param time_to_hospitalization @time_to_hospitalization@)
(param time_to_critical @time_to_critical@)
(param time_to_death @time_to_death@)
(param recovery_time_asymp @recovery_time_asymp@)
(param recovery_time_mild @recovery_time_mild@)
(param recovery_time_hosp @recovery_time_hosp@)
(param recovery_time_crit @recovery_time_crit@)
(param fraction_symptomatic @fraction_symptomatic@)
(param fraction_severe @fraction_severe@)
(param fraction_critical @fraction_critical@ )

(param cfr @cfr@)
(param fraction_dead (/ cfr fraction_severe))
(param fraction_hospitalized (- 1 (+ fraction_critical fraction_dead)))

(param reduced_inf_of_det_cases @reduced_inf_of_det_cases@)
(param reduced_inf_of_det_cases_ct 0)

(param d_As @d_As@)
(param d_P @d_P@)
(param d_Sys @d_Sys@)

(param Kr_a (/ 1 recovery_time_asymp))
(param Kr_m (/ 1 recovery_time_mild))
(param Kr_h (/ 1 recovery_time_hosp))
(param Kr_c (/ 1 recovery_time_crit))
(param Kl (/ (- 1 fraction_symptomatic ) time_to_infectious))
(param Ks (/ fraction_symptomatic  time_to_infectious))
(param Ksys (* fraction_severe (/ 1 time_to_symptoms)))
(param Ksym (* (- 1 fraction_severe) (/ 1 time_to_symptoms)))
(param Kc (/ 1 time_to_critical))
(param Km (/ 1 time_to_death))
"""

    expand_base_str = """
(param Kh1 (/ fraction_hospitalized time_to_hospitalization))
(param Kh2 (/ fraction_critical time_to_hospitalization ))
(param Kh3 (/ fraction_dead  time_to_hospitalization))
"""

    expand_testDelay_AsSymSys_str = """
(param Kh1 (/ fraction_hospitalized time_to_hospitalization))
(param Kh2 (/ fraction_critical time_to_hospitalization ))
(param Kh3 (/ fraction_dead  time_to_hospitalization))

(param time_D_Sys @time_to_detection_Sys@)
(param Ksys_D (/ 1 time_D_Sys))
(param Kh1_D (/ fraction_hospitalized (- time_to_hospitalization time_D_Sys)))
(param Kh2_D (/ fraction_critical (- time_to_hospitalization time_D_Sys) ))
(param Kh3_D (/ fraction_dead  (- time_to_hospitalization time_D_Sys)))

(param time_D_Sym @time_to_detection_Sym@)
(param Ksym_D (/ 1 time_D_Sym))
(param Kr_m_D (/ 1 (- recovery_time_mild time_D_Sym )))

(param time_D_As @time_to_detection_As@)
(param Kl_D (/ 1 time_D_As))
(param Kr_a_D (/ 1 (- recovery_time_asymp time_D_As )))
"""

    params_str = params_str + expand_testDelay_AsSymSys_str

    params_str = params_str.replace("  ", " ")

    return (params_str)


def write_params_vaccine():

    params_str = """
(param reduced_infectiousness 0.8)
(param reduced_disease_severity 0.5)
(param fraction_symptomatic_V (* @fraction_symptomatic@ reduced_disease_severity))
(param fraction_severe_V (* @fraction_severe@ reduced_disease_severity))
(param fraction_critical_V (* @fraction_critical@ reduced_disease_severity) )
(param fraction_dead_V (/ cfr fraction_severe_V))
(param fraction_hospitalized_V (- 1 (+ fraction_critical_V fraction_dead_V)))

(param Kl_V (/ (- 1 fraction_symptomatic_V ) time_to_infectious))
(param Ks_V (/ fraction_symptomatic_V  time_to_infectious))
(param Ksys_V (* fraction_severe_V (/ 1 time_to_symptoms)))
(param Ksym_V (* (- 1 fraction_severe_V) (/ 1 time_to_symptoms)))
"""

    expand_vaccine_str = """
(param Kv_1 0)
(time-event vaccination_start_time @vaccination_start_time@ ((Kv_1 @vacc_daily_cov@)))
"""

    expand_base_str = """
(param Kh1_V (/ fraction_hospitalized_V time_to_hospitalization))
(param Kh2_V (/ fraction_critical_V time_to_hospitalization ))
(param Kh3_V (/ fraction_dead_V  time_to_hospitalization))
"""

    expand_uniformtestDelay_str = """
(param Kh1_V (/ fraction_hospitalized_V time_to_hospitalization))
(param Kh2_V (/ fraction_critical_V time_to_hospitalization ))
(param Kh3_V (/ fraction_dead_V  time_to_hospitalization))
(param Kh1_D_V (/ fraction_hospitalized_V (- time_to_hospitalization time_D)))
(param Kh2_D_V (/ fraction_critical_V (- time_to_hospitalization time_D) ))
(param Kh3_D_V (/ fraction_dead_V  (- time_to_hospitalization time_D)))
"""

    expand_testDelay_SymSys_str = """
(param Kh1_V (/ fraction_hospitalized_V time_to_hospitalization))
(param Kh2_V (/ fraction_critical_V time_to_hospitalization ))
(param Kh3_V (/ fraction_dead_V  time_to_hospitalization))
(param Kh1_D_V (/ fraction_hospitalized_V (- time_to_hospitalization time_D_Sys)))
(param Kh2_D_V (/ fraction_critical_V (- time_to_hospitalization time_D_Sys) ))
(param Kh3_D_V (/ fraction_dead_V  (- time_to_hospitalization time_D_Sys)))
"""

    expand_testDelay_AsSymSys_str = """
(param Kh1_V (/ fraction_hospitalized_V time_to_hospitalization))
(param Kh2_V (/ fraction_critical_V time_to_hospitalization ))
(param Kh3_V (/ fraction_dead_V  time_to_hospitalization))

(param Kh1_D_V (/ fraction_hospitalized_V (- time_to_hospitalization time_D_Sys)))
(param Kh2_D_V (/ fraction_critical_V (- time_to_hospitalization time_D_Sys) ))
(param Kh3_D_V (/ fraction_dead_V  (- time_to_hospitalization time_D_Sys)))
"""

    params_str = params_str + expand_vaccine_str+ expand_testDelay_AsSymSys_str

    params_str = params_str.replace("  ", " ")

    return (params_str)



def write_Ki_timeventsaccine(grp):
    grp = str(grp)
    grpout = sub(grp)
    params_str = """
(param Ki_{grp} @Ki_{grp}@)
(observe Ki_t_{grpout} Ki_{grp})
(time-event time_infection_import @time_infection_import_{grp}@ ((As::{grp} @initialAs_{grp}@) (S::{grp} (- S::{grp} @initialAs_{grp}@))))
""".format(grpout=grpout, grp=grp)
    params_str = params_str.replace("  ", " ")

    return (params_str)

def write_Ki_timevents_vaccine(grp):
    grp = str(grp)
    grpout = sub(grp)
    params_str = """
;(param Ki_V_{grp} Ki_V_{grp})
(param Ki_V_{grp} @Ki_{grp}@)
(observe Ki_t_V_{grpout} Ki_V_{grp})
""".format(grpout=grpout, grp=grp)
    params_str = params_str.replace("  ", " ")

    return (params_str)


def write_N_population(grpList):
    stringAll = ""
    for key in grpList:
        string1 = """\n(param N_{grp} (+ @speciesS_{grp}@ @initialAs_{grp}@) )""".format(grp=key)
        stringAll = stringAll + string1

    string2 = "\n(param N_All (+ " + repeat_string_by_grp('N_', grpList) + "))"
    string3 = "\n(observe N_All N_All)"
    stringAll = stringAll + string2 + string3

    return (stringAll)

def write_N_population_vaccine(grpList):
    stringAll = ""
    for grp in grpList:
        string1 = """\n(func N_V_{grp} (+ exposed_V_{grp} infected_V_{grp} recovered_V_{grp} deaths_V_{grp}) )""".format(grp=grp)
        string2 = """\n(observe N_V_{grp} N_V_{grp}""".format(grp=grp)
        stringAll = stringAll + string1 + string2

    string3 = "\n(func N_V_All (+ " + repeat_string_by_grp('N_V_', grpList) + "))"
    string4 = "\n(observe N_V_All N_V_All)"
    stringAll = stringAll + string3 + string4

    return (stringAll)

def repeat_string_by_grp(fixedstring, grpList):
    stringAll = ""
    for grp in grpList:
        temp_string = " " + fixedstring + grp
        stringAll = stringAll + temp_string

    return stringAll


def write_All_novaccine(grpList, observeLevel='primary'):
    obs_primary_All_str = ""
    obs_primary_All_str = obs_primary_All_str + "\n(observe susceptible_noV_All (+ " + repeat_string_by_grp('S::',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe infected_noV_All (+ " + repeat_string_by_grp('infected_',grpList) +  "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe recovered_noV_All (+ " + repeat_string_by_grp('recovered_',grpList) +  "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe infected_cumul_noV_All (+ " + repeat_string_by_grp('infected_cumul_', grpList) +  "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe asymp_cumul_noV_All (+ " + repeat_string_by_grp('asymp_cumul_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe asymp_det_cumul_noV_All (+ " + repeat_string_by_grp('asymp_det_cumul_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symptomatic_mild_noV_All (+ " + repeat_string_by_grp('symptomatic_mild_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symptomatic_severe_noV_All (+ " + repeat_string_by_grp('symptomatic_severe_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_mild_cumul_noV_All (+ " + repeat_string_by_grp('symp_mild_cumul_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_severe_cumul_noV_All (+ " + repeat_string_by_grp('symp_severe_cumul_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_mild_det_cumul_noV_All (+ " + repeat_string_by_grp('symp_mild_det_cumul_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_severe_det_cumul_noV_All  (+ " + repeat_string_by_grp('symp_severe_det_cumul_', grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe hosp_det_cumul_noV_All (+ " + repeat_string_by_grp('hosp_det_cumul_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe hosp_cumul_noV_All (+ " + repeat_string_by_grp('hosp_cumul_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe detected_cumul_noV_All (+ " + repeat_string_by_grp(
        'detected_cumul_', grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe crit_cumul_noV_All (+ " + repeat_string_by_grp('crit_cumul_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe crit_det_cumul_noV_All (+ " + repeat_string_by_grp('crit_det_cumul_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe death_det_cumul_noV_All (+ " + repeat_string_by_grp('death_det_cumul_', grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe deaths_det_noV_All (+ " + repeat_string_by_grp('D3_det3::',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe deaths_noV_All (+ " + repeat_string_by_grp('deaths_',grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe crit_det_noV_All (+ " + repeat_string_by_grp('crit_det_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe critical_noV_All (+ " + repeat_string_by_grp('critical_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe hosp_det_noV_All (+ " + repeat_string_by_grp('hosp_det_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe hospitalized_noV_All (+ " + repeat_string_by_grp(
        'hospitalized_', grpList) + "))"

    obs_secondary_All_str = ""
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe exposed_noV_All (+ " + repeat_string_by_grp('E::',grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe asymptomatic_noV_All (+ " + repeat_string_by_grp('asymptomatic_', grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe asymptomatic_det_noV_All (+ " + repeat_string_by_grp('As_det1::', grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe presymptomatic_noV_All (+ " + repeat_string_by_grp('P::',grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe presymptomatic_det_noV_All (+ " + repeat_string_by_grp('P_det::', grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe detected_noV_All (+ " + repeat_string_by_grp('detected_',grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe symptomatic_mild_det_noV_All (+ " + repeat_string_by_grp('symptomatic_mild_det_', grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe symptomatic_severe_det_noV_All (+ " + repeat_string_by_grp('symptomatic_severe_det_', grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe recovered_det_noV_All (+ " + repeat_string_by_grp('recovered_det_', grpList) + "))"

    obs_tertiary_All_str = ""
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_det_noV_All (+ " + repeat_string_by_grp('infectious_det_', grpList) + "))"
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_undet_noV_All (+ " + repeat_string_by_grp('infectious_undet_', grpList) + "))"
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_det_symp_noV_All (+ " + repeat_string_by_grp('infectious_det_symp_', grpList) + "))"
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_det_AsP_noV_All (+ " + repeat_string_by_grp('infectious_det_AsP_', grpList) + "))"

    if observeLevel == 'primary':
        obs_All_str = obs_primary_All_str
    if observeLevel == 'secondary':
        obs_All_str = obs_primary_All_str + obs_secondary_All_str
    if observeLevel == 'tertiary':
        obs_All_str = obs_primary_All_str + obs_tertiary_All_str
    if observeLevel == 'all':
        obs_All_str = obs_primary_All_str + obs_secondary_All_str + obs_tertiary_All_str

    obs_All_str = obs_All_str.replace("  ", " ")
    return (obs_All_str)

def write_All_vaccine(grpList, observeLevel='primary'):
    obs_primary_All_str = ""
    obs_primary_All_str = obs_primary_All_str + "\n(observe susceptible_All_V (+ " + repeat_string_by_grp('S_V::',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe infected_All_V (+ " + repeat_string_by_grp('infected_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe recovered_All_V (+ " + repeat_string_by_grp('recovered_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe infected_cumul_All_V (+ " + repeat_string_by_grp(
        'infected_cumul_V_', grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe asymp_cumul_All_V (+ " + repeat_string_by_grp('asymp_cumul_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe asymp_det_cumul_All_V (+ " + repeat_string_by_grp(
        'asymp_det_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symptomatic_mild_All_V (+ " + repeat_string_by_grp(
        'symptomatic_mild_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symptomatic_severe_All_V (+ " + repeat_string_by_grp(
        'symptomatic_severe_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_mild_cumul_All_V (+ " + repeat_string_by_grp(
        'symp_mild_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_severe_cumul_All_V (+ " + repeat_string_by_grp(
        'symp_severe_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_mild_det_cumul_All_V (+ " + repeat_string_by_grp(
        'symp_mild_det_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_severe_det_cumul_All_V (+ " + repeat_string_by_grp(
        'symp_severe_det_cumul_V_', grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe hosp_det_cumul_All_V (+ " + repeat_string_by_grp(
        'hosp_det_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe hosp_cumul_All_V (+ " + repeat_string_by_grp('hosp_cumul_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe detected_cumul_All_V (+ " + repeat_string_by_grp(
        'detected_cumul_V_', grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe crit_cumul_All_V (+ " + repeat_string_by_grp('crit_cumul_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe crit_det_cumul_All_V (+ " + repeat_string_by_grp(
        'crit_det_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe death_det_cumul_All_V (+ " + repeat_string_by_grp(
        'death_det_cumul_V_', grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe deaths_det_All_V (+ " + repeat_string_by_grp('D3_det3_V::',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe deaths_All_V (+ " + repeat_string_by_grp('deaths_V_',grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe crit_det_All_V (+ " + repeat_string_by_grp('crit_det_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe critical_All_V (+ " + repeat_string_by_grp('critical_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe hosp_det_All_V (+ " + repeat_string_by_grp('hosp_det_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe hospitalized_All_V (+ " + repeat_string_by_grp(
        'hospitalized_V_', grpList) + "))"

    obs_secondary_All_str = ""
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe exposed_All_V (+ " + repeat_string_by_grp('E_V::',grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe asymptomatic_All_V (+ " + repeat_string_by_grp(
        'asymptomatic_V_', grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe asymptomatic_det_All_V (+ " + repeat_string_by_grp(
        'As_det1_V::', grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe presymptomatic_All_V (+ " + repeat_string_by_grp('P_V::',grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe presymptomatic_det_All_V (+ " + repeat_string_by_grp(
        'P_det_V::', grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe detected_All_V (+ " + repeat_string_by_grp('detected_V_',grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe symptomatic_mild_det_All_V (+ " + repeat_string_by_grp(
        'symptomatic_mild_det_V_', grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe symptomatic_severe_det_All_V (+ " + repeat_string_by_grp(
        'symptomatic_severe_det_V_', grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe recovered_det_All_V (+ " + repeat_string_by_grp(
        'recovered_det_V_', grpList) + "))"

    obs_tertiary_All_str = ""
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_det_All_V (+ " + repeat_string_by_grp(
        'infectious_det_V_', grpList) + "))"
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_undet_All_V (+ " + repeat_string_by_grp(
        'infectious_undet_V_', grpList) + "))"
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_det_symp_All_V (+ " + repeat_string_by_grp(
        'infectious_det_symp_V_', grpList) + "))"
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_det_AsP_All_V (+ " + repeat_string_by_grp(
        'infectious_det_AsP_V_', grpList) + "))"

    if observeLevel == 'primary':
        obs_All_str = obs_primary_All_str
    if observeLevel == 'secondary':
        obs_All_str = obs_primary_All_str + obs_secondary_All_str
    if observeLevel == 'tertiary':
        obs_All_str = obs_primary_All_str + obs_tertiary_All_str
    if observeLevel == 'all':
        obs_All_str = obs_primary_All_str + obs_secondary_All_str + obs_tertiary_All_str

    obs_All_str = obs_All_str.replace("  ", " ")
    return (obs_All_str)

def write_All(grpList, observeLevel='primary'):
    obs_primary_All_str = ""
    obs_primary_All_str = obs_primary_All_str + "\n(observe susceptible_All (+ " + repeat_string_by_grp('S::',grpList) + repeat_string_by_grp('S_V::',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe infected_All (+ " + repeat_string_by_grp('infected_',grpList) + repeat_string_by_grp('infected_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe recovered_All (+ "  + repeat_string_by_grp('recovered_',grpList) + repeat_string_by_grp('recovered_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe infected_cumul_All (+ " + repeat_string_by_grp(
        'infected_cumul_', grpList) +  repeat_string_by_grp('infected_cumul_V_', grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe asymp_cumul_All (+ " + repeat_string_by_grp('asymp_cumul_',grpList) +  repeat_string_by_grp('asymp_cumul_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe asymp_det_cumul_All (+ " + repeat_string_by_grp(
        'asymp_det_cumul_', grpList) +  repeat_string_by_grp(
        'asymp_det_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symptomatic_mild_All (+ " + repeat_string_by_grp(
        'symptomatic_mild_', grpList) + repeat_string_by_grp(
        'symptomatic_mild_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symptomatic_severe_All (+ " + repeat_string_by_grp(
        'symptomatic_severe_', grpList) + repeat_string_by_grp(
        'symptomatic_severe_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_mild_cumul_All (+ "  + repeat_string_by_grp(
        'symp_mild_cumul_', grpList) + repeat_string_by_grp(
        'symp_mild_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_severe_cumul_All (+ " + repeat_string_by_grp(
        'symp_severe_cumul_', grpList) + repeat_string_by_grp(
        'symp_severe_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_mild_det_cumul_All (+ " + repeat_string_by_grp(
        'symp_mild_det_cumul_', grpList) + repeat_string_by_grp(
        'symp_mild_det_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe symp_severe_det_cumul_All (+ " + repeat_string_by_grp(
        'symp_severe_det_cumul_', grpList) + repeat_string_by_grp(
        'symp_severe_det_cumul_V_', grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe hosp_det_cumul_All (+ " + repeat_string_by_grp(
        'hosp_det_cumul_', grpList) + repeat_string_by_grp(
        'hosp_det_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe hosp_cumul_All(+ "  + repeat_string_by_grp('hosp_cumul_',grpList) + repeat_string_by_grp('hosp_cumul_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe detected_cumul_All (+ " + repeat_string_by_grp(
        'detected_cumul_', grpList) + repeat_string_by_grp(
        'detected_cumul_V_', grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe crit_cumul_All (+ " + repeat_string_by_grp('crit_cumul_',grpList) + repeat_string_by_grp('crit_cumul_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe crit_det_cumul_All (+ " + repeat_string_by_grp(
        'crit_det_cumul_', grpList) + repeat_string_by_grp(
        'crit_det_cumul_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe death_det_cumul_All (+ " + repeat_string_by_grp(
        'death_det_cumul_', grpList) + repeat_string_by_grp(
        'death_det_cumul_V_', grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe deaths_det_All (+ " + repeat_string_by_grp('D3_det3::',grpList) + repeat_string_by_grp('D3_det3_V::',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe deaths_All (+ " + repeat_string_by_grp('deaths_',grpList) + repeat_string_by_grp('deaths_V_',grpList) + "))"

    obs_primary_All_str = obs_primary_All_str + "\n(observe crit_det_All (+ " + repeat_string_by_grp('crit_det_', grpList) + repeat_string_by_grp('crit_det_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe critical_All (+ " + repeat_string_by_grp('critical_',grpList) +  repeat_string_by_grp('critical_V_',grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe hosp_det_All (+ " + repeat_string_by_grp('hosp_det_', grpList) +  repeat_string_by_grp('hosp_det_V_', grpList) + "))"
    obs_primary_All_str = obs_primary_All_str + "\n(observe hospitalized_All (+ " + repeat_string_by_grp(
        'hospitalized_', grpList) + repeat_string_by_grp(
        'hospitalized_V_', grpList) + "))"

    obs_secondary_All_str = ""
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe exposed_All (+ " + repeat_string_by_grp('E::',grpList) +  repeat_string_by_grp('E_V::',grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe asymptomatic_All (+ " + repeat_string_by_grp(
        'asymptomatic_', grpList) + repeat_string_by_grp(
        'asymptomatic_V_', grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe asymptomatic_det_All (+ " + repeat_string_by_grp(
        'As_det1::', grpList) + repeat_string_by_grp(
        'As_det1_V::', grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe presymptomatic_All (+ " + repeat_string_by_grp('P::',grpList) + repeat_string_by_grp('P_V::',grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe presymptomatic_det_All (+ "  + repeat_string_by_grp(
        'P_det::', grpList) + repeat_string_by_grp(
        'P_det_V::', grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe detected_All (+ "  + repeat_string_by_grp('detected_',grpList) +  repeat_string_by_grp('detected_V_',grpList) + "))"

    obs_secondary_All_str = obs_secondary_All_str + "\n(observe symptomatic_mild_det_All (+ " + repeat_string_by_grp(
        'symptomatic_mild_det_', grpList) +  repeat_string_by_grp(
        'symptomatic_mild_det_V_', grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe symptomatic_severe_det_All (+ " + repeat_string_by_grp(
        'symptomatic_severe_det_', grpList) + repeat_string_by_grp(
        'symptomatic_severe_det_V_', grpList) + "))"
    obs_secondary_All_str = obs_secondary_All_str + "\n(observe recovered_det_All(+ " + repeat_string_by_grp(
        'recovered_det_', grpList) + repeat_string_by_grp(
        'recovered_det_V_', grpList) + "))"

    obs_tertiary_All_str = ""
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_det_All (+ " + repeat_string_by_grp(
        'infectious_det_', grpList) + repeat_string_by_grp(
        'infectious_det_V_', grpList) + "))"
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_undet_All (+ " + repeat_string_by_grp(
        'infectious_undet_', grpList) + repeat_string_by_grp(
        'infectious_undet_V_', grpList) + "))"
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_det_symp_All (+ " + repeat_string_by_grp(
        'infectious_det_symp_', grpList) + repeat_string_by_grp(
        'infectious_det_symp_V_', grpList) + "))"
    obs_tertiary_All_str = obs_tertiary_All_str + "\n(observe infectious_det_AsP_All (+ " + repeat_string_by_grp(
        'infectious_det_AsP_', grpList) +  repeat_string_by_grp(
        'infectious_det_AsP_V_', grpList) + "))"

    if observeLevel == 'primary':
        obs_All_str = obs_primary_All_str
    if observeLevel == 'secondary':
        obs_All_str = obs_primary_All_str + obs_secondary_All_str
    if observeLevel == 'tertiary':
        obs_All_str = obs_primary_All_str + obs_tertiary_All_str
    if observeLevel == 'all':
        obs_All_str = obs_primary_All_str + obs_secondary_All_str + obs_tertiary_All_str

    obs_All_str = obs_All_str.replace("  ", " ")
    return (obs_All_str)

def write_reactionsaccine(grp, expandModel=None):
    grp = str(grp)

    reaction_str_I = """
(reaction exposure_{grp}   (S::{grp}) (E::{grp}) (* Ki_{grp} S::{grp}  (/ (+ infectious_undet_{grp} (* infectious_undet_V_{grp}  reduced_infectiousness) (* (+ infectious_det_symp_{grp} infectious_det_symp_V_{grp}) reduced_inf_of_det_cases) (* (+ infectious_det_AsP_{grp} infectious_det_AsP_V_{grp}) reduced_inf_of_det_cases_ct)) N_{grp} )))
""".format(grp=grp)

    reaction_str_III = """
(reaction recovery_H1_{grp}   (H1::{grp})   (RH1::{grp})   (* Kr_h H1::{grp}))
(reaction recovery_C2_{grp}   (C2::{grp})   (RC2::{grp})   (* Kr_c C2::{grp}))
(reaction recovery_H1_det3_{grp}   (H1_det3::{grp})   (RH1_det3::{grp})   (* Kr_h H1_det3::{grp}))
(reaction recovery_C2_det3_{grp}   (C2_det3::{grp})   (RC2_det3::{grp})   (* Kr_c C2_det3::{grp}))
    """.format(grp=grp)

    expand_base_str = """
(reaction infection_asymp_undet_{grp}  (E::{grp})   (As::{grp})   (* Kl E::{grp} (- 1 d_As)))
(reaction infection_asymp_det_{grp}  (E::{grp})   (As_det1::{grp})   (* Kl E::{grp} d_As))
(reaction presymptomatic_{grp} (E::{grp})   (P::{grp})   (* Ks E::{grp} (- 1 d_P)))
(reaction presymptomatic_{grp} (E::{grp})   (P_det::{grp})   (* Ks E::{grp} d_P))

(reaction mild_symptomatic_undet_{grp} (P::{grp})  (Sym::{grp}) (* Ksym P::{grp} (- 1 d_Sym_{grp})))
(reaction mild_symptomatic_det_{grp} (P::{grp})  (Sym_det2::{grp}) (* Ksym P::{grp} d_Sym_{grp}))
(reaction severe_symptomatic_undet_{grp} (P::{grp})  (Sys::{grp})  (* Ksys P::{grp} (- 1 d_Sys)))
(reaction severe_symptomatic_det_{grp} (P::{grp})  (Sys_det3::{grp})  (* Ksys P::{grp} d_Sys))

(reaction mild_symptomatic_det_{grp} (P_det::{grp})  (Sym_det2::{grp}) (* Ksym P_det::{grp}))
(reaction severe_symptomatic_det_{grp} (P_det::{grp})  (Sys_det3::{grp})  (* Ksys P_det::{grp} ))

(reaction hospitalization_1_{grp}   (Sys::{grp})   (H1::{grp})   (* Kh1 Sys::{grp}))
(reaction hospitalization_2_{grp}   (Sys::{grp})   (H2::{grp})   (* Kh2 Sys::{grp}))
(reaction hospitalization_3_{grp}   (Sys::{grp})   (H3::{grp})   (* Kh3 Sys::{grp}))
(reaction critical_2_{grp}   (H2::{grp})   (C2::{grp})   (* Kc H2::{grp}))
(reaction critical_3_{grp}   (H3::{grp})   (C3::{grp})   (* Kc H3::{grp}))
(reaction death_{grp}   (C3::{grp})   (D3::{grp})   (* Km C3::{grp}))

(reaction hospitalization_1_det_{grp}   (Sys_det3::{grp})   (H1_det3::{grp})   (* Kh1 Sys_det3::{grp}))
(reaction hospitalization_2_det_{grp}   (Sys_det3::{grp})   (H2_det3::{grp})   (* Kh2 Sys_det3::{grp}))
(reaction hospitalization_3_det_{grp}   (Sys_det3::{grp})   (H3_det3::{grp})   (* Kh3 Sys_det3::{grp}))
(reaction critical_2_det2_{grp}   (H2_det3::{grp})   (C2_det3::{grp})   (* Kc H2_det3::{grp}))
(reaction critical_3_det2_{grp}   (H3_det3::{grp})   (C3_det3::{grp})   (* Kc H3_det3::{grp}))
(reaction death_det3_{grp}   (C3_det3::{grp})   (D3_det3::{grp})   (* Km C3_det3::{grp}))

(reaction recovery_As_{grp}   (As::{grp})   (RAs::{grp})   (* Kr_a As::{grp}))
(reaction recovery_As_det_{grp} (As_det1::{grp})   (RAs_det1::{grp})   (* Kr_a As_det1::{grp}))

(reaction recovery_Sym_{grp}   (Sym::{grp})   (RSym::{grp})   (* Kr_m  Sym::{grp}))
(reaction recovery_Sym_det2_{grp}   (Sym_det2::{grp})   (RSym_det2::{grp})   (* Kr_m  Sym_det2::{grp}))
""".format(grp=grp)


    expand_testDelay_AsSymSys_str = """
(reaction infection_asymp_det_{grp}  (E::{grp})   (As_preD::{grp})   (* Kl E::{grp}))
(reaction infection_asymp_undet_{grp}  (As_preD::{grp})   (As::{grp})   (* Kl_D As_preD::{grp} (- 1 d_As)))
(reaction infection_asymp_det_{grp}  (As_preD::{grp})   (As_det1::{grp})   (* Kl_D As_preD::{grp} d_As))

(reaction presymptomatic_{grp} (E::{grp})   (P::{grp})   (* Ks  E::{grp} (- 1 d_P)))
(reaction presymptomatic_{grp} (E::{grp})   (P_det::{grp})   (* Ks  E::{grp} d_P))

; developing symptoms - same time to symptoms as in master emodl
(reaction mild_symptomatic_{grp} (P::{grp})  (Sym_preD::{grp}) (* Ksym P::{grp}))
(reaction severe_symptomatic_{grp} (P::{grp})  (Sys_preD::{grp})  (* Ksys P::{grp}))

; never detected 
(reaction mild_symptomatic_undet_{grp} (Sym_preD::{grp})  (Sym::{grp}) (* Ksym_D Sym_preD::{grp} (- 1 d_Sym_{grp})))
(reaction severe_symptomatic_undet_{grp} (Sys_preD::{grp})  (Sys::{grp})  (* Ksys_D Sys_preD::{grp} (- 1 d_Sys)))

; new detections  - time to detection is subtracted from hospital time
(reaction mild_symptomatic_det_{grp} (Sym_preD::{grp})  (Sym_det2a::{grp}) (* Ksym_D Sym_preD::{grp} d_Sym_{grp}))
(reaction severe_symptomatic_det_{grp} (Sys_preD::{grp})  (Sys_det3a::{grp})  (* Ksys_D Sys_preD::{grp} d_Sys))

; developing symptoms - already detected, same time to symptoms as in master emodl
(reaction mild_symptomatic_det_{grp} (P_det::{grp})  (Sym_det2b::{grp}) (* Ksym  P_det::{grp}))
(reaction severe_symptomatic_det_{grp} (P_det::{grp})  (Sys_det3b::{grp})  (* Ksys  P_det::{grp} ))

(reaction hospitalization_1_{grp}  (Sys::{grp})   (H1::{grp})   (* Kh1_D Sys::{grp}))
(reaction hospitalization_2_{grp}   (Sys::{grp})   (H2::{grp})   (* Kh2_D Sys::{grp}))
(reaction hospitalization_3_{grp}   (Sys::{grp})   (H3::{grp})   (* Kh3_D Sys::{grp}))
(reaction critical_2_{grp}  (H2::{grp})   (C2::{grp})   (* Kc H2::{grp}))
(reaction critical_3_{grp}   (H3::{grp})   (C3::{grp})   (* Kc H3::{grp}))
(reaction death_{grp}   (C3::{grp})   (D3::{grp})   (* Km C3::{grp}))

(reaction hospitalization_1_det_{grp}   (Sys_det3a::{grp})   (H1_det3::{grp})   (* Kh1_D Sys_det3a::{grp}))
(reaction hospitalization_2_det_{grp}   (Sys_det3a::{grp})   (H2_det3::{grp})   (* Kh2_D Sys_det3a::{grp}))
(reaction hospitalization_3_det_{grp}   (Sys_det3a::{grp})   (H3_det3::{grp})   (* Kh3_D Sys_det3a::{grp}))

(reaction hospitalization_1_det_{grp}   (Sys_det3b::{grp})   (H1_det3::{grp})   (* Kh1 Sys_det3b::{grp}))
(reaction hospitalization_2_det_{grp}   (Sys_det3b::{grp})   (H2_det3::{grp})   (* Kh2 Sys_det3b::{grp}))
(reaction hospitalization_3_det_{grp}   (Sys_det3b::{grp})   (H3_det3::{grp})   (* Kh3 Sys_det3b::{grp}))

(reaction critical_2_det2_{grp}   (H2_det3::{grp})   (C2_det3::{grp})   (* Kc H2_det3::{grp}))
(reaction critical_3_det2_{grp}   (H3_det3::{grp})   (C3_det3::{grp})   (* Kc H3_det3::{grp}))
(reaction death_det3_{grp}   (C3_det3::{grp})   (D3_det3::{grp})   (* Km C3_det3::{grp}))

(reaction recovery_As_{grp}   (As::{grp})   (RAs::{grp})   (* Kr_a_D As::{grp}))
(reaction recovery_As_det_{grp} (As_det1::{grp})   (RAs_det1::{grp})   (* Kr_a_D As_det1::{grp}))

(reaction recovery_Sym_{grp}   (Sym::{grp})   (RSym::{grp})   (* Kr_m_D  Sym::{grp}))
(reaction recovery_Sym_det2a_{grp}   (Sym_det2a::{grp})   (RSym_det2::{grp})   (* Kr_m_D  Sym_det2a::{grp}))
(reaction recovery_Sym_det2b_{grp}   (Sym_det2b::{grp})   (RSym_det2::{grp})   (* Kr_m  Sym_det2b::{grp}))
 """.format(grp=grp)

    if expandModel == None:
        reaction_str = reaction_str_I + expand_base_str + reaction_str_III
    if expandModel == "testDelay_SymSys" or expandModel == "uniformtestDelay":
        reaction_str = reaction_str_I + expand_testDelay_SymSys_str + reaction_str_III
    if expandModel == 'testDelay_AsSymSys':
        reaction_str = reaction_str_I + expand_testDelay_AsSymSys_str + reaction_str_III

    reaction_str = reaction_str.replace("  ", " ")

    return (reaction_str)


def write_reactions_vaccine(grp, expandModel=None):
    grp = str(grp)

    reaction_str_I = """
(reaction vaccination_{grp}   (S::{grp}) (S_V::{grp}) (* Kv_1 S::{grp} ))
(reaction exposure_V_{grp}   (S_V::{grp}) (E_V::{grp}) (* Ki_{grp} S_V::{grp} (/ (+ infectious_undet_{grp} (* infectious_undet_V_{grp} reduced_infectiousness) (* (+ infectious_det_symp_{grp} infectious_det_symp_V_{grp}) reduced_inf_of_det_cases) (* (+ infectious_det_AsP_{grp} infectious_det_AsP_V_{grp}) reduced_inf_of_det_cases_ct)) N_{grp} )))
""".format(grp=grp)

    reaction_str_III = """
(reaction recovery_H1_V_{grp}   (H1_V::{grp})   (RH1_V::{grp})   (* Kr_h H1_V::{grp}))
(reaction recovery_C2_V_{grp}   (C2_V::{grp})   (RC2_V::{grp})   (* Kr_c C2_V::{grp}))
(reaction recovery_H1_det3_V_{grp}   (H1_det3_V::{grp})   (RH1_det3_V::{grp})   (* Kr_h H1_det3_V::{grp}))
(reaction recovery_C2_det3_V_{grp}   (C2_det3_V::{grp})   (RC2_det3_V::{grp})   (* Kr_c C2_det3_V::{grp}))
    """.format(grp=grp)

    expand_base_str = """
(reaction infection_asymp_undet_V_{grp}  (E_V::{grp})   (As_V::{grp})   (* Kl E_V::{grp} (- 1 d_As)))
(reaction infection_asymp_det_V_{grp}  (E_V::{grp})   (As_det1_V::{grp})   (* Kl E_V::{grp} d_As))
(reaction presymptomatic_V_{grp} (E_V::{grp})   (P_V::{grp})   (* Ks_V E_V::{grp} (- 1 d_P)))
(reaction presymptomatic_V_{grp} (E_V::{grp})   (P_det_V::{grp})   (* Ks_V E_V::{grp} d_P))

(reaction mild_symptomatic_undet_V_{grp} (P_V::{grp})  (Sym_V::{grp}) (* Ksym_V P_V::{grp} (- 1 d_Sym_{grp})))
(reaction mild_symptomatic_det_V_{grp} (P_V::{grp})  (Sym_det2_V::{grp}) (* Ksym_V P_V::{grp} d_Sym_{grp}))
(reaction severe_symptomatic_undet_V_{grp} (P_V::{grp})  (Sys_V::{grp})  (* Ksys_V P_V::{grp} (- 1 d_Sys)))
(reaction severe_symptomatic_det_V_{grp} (P_V::{grp})  (Sys_det3_V::{grp})  (* Ksys_V P_V::{grp} d_Sys))

(reaction mild_symptomatic_det_V_{grp} (P_det_V::{grp})  (Sym_det2_V::{grp}) (* Ksym_V P_det_V::{grp}))
(reaction severe_symptomatic_det_V_{grp} (P_det_V::{grp})  (Sys_det3_V::{grp})  (* Ksys_V P_det_V::{grp} ))

(reaction hospitalization_1_V_{grp}   (Sys_V::{grp})   (H1_V::{grp})   (* Kh1_V Sys_V::{grp}))
(reaction hospitalization_2_V_{grp}   (Sys_V::{grp})   (H2_V::{grp})   (* Kh2_V Sys_V::{grp}))
(reaction hospitalization_3_V_{grp}   (Sys_V::{grp})   (H3_V::{grp})   (* Kh3_V Sys_V::{grp}))
(reaction critical_2_V_{grp}   (H2_V::{grp})   (C2_V::{grp})   (* Kc H2_V::{grp}))
(reaction critical_3_V_{grp}   (H3_V::{grp})   (C3_V::{grp})   (* Kc H3_V::{grp}))
(reaction death_V_{grp}   (C3_V::{grp})   (D3_V::{grp})   (* Km C3_V::{grp}))

(reaction hospitalization_1_det_V_{grp}   (Sys_det3_V::{grp})   (H1_det3_V::{grp})   (* Kh1_V Sys_det3_V::{grp}))
(reaction hospitalization_2_det_V_{grp}   (Sys_det3_V::{grp})   (H2_det3_V::{grp})   (* Kh2_V Sys_det3_V::{grp}))
(reaction hospitalization_3_det_V_{grp}   (Sys_det3_V::{grp})   (H3_det3_V::{grp})   (* Kh3_V Sys_det3_V::{grp}))
(reaction critical_2_det2_V_{grp}   (H2_det3_V::{grp})   (C2_det3_V::{grp})   (* Kc H2_det3_V::{grp}))
(reaction critical_3_det2_V_{grp}   (H3_det3_V::{grp})   (C3_det3_V::{grp})   (* Kc H3_det3_V::{grp}))
(reaction death_det3_V_{grp}   (C3_det3_V::{grp})   (D3_det3_V::{grp})   (* Km C3_det3_V::{grp}))

(reaction recovery_As_V_{grp}   (As_V::{grp})   (RAs_V::{grp})   (* Kr_a As_V::{grp}))
(reaction recovery_As_det_V_{grp} (As_det1_V::{grp})   (RAs_det1_V::{grp})   (* Kr_a As_det1_V::{grp}))

(reaction recovery_Sym_V_{grp}   (Sym_V::{grp})   (RSym_V::{grp})   (* Kr_m  Sym_V::{grp}))
(reaction recovery_Sym_det2_V_{grp}   (Sym_det2_V::{grp})   (RSym_det2_V::{grp})   (* Kr_m  Sym_det2_V::{grp}))
""".format(grp=grp)


    expand_testDelay_AsSymSys_str = """
(reaction infection_asymp_det_V_{grp}  (E_V::{grp})   (As_preD_V::{grp})   (* Kl E_V::{grp}))
(reaction infection_asymp_undet_V_{grp}  (As_preD_V::{grp})   (As_V::{grp})   (* Kl_D As_preD_V::{grp} (- 1 d_As)))
(reaction infection_asymp_det_V_{grp}  (As_preD_V::{grp})   (As_det1_V::{grp})   (* Kl_D As_preD_V::{grp} d_As))

(reaction presymptomatic_V_{grp} (E_V::{grp})   (P_V::{grp})   (* Ks_V  E_V::{grp} (- 1 d_P)))
(reaction presymptomatic_V_{grp} (E_V::{grp})   (P_det_V::{grp})   (* Ks_V  E_V::{grp} d_P))

; developing symptoms - same time to symptoms as in master emodl
(reaction mild_symptomatic_V_{grp} (P_V::{grp})  (Sym_preD_V::{grp}) (* Ksym_V P_V::{grp}))
(reaction severe_symptomatic_V_{grp} (P_V::{grp})  (Sys_preD_V::{grp})  (* Ksys_V P_V::{grp}))

; never detected 
(reaction mild_symptomatic_undet_V_{grp} (Sym_preD_V::{grp})  (Sym_V::{grp}) (* Ksym_D Sym_preD_V::{grp} (- 1 d_Sym_{grp})))
(reaction severe_symptomatic_undet_V_{grp} (Sys_preD_V::{grp})  (Sys_V::{grp})  (* Ksys_D Sys_preD_V::{grp} (- 1 d_Sys)))

; new detections  - time to detection is subtracted from hospital time
(reaction mild_symptomatic_det_V_{grp} (Sym_preD_V::{grp})  (Sym_det2a_V::{grp}) (* Ksym_D Sym_preD_V::{grp} d_Sym_{grp}))
(reaction severe_symptomatic_det_V_{grp} (Sys_preD_V::{grp})  (Sys_det3a_V::{grp})  (* Ksys_D Sys_preD_V::{grp} d_Sys))

; developing symptoms - already detected, same time to symptoms as in master emodl
(reaction mild_symptomatic_det_V_{grp} (P_det_V::{grp})  (Sym_det2b_V::{grp}) (* Ksym_V  P_det_V::{grp}))
(reaction severe_symptomatic_det_V_{grp} (P_det_V::{grp})  (Sys_det3b_V::{grp})  (* Ksys_V  P_det_V::{grp} ))

(reaction hospitalization_1_V_{grp}  (Sys_V::{grp})   (H1_V::{grp})   (* Kh1_D_V Sys_V::{grp}))
(reaction hospitalization_2_V_{grp}   (Sys_V::{grp})   (H2_V::{grp})   (* Kh2_D_V Sys_V::{grp}))
(reaction hospitalization_3_V_{grp}   (Sys_V::{grp})   (H3_V::{grp})   (* Kh3_D_V Sys_V::{grp}))
(reaction critical_2_V_{grp}  (H2_V::{grp})   (C2_V::{grp})   (* Kc H2_V::{grp}))
(reaction critical_3_V_{grp}   (H3_V::{grp})   (C3_V::{grp})   (* Kc H3_V::{grp}))
(reaction death_V_{grp}   (C3_V::{grp})   (D3_V::{grp})   (* Km C3_V::{grp}))

(reaction hospitalization_1_det_V_{grp}   (Sys_det3a_V::{grp})   (H1_det3_V::{grp})   (* Kh1_D_V Sys_det3a_V::{grp}))
(reaction hospitalization_2_det_V_{grp}   (Sys_det3a_V::{grp})   (H2_det3_V::{grp})   (* Kh2_D_V Sys_det3a_V::{grp}))
(reaction hospitalization_3_det_V_{grp}   (Sys_det3a_V::{grp})   (H3_det3_V::{grp})   (* Kh3_D_V Sys_det3a_V::{grp}))

(reaction hospitalization_1_det_V_{grp}   (Sys_det3b_V::{grp})   (H1_det3_V::{grp})   (* Kh1_V Sys_det3b_V::{grp}))
(reaction hospitalization_2_det_V_{grp}   (Sys_det3b_V::{grp})   (H2_det3_V::{grp})   (* Kh2_V Sys_det3b_V::{grp}))
(reaction hospitalization_3_det_V_{grp}   (Sys_det3b_V::{grp})   (H3_det3_V::{grp})   (* Kh3_V Sys_det3b_V::{grp}))

(reaction critical_2_det2_V_{grp}   (H2_det3_V::{grp})   (C2_det3_V::{grp})   (* Kc H2_det3_V::{grp}))
(reaction critical_3_det2_V_{grp}   (H3_det3_V::{grp})   (C3_det3_V::{grp})   (* Kc H3_det3_V::{grp}))
(reaction death_det3_V_{grp}   (C3_det3_V::{grp})   (D3_det3_V::{grp})   (* Km C3_det3_V::{grp}))

(reaction recovery_As_V_{grp}   (As_V::{grp})   (RAs_V::{grp})   (* Kr_a_D As_V::{grp}))
(reaction recovery_As_det_V_{grp} (As_det1_V::{grp})   (RAs_det1_V::{grp})   (* Kr_a_D As_det1_V::{grp}))

(reaction recovery_Sym_V_{grp}   (Sym_V::{grp})   (RSym_V::{grp})   (* Kr_m_D  Sym_V::{grp}))
(reaction recovery_Sym_det2a_V_{grp}   (Sym_det2a_V::{grp})   (RSym_det2_V::{grp})   (* Kr_m_D  Sym_det2a_V::{grp}))
(reaction recovery_Sym_det2b_V_{grp}   (Sym_det2b_V::{grp})   (RSym_det2_V::{grp})   (* Kr_m  Sym_det2b_V::{grp}))
 """.format(grp=grp)

    if expandModel == None:
        reaction_str = reaction_str_I + expand_base_str + reaction_str_III
    if expandModel == "testDelay_SymSys" or expandModel == "uniformtestDelay":
        reaction_str = reaction_str_I + expand_testDelay_SymSys_str + reaction_str_III
    if expandModel == 'testDelay_AsSymSys':
        reaction_str = reaction_str_I + expand_testDelay_AsSymSys_str + reaction_str_III

    reaction_str = reaction_str.replace("  ", " ")

    return (reaction_str)



def define_change_detection_and_isolation(grpList=None,
                                          reduced_inf_of_det_cases=True,
                                          d_As=True,
                                          d_P=True,
                                          d_Sym_ct=True,
                                          d_Sym_grp=False,
                                          d_Sym_grp_option=None):
    """ Write the emodl chunk for changing detection rates and reduced infectiousness
    to approximate contact tracing or improved health system interventions.
    Helper function called by write_interventions

    Parameters
    ----------
    grpList: list
        List that contains the groupnames for which parameters are repeated
    reduced_inf_of_det_cases : boolean
        Boolean to add a change in infectiousness of As and P detected cases if set to True
    d_As : boolean
        Boolean to add a change in detection of asymptomatic cases if set to True
    d_P : boolean
        Boolean to add a change in detection of presymptomatic cases if set to True
    d_Sym_ct : boolean
        Boolean to add a change in detection of symptomatic cases if set to True
    d_Sym_grp : boolean
        Boolean to denote whether dSym is group specific or generic
    d_Sym_grp_option : character
        Chracter used to flag which increase option to select, possible characters are:
        increase_to_grp_target (select for each group a specific target to reach),
        increase_to_common_target (use same target for all groups),
        common_increase (rather than replacing the old detection level, increase by a specified percentage),
        grp_specific_increase (define a group specific increase, i.e. group 1 by 10%, group 2 by 50%).
        Default is increase_to_common_target
    """

    observe_str = """
(observe d_As_t d_As)
(observe d_P_t d_P)
"""

    reduced_inf_of_det_cases_str = ""
    d_As_str = ""
    d_P_str = ""
    d_Sym_ct_param_str = ""
    d_Sym_ct_str = ""

    if reduced_inf_of_det_cases:
        reduced_inf_of_det_cases_str = """(reduced_inf_of_det_cases_ct @reduced_inf_of_det_cases_ct1@ )"""
    if d_As:
        d_As_str = """(d_As @d_AsP_ct1@)"""
    if d_P:
        d_P_str = """(d_P @d_AsP_ct1@)"""

    if d_Sym_ct:

        ### Simple, not group specific
        if d_Sym_ct and not d_Sym_grp:
            d_Sym_ct_str = """(d_Sym @d_Sym_ct1@)"""

        ### Group specific
        if d_Sym_grp:

            for grp in grpList:

                if d_Sym_grp_option == 'increase_to_grp_target':
                    d_Sym_ct_param_str = d_Sym_ct_param_str + """(param d_Sym_ct1_{grp} @d_Sym_ct1_{grp}@)""".format(
                        grp=grp)

                if d_Sym_grp_option == 'increase_to_common_target':
                    d_Sym_ct_param_str = d_Sym_ct_param_str + "\n" + """(param d_Sym_ct1_{grp} @d_Sym_ct1@)""".format(
                        grp=grp)

                if d_Sym_grp_option == 'common_increase':
                    d_Sym_ct_param_str = d_Sym_ct_param_str + "\n" + """(param d_Sym_ct1_{grp} (+ @d_Sym_change5_{grp}@ (* @d_Sym_change5_{grp}@ @d_Sym_ct1@ )))""".format(
                        grp=grp)

                if d_Sym_grp_option == 'grp_specific_increase':
                    d_Sym_ct_param_str = d_Sym_ct_param_str + "\n" + """(param d_Sym_ct1_{grp} (+ @d_Sym_change5_{grp}@ (* @d_Sym_change5_{grp}@ @d_Sym_ct1_{grp}@ )))""".format(
                        grp=grp)

                d_Sym_ct_str = d_Sym_ct_str + """(d_Sym_{grp} d_Sym_ct1_{grp})""".format(grp=grp)

    observe_str = observe_str + "\n" + d_Sym_ct_param_str
    change_param_str = reduced_inf_of_det_cases_str + d_As_str + d_P_str + d_Sym_ct_str
    time_event_str = """(time-event contact_tracing_start @contact_tracing_start_1@ ( {change_param} ))""".format(
        change_param=change_param_str)

    contactTracing_str = observe_str + "\n" + time_event_str

    return (contactTracing_str)



def write_interventions(grpList, total_string, scenarioName, change_testDelay=None, trigger_channel=None):
    param_change_str = """
(observe d_Sys_t d_Sys)
(time-event detection1 @detection_time_1@ ((d_Sys @d_Sys_incr1@)))
(time-event detection2 @detection_time_2@ ((d_Sys @d_Sys_incr2@)))
(time-event detection3 @detection_time_3@ ((d_Sys @d_Sys_incr3@)))
(time-event detection4 @detection_time_4@ ((d_Sys @d_Sys_incr4@)))
(time-event detection5 @detection_time_5@ ((d_Sys @d_Sys_incr5@)))
(time-event detection6 @detection_time_6@ ((d_Sys @d_Sys_incr6@)))
(time-event detection5 @detection_time_7@ ((d_Sys @d_Sys_incr7@)))


(observe frac_crit_t fraction_critical)
(observe fraction_hospitalized_t fraction_hospitalized)
(observe fraction_dead_t fraction_dead)

(time-event frac_crit_adjust1 @crit_time_1@ ((fraction_critical @fraction_critical_incr1@) (fraction_hospitalized (- 1 (+ @fraction_critical_incr1@ @fraction_dead@))) (Kh1 (/ fraction_hospitalized time_to_hospitalization)) (Kh2 (/ fraction_critical time_to_hospitalization )) (Kh1_D (/ fraction_hospitalized (- time_to_hospitalization time_D_Sys))) (Kh2_D (/ fraction_critical (- time_to_hospitalization time_D_Sys) )) ))  
(time-event frac_crit_adjust2 @crit_time_2@ ((fraction_critical @fraction_critical_incr2@) (fraction_hospitalized (- 1 (+ @fraction_critical_incr2@ @fraction_dead@))) (Kh1 (/ fraction_hospitalized time_to_hospitalization)) (Kh2 (/ fraction_critical time_to_hospitalization )) (Kh1_D (/ fraction_hospitalized (- time_to_hospitalization time_D_Sys))) (Kh2_D (/ fraction_critical (- time_to_hospitalization time_D_Sys) )) ))
(time-event frac_crit_adjust3 @crit_time_3@ ((fraction_critical @fraction_critical_incr3@) (fraction_hospitalized (- 1 (+ @fraction_critical_incr3@ @fraction_dead@))) (Kh1 (/ fraction_hospitalized time_to_hospitalization)) (Kh2 (/ fraction_critical time_to_hospitalization )) (Kh1_D (/ fraction_hospitalized (- time_to_hospitalization time_D_Sys))) (Kh2_D (/ fraction_critical (- time_to_hospitalization time_D_Sys) )) )) 

(param cfr_change1 (* @cfr@ (/ 2 3) ) )
(param cfr_change2 (* @cfr@ (/ 1 3) ) )
(observe cfr_t cfr)
(time-event cfr_adjust1 @cfr_time_1@ ((cfr cfr_change1) (fraction_dead (/ cfr fraction_severe)) (fraction_hospitalized (- 1 (+ fraction_critical fraction_dead))) (Kh1 (/ fraction_hospitalized time_to_hospitalization)) (Kh2 (/ fraction_critical time_to_hospitalization )) (Kh1_D (/ fraction_hospitalized (- time_to_hospitalization time_D_Sys))) (Kh2_D (/ fraction_critical (- time_to_hospitalization time_D_Sys) )) )) 
(time-event cfr_adjust2 @cfr_time_2@ ((cfr cfr_change2) (fraction_dead (/ cfr fraction_severe)) (fraction_hospitalized (- 1 (+ fraction_critical fraction_dead))) (Kh1 (/ fraction_hospitalized time_to_hospitalization)) (Kh2 (/ fraction_critical time_to_hospitalization )) (Kh1_D (/ fraction_hospitalized (- time_to_hospitalization time_D_Sys))) (Kh2_D (/ fraction_critical (- time_to_hospitalization time_D_Sys) )) )) 
"""

    ki_multiplier_change_str = ""
    for grp in grpList:
        temp_str = """
(param Ki_red3a_{grp} (* Ki_{grp} @ki_multiplier_3a_{grp}@))
(param Ki_red3b_{grp} (* Ki_{grp} @ki_multiplier_3b_{grp}@))
(param Ki_red3c_{grp} (* Ki_{grp} @ki_multiplier_3c_{grp}@))
(param Ki_red4_{grp} (* Ki_{grp} @ki_multiplier_4_{grp}@))
(param Ki_red5_{grp} (* Ki_{grp} @ki_multiplier_5_{grp}@))
(param Ki_red6_{grp} (* Ki_{grp} @ki_multiplier_6_{grp}@))
(param Ki_red7_{grp} (* Ki_{grp} @ki_multiplier_7_{grp}@))
(param Ki_red8_{grp} (* Ki_{grp} @ki_multiplier_8_{grp}@))
(param Ki_red9_{grp} (* Ki_{grp} @ki_multiplier_9_{grp}@))
(param Ki_red10_{grp} (* Ki_{grp} @ki_multiplier_10_{grp}@))
(param Ki_red11_{grp} (* Ki_{grp} @ki_multiplier_11_{grp}@))
(param Ki_red12_{grp} (* Ki_{grp} @ki_multiplier_12_{grp}@))
(param Ki_red13_{grp} (* Ki_{grp} @ki_multiplier_13_{grp}@))

(time-event ki_multiplier_change_3a @ki_multiplier_time_3a@ ((Ki_{grp} Ki_red3a_{grp})))
(time-event ki_multiplier_change_3b @ki_multiplier_time_3b@ ((Ki_{grp} Ki_red3b_{grp})))
(time-event ki_multiplier_change_3c @ki_multiplier_time_3c@ ((Ki_{grp} Ki_red3c_{grp})))
(time-event ki_multiplier_change_4 @ki_multiplier_time_4@ ((Ki_{grp} Ki_red4_{grp})))
(time-event ki_multiplier_change_5 @ki_multiplier_time_5@ ((Ki_{grp} Ki_red5_{grp})))
(time-event ki_multiplier_change_6 @ki_multiplier_time_6@ ((Ki_{grp} Ki_red6_{grp})))
(time-event ki_multiplier_change_7 @ki_multiplier_time_7@ ((Ki_{grp} Ki_red7_{grp})))
(time-event ki_multiplier_change_8 @ki_multiplier_time_8@ ((Ki_{grp} Ki_red8_{grp})))
(time-event ki_multiplier_change_9 @ki_multiplier_time_9@ ((Ki_{grp} Ki_red9_{grp})))
(time-event ki_multiplier_change_10 @ki_multiplier_time_10@ ((Ki_{grp} Ki_red10_{grp})))
(time-event ki_multiplier_change_11 @ki_multiplier_time_11@ ((Ki_{grp} Ki_red11_{grp})))
(time-event ki_multiplier_change_12 @ki_multiplier_time_12@ ((Ki_{grp} Ki_red12_{grp})))
(time-event ki_multiplier_change_13 @ki_multiplier_time_13@ ((Ki_{grp} Ki_red13_{grp})))
            """.format(grp=grp)
        ki_multiplier_change_str = ki_multiplier_change_str + temp_str

    rollback_str = ""
    for grp in grpList:
        temp_str = """
(time-event ki_multiplier_change_rollback @socialDistance_rollback_time@ ((Ki_{grp} Ki_red4_{grp})))
                """.format(grp=grp)
        rollback_str = rollback_str + temp_str

    rollbacktriggered_str = ""
    for grp in grpList:
        temp_str = """
(state-event rollbacktrigger_{grp} (and (> time @today@) (> {channel}_{grp} (* @trigger_{grp}@ @capacity_multiplier@)) ) ((Ki_{grp} Ki_red7_{grp})))
                    """.format(channel=trigger_channel, grp=grp)
        rollbacktriggered_str = rollbacktriggered_str + temp_str

    rollbacktriggered_delay_str = ""
    for grp in grpList:
        grpout = sub(grp)
        temp_str = """
(param time_of_trigger_{grp} 10000)
(state-event rollbacktrigger_{grp} (and (> time @today@) (> crit_det_{grp} (* @trigger_{grp}@ @capacity_multiplier@)) ) ((time_of_trigger_{grp} time)))
(func time_since_trigger_{grp} (- time time_of_trigger_{grp}))
(state-event apply_rollback_{grp} (> (- time_since_trigger_{grp} @trigger_delay_days@) 0) ((Ki_{grp} Ki_red7_{grp})))   
(observe triggertime_{grpout} time_of_trigger_{grp})
                   """.format(channel=trigger_channel, grpout=grpout, grp=grp)
        rollbacktriggered_delay_str = rollbacktriggered_delay_str + temp_str

    d_Sym_change_str = ""
    for grp in grpList:
        grpout = sub(grp)
        temp_str = """
(param d_Sym_{grp} @d_Sym_{grp}@)
(observe d_Sym_t_{grpout} d_Sym_{grp})

(time-event d_Sym_change1 @d_Sym_change_time_1@ ((d_Sym_{grp} @d_Sym_change1_{grp}@)))
(time-event d_Sym_change2 @d_Sym_change_time_2@ ((d_Sym_{grp} @d_Sym_change2_{grp}@)))
(time-event d_Sym_change3 @d_Sym_change_time_3@ ((d_Sym_{grp} @d_Sym_change3_{grp}@)))
(time-event d_Sym_change4 @d_Sym_change_time_4@ ((d_Sym_{grp} @d_Sym_change4_{grp}@)))
(time-event d_Sym_change5 @d_Sym_change_time_5@ ((d_Sym_{grp} @d_Sym_change5_{grp}@)))
            """.format(grpout=grpout, grp=grp)
        d_Sym_change_str = d_Sym_change_str + temp_str

    interventionSTOP_str = ""
    for grp in grpList:
        temp_str = """
(param Ki_back_{grp} (* Ki_{grp} @backtonormal_multiplier@))
(time-event stopInterventions @socialDistanceSTOP_time@ ((Ki_{grp} Ki_back_{grp})))
        """.format(grp=grp)
        interventionSTOP_str = interventionSTOP_str + temp_str

    # % change from lowest transmission level - immediate
    # starting point is lowest level of transmission  Ki_red4
    interventionSTOP_adj_str = ""
    for grp in grpList:
        temp_str = """
(param Ki_back_{grp} (+ Ki_red7_{grp} (* @backtonormal_multiplier@ (- Ki_{grp} Ki_red7_{grp}))))
(time-event stopInterventions @socialDistanceSTOP_time@ ((Ki_{grp} Ki_back_{grp})))
        """.format(grp=grp)
        interventionSTOP_adj_str = interventionSTOP_adj_str + temp_str

    # % change from current transmission level - immediate
    # starting point is current level of transmission  Ki_red6
    interventionSTOP_adj2_str = ""
    for grp in grpList:
        temp_str = """
(param Ki_back_{grp} (+ Ki_red7_{grp} (* @backtonormal_multiplier@ (- Ki_{grp} Ki_red7_{grp}))))
(time-event stopInterventions @socialDistanceSTOP_time@ ((Ki_{grp} Ki_back_{grp})))
        """.format(grp=grp)
        interventionSTOP_adj2_str = interventionSTOP_adj2_str + temp_str

    # gradual reopening from 'lowest' transmission level,  Ki_red6 == Ki_back1
    gradual_reopening_str = ""
    for grp in grpList:
        temp_str = """
(param backtonormal_multiplier_1_adj_{grp}  (- @backtonormal_multiplier@ backtonormal_multiplier_1_{grp} ))
(observe backtonormal_multiplier_1_adj_{grp}  backtonormal_multiplier_1_adj_{grp})

(param Ki_back2_{grp} (+ Ki_red6_{grp} (* backtonormal_multiplier_1_adj_{grp} 0.3333 (- Ki_{grp} Ki_red4_{grp}))))
(param Ki_back3_{grp} (+ Ki_red6_{grp} (* backtonormal_multiplier_1_adj_{grp} 0.6666 (- Ki_{grp} Ki_red4_{grp}))))
(param Ki_back4_{grp} (+ Ki_red6_{grp} (* backtonormal_multiplier_1_adj_{grp} 1.00 (- Ki_{grp} Ki_red4_{grp}))))
(time-event gradual_reopening2 @gradual_reopening_time1@ ((Ki_{grp} Ki_back2_{grp})))
(time-event gradual_reopening3 @gradual_reopening_time2@ ((Ki_{grp} Ki_back3_{grp})))
(time-event gradual_reopening4 @gradual_reopening_time3@ ((Ki_{grp} Ki_back4_{grp})))
        """.format(grp=grp)
        gradual_reopening_str = gradual_reopening_str + temp_str

    # gradual reopening from 'current' transmission level
    gradual_reopening2_str = ""
    for grp in grpList:
        temp_str = """
(param Ki_back1_{grp} (+ Ki_red7_{grp} (* @reopening_multiplier_4@ 0.25 (- Ki_{grp} Ki_red7_{grp}))))
(param Ki_back2_{grp} (+ Ki_red7_{grp} (* @reopening_multiplier_4@ 0.50 (- Ki_{grp} Ki_red7_{grp}))))
(param Ki_back3_{grp} (+ Ki_red7_{grp} (* @reopening_multiplier_4@ 0.75 (- Ki_{grp} Ki_red7_{grp}))))
(param Ki_back4_{grp} (+ Ki_red7_{grp} (* @reopening_multiplier_4@ 1.00 (- Ki_{grp} Ki_red7_{grp}))))
(time-event gradual_reopening1 @gradual_reopening_time1@ ((Ki_{grp} Ki_back1_{grp})))
(time-event gradual_reopening2 @gradual_reopening_time2@ ((Ki_{grp} Ki_back2_{grp})))
(time-event gradual_reopening3 @gradual_reopening_time3@ ((Ki_{grp} Ki_back3_{grp})))
(time-event gradual_reopening4 @gradual_reopening_time4@ ((Ki_{grp} Ki_back4_{grp})))
        """.format(grp=grp)
        gradual_reopening2_str = gradual_reopening2_str + temp_str

    # gradual reopening from 'current' transmission level with region-specific reopening
    gradual_reopening3_str = ""
    for grp in grpList:
        temp_str = """
(param Ki_back1_{grp} (+ Ki_red7_{grp} (* @reopening_multiplier_4_{grp}@ 0.25 (- Ki_{grp} Ki_red7_{grp}))))
(param Ki_back2_{grp} (+ Ki_red7_{grp} (* @reopening_multiplier_4_{grp}@ 0.50 (- Ki_{grp} Ki_red7_{grp}))))
(param Ki_back3_{grp} (+ Ki_red7_{grp} (* @reopening_multiplier_4_{grp}@ 0.75 (- Ki_{grp} Ki_red7_{grp}))))
(param Ki_back4_{grp} (+ Ki_red7_{grp} (* @reopening_multiplier_4_{grp}@ 1.00 (- Ki_{grp} Ki_red7_{grp}))))
(time-event gradual_reopening1 @gradual_reopening_time1@ ((Ki_{grp} Ki_back1_{grp})))
(time-event gradual_reopening2 @gradual_reopening_time2@ ((Ki_{grp} Ki_back2_{grp})))
(time-event gradual_reopening3 @gradual_reopening_time3@ ((Ki_{grp} Ki_back3_{grp})))
(time-event gradual_reopening4 @gradual_reopening_time4@ ((Ki_{grp} Ki_back4_{grp})))
        """.format(grp=grp)
        gradual_reopening3_str = gradual_reopening3_str + temp_str

    improveHS_str = define_change_detection_and_isolation(grpList=grpList,
                                                          reduced_inf_of_det_cases=False,
                                                          d_As=False,
                                                          d_P=False,
                                                          d_Sym_ct=True,
                                                          d_Sym_grp=True,
                                                          d_Sym_grp_option='increase_to_common_target')

    contactTracing_str = define_change_detection_and_isolation(grpList=grpList,
                                                               reduced_inf_of_det_cases=True,
                                                               d_As=True,
                                                               d_P=True,
                                                               d_Sym_ct=False,
                                                               d_Sym_grp=False,
                                                               d_Sym_grp_option=None)

    contactTracing_improveHS_str = define_change_detection_and_isolation(grpList=grpList,
                                                                         reduced_inf_of_det_cases=True,
                                                                         d_As=True,
                                                                         d_P=True,
                                                                         d_Sym_ct=True,
                                                                         d_Sym_grp=True,
                                                                         d_Sym_grp_option='increase_to_common_target')

    change_uniformtestDelay_str = """
(time-event change_testDelay1 @change_testDelay_time1@ ( {} {} {} {} {} {} {} ))
    """.format("(time_D @change_testDelay_1@)",
               "(Ksys_D (/ 1 time_D))",
               "(Ksym_D (/ 1 time_D))",
               "(Kh1_D (/ fraction_hospitalized (- time_to_hospitalization time_D)))",
               "(Kh2_D (/ fraction_critical (- time_to_hospitalization time_D) ))",
               "(Kh3_D (/ fraction_dead (- time_to_hospitalization time_D)))",
               "(Kr_m_D (/ 1 (- recovery_time_mild time_D )))")

    change_testDelay_Sym_str = """
(time-event change_testDelay1 @change_testDelay_time1@ ( {} {} {} ))
    """.format("(time_D_Sym @change_testDelay_Sym_1@)",
               "(Ksym_D (/ 1 time_D_Sym))",
               "(Kr_m_D (/ 1 (- recovery_time_mild time_D_Sym )))")

    change_testDelay_Sys_str = """
(time-event change_testDelay1 @change_testDelay_time1@ ( {} {} {} {} {} ))
    """.format("(time_D_Sys @change_testDelay_Sys_1@)",
               "(Ksys_D (/ 1 time_D_Sys))",
               "(Kh1_D (/ fraction_hospitalized (- time_to_hospitalization time_D_Sys)))",
               "(Kh2_D (/ fraction_critical (- time_to_hospitalization time_D_Sys) ))",
               "(Kh3_D (/ fraction_dead (- time_to_hospitalization time_D_Sys)))")

    change_testDelay_As_str = """
(time-event change_testDelay1 @change_testDelay_time1@ ( {} {} {} ))
    """.format("(time_D_As @change_testDelay_As_1@)",
               "(Kl_D (/ 1 time_D_As))",
               "(Kr_a_D (/ 1 (- recovery_time_asymp time_D_As )))")

    fittedTimeEvents_str = param_change_str + ki_multiplier_change_str + d_Sym_change_str

    if scenarioName == "interventionStop":
        total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str + interventionSTOP_str)
    if scenarioName == "interventionSTOP_adj":
        total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str + interventionSTOP_adj_str)
    if scenarioName == "interventionSTOP_adj2":
        total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str + interventionSTOP_adj2_str)
    if scenarioName == "gradual_reopening":
        total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str + gradual_reopening_str)
    if scenarioName == "gradual_reopening2":
        total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str + gradual_reopening2_str)
    if scenarioName == "gradual_reopening3":
        total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str + gradual_reopening3_str)
    if scenarioName == "continuedSIP":
        total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str)
    if scenarioName == "rollback":
        total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str + rollback_str)
    if scenarioName == "reopen_rollback":
        total_string = total_string.replace(';[INTERVENTIONS]',
                                            fittedTimeEvents_str + interventionSTOP_adj2_str + rollback_str)
    if scenarioName == "reopen_contactTracing":
        total_string = total_string.replace(';[INTERVENTIONS]',
                                            fittedTimeEvents_str + gradual_reopening2_str + contactTracing_str)
    if scenarioName == "reopen_contactTracing_improveHS":
        total_string = total_string.replace(';[INTERVENTIONS]',
                                            fittedTimeEvents_str + gradual_reopening2_str + contactTracing_improveHS_str)
    if scenarioName == "reopen_improveHS":
        total_string = total_string.replace(';[INTERVENTIONS]',
                                            fittedTimeEvents_str + gradual_reopening2_str + improveHS_str)
    if scenarioName == "contactTracing":
        total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str + contactTracing_str)
    if scenarioName == "contactTracing_improveHS":
        total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str + contactTracing_improveHS_str)
    if scenarioName == "improveHS":
        total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str + improveHS_str)
    if scenarioName == "rollbacktriggered":
        total_string = total_string.replace(';[INTERVENTIONS]',
                                            fittedTimeEvents_str + gradual_reopening2_str + rollbacktriggered_str)
    if scenarioName == "rollbacktriggered_delay":
        total_string = total_string.replace(';[INTERVENTIONS]',
                                            fittedTimeEvents_str + gradual_reopening3_str + rollbacktriggered_delay_str)

    # if scenarioName == "gradual_contactTracing" :
    #    total_string = total_string.replace(';[INTERVENTIONS]', fittedTimeEvents_str + gradual_reopening2_str + contactTracing_gradual_str)

    if change_testDelay != None:
        if change_testDelay == "uniform":
            total_string = total_string.replace(';[ADDITIONAL_TIMEEVENTS]', change_uniformtestDelay_str)
        if change_testDelay == "As":
            total_string = total_string.replace(';[ADDITIONAL_TIMEEVENTS]', change_testDelay_As_str)
        if change_testDelay == "Sym":
            total_string = total_string.replace(';[ADDITIONAL_TIMEEVENTS]', change_testDelay_Sym_str)
        if change_testDelay == "Sys":
            total_string = total_string.replace(';[ADDITIONAL_TIMEEVENTS]', change_testDelay_Sys_str)
        if change_testDelay == "AsSym":
            total_string = total_string.replace(';[ADDITIONAL_TIMEEVENTS]',
                                                change_testDelay_As_str + '\n' + change_testDelay_Sym_str)
        if change_testDelay == "SymSys":
            total_string = total_string.replace(';[ADDITIONAL_TIMEEVENTS]',
                                                change_testDelay_Sym_str + '\n' + change_testDelay_Sys_str)
        if change_testDelay == "AsSymSys":
            total_string = total_string.replace(';[ADDITIONAL_TIMEEVENTS]',
                                                change_testDelay_As_str + '\n' + change_testDelay_Sym_str + '\n' + change_testDelay_Sys_str)

    return (total_string)


###stringing all of my functions together to make the file:

def generate_emodl(grpList, file_output, expandModel, add_interventions, observeLevel='primary',
                   change_testDelay=None, trigger_channel=None):
    if (os.path.exists(file_output)):
        os.remove(file_output)

    model_name = "seir.emodl"  ### can make this more flexible
    header_str = "; simplemodel \n\n" + "(import (rnrs) (emodl cmslib)) \n\n" + '(start-model "{}") \n\n'.format(
        model_name)
    footer_str = "(end-model)"

    # building up the .emodl string
    total_string = ""
    species_string = ""
    observe_string = ""
    param_string = ""
    reaction_string = ""
    functions_string = ""
    total_string = total_string + header_str

    for grp in grpList:
        total_string = total_string + "\n(locale site-{})\n".format(grp)
        total_string = total_string + "(set-locale site-{})\n".format(grp)
        total_string = total_string + write_species_vaccine(grp)  + write_speciesaccine(grp)
        functions_string = functions_string + write_functionsaccine(grp) + write_functions_vaccine(grp)
        observe_string = observe_string + write_observe(grp, observeLevel=observeLevel) + write_observe_novaccine(grp, observeLevel=observeLevel) + write_observe_vaccine(grp, observeLevel=observeLevel)
        reaction_string = reaction_string + write_reactionsaccine(grp, expandModel) + write_reactions_vaccine(grp, expandModel)
        param_string = param_string + write_Ki_timeventsaccine(grp) #+ write_Ki_timevents_vaccine(grp)

    param_string = write_paramsaccine() + param_string + write_params_vaccine() + write_N_population(grpList) #+ write_N_population_vaccine(grpList)
    functions_string = functions_string + write_All(grpList, observeLevel=observeLevel) + write_All_novaccine(grpList, observeLevel=observeLevel) + write_All_vaccine(grpList, observeLevel=observeLevel)

    intervention_string = ";[INTERVENTIONS]\n;[ADDITIONAL_TIMEEVENTS]"

    total_string = total_string + '\n\n' + species_string + '\n\n' + functions_string + '\n\n' + observe_string + '\n\n' + param_string + '\n\n' + intervention_string + '\n\n' + reaction_string + '\n\n' + footer_str

    ### Custom adjustments for EMS 6 (earliest start date)
    total_string = total_string.replace('(species As::EMS_6 0)', '(species As::EMS_6 1)')
    ### Add interventions (optional)
    if add_interventions != None:
        total_string = write_interventions(grpList, total_string, add_interventions, change_testDelay, trigger_channel)

    print(total_string)
    emodl = open(file_output, "w")  ## again, can make this more dynamic
    emodl.write(total_string)
    emodl.close()
    if (os.path.exists(file_output)):
        print("{} file was successfully created".format(file_output))
    else:
        print("{} file was NOT created".format(file_output))


if __name__ == '__main__':
    ems_grp = ['EMS_1', 'EMS_2', 'EMS_3', 'EMS_4', 'EMS_5', 'EMS_6', 'EMS_7', 'EMS_8', 'EMS_9', 'EMS_10', 'EMS_11']

    generate_emodl(grpList=ems_grp, expandModel="testDelay_AsSymSys", add_interventions='continuedSIP',
                   file_output=os.path.join(emodl_dir, 'extendedmodel_EMS_vaccine.emodl'))
