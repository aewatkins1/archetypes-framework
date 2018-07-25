import pandas as pd
import json

import os

from dtk.vector.species import set_params_by_species, set_species_param
from dtk.interventions.health_seeking import add_health_seeking
from dtk.interventions.irs import add_IRS
from dtk.interventions.itn_age_season import add_ITN_age_season

from input_file_generation.add_properties_to_demographics import generate_demographics_properties, check_df_valid


def net_usage_overlay(site_name, hates_net_prop):

    base_demog_path = os.path.join("sites", site_name, "demographics_{name}.json".format(name=site_name))
    overlay_path = os.path.join("sites", site_name, "demographics_{name}_hateforest_{prop}.json".format(name=site_name,
                                                                                                        prop=hates_net_prop))
    with open(base_demog_path) as f:
        demo = json.loads(f.read())

    nodeid = demo["Nodes"][0]["NodeID"]

    prop_by_node_dict = { 'node' : [nodeid, nodeid],
                          'Property' : ['NetUsage', 'NetUsage'],
                          'Property_Type' : ['IP', 'IP'],
                          'Property_Value' : [ 'HatesNets',
                                               'LovesNets'],
                          'Initial_Distribution' : [hates_net_prop, 1-hates_net_prop]}

    # base IPs and NPs
    IPs = [
        { 'Property' : 'NetUsage',
          'Values' : [ 'HatesNets',
                       'LovesNets'],
          'Initial_Distribution' : [0, 1],
          'Transitions' : [] }
    ]
    NPs = [
    ]

    df = pd.DataFrame(prop_by_node_dict)
    if (not df.empty) and (not check_df_valid(df, IPs, NPs)):
        print('properties by node df is invalid')
        exit()
    generate_demographics_properties(base_demog_path, overlay_path, IPs=IPs, NPs=NPs, df=df, as_overlay=True)

def assign_overlay(cb, fname, tags):
    filenames = cb.get_param('Demographics_Filenames')
    cb.update_params({"Demographics_Filenames": filenames + [fname]})
    return tags

def site_simulation_setup(cb, site_name, species_details, vectors, max_larval_capacity=4e8):

    site_dir = os.path.join("sites", site_name)

    # directories
    # todo: do I need this if I specify an asset collection?
    cb.update_params({
                    "Demographics_Filenames": ["sites/{site}/demographics_{site}.json".format(site=site_name)],
                    "Air_Temperature_Filename": os.path.join(site_dir,
                                                             "air_temperature_daily.bin"),
                    "Land_Temperature_Filename": os.path.join(site_dir,
                                                              "air_temperature_daily.bin"),
                    "Rainfall_Filename": os.path.join(site_dir,
                                                      "rainfall_daily.bin"),
                    "Relative_Humidity_Filename": os.path.join(site_dir,
                                                               "relative_humidity_daily.bin")
                    }
    )

    # Find vector proportions for each vector in our site
    set_params_by_species(cb.params, [name for name in vectors.keys()])

    for species_name, species_prop in vectors.items():
        set_species_param(cb, species_name, "Adult_Life_Expectancy", 20)
        set_species_param(cb, species_name, "Vector_Sugar_Feeding_Frequency", "VECTOR_SUGAR_FEEDING_NONE")

        species_modifications = species_details[species_name]

        for param, val in species_modifications.items():
            if param == "habitat_split":
                new_vals = {hab: hab_prop * species_prop * max_larval_capacity for hab, hab_prop in val.items()}
                set_species_param(cb, species_name, "Larval_Habitat_Types", new_vals)
            else:
                set_species_param(cb, species_name, param, val)

    return_params = {"{species}_proportion".format(species=species): prop for species, prop in vectors.items()}
    return_params["Site_Name"] = site_name

    return return_params


# itns
def add_annual_itns(cb, year_count=1, n_rounds=1, coverage=0.8, discard_halflife=270, start_day=0, IP=[]):

    # per-round coverage: 1 minus the nth root of *not* getting a net in any one round
    per_round_coverage = 1 - (1 - coverage) ** (1 / n_rounds)

    for year in range(year_count):
        for this_round in range(n_rounds):

            add_ITN_age_season(cb,
                               coverage_all=per_round_coverage,
                               discard={"halflife": discard_halflife},
                               start=(365 * year) + (30 * this_round) + start_day,
                               ind_property_restrictions=IP)

    return {"ITN_Coverage": coverage, "ITN_Halflife": discard_halflife, "ITN_Per_Round_Coverage": per_round_coverage,
            "ITN_start": start_day, "ITN_Rounds": n_rounds, "ITN_Distributions": year_count}


# irs
def add_irs_group(cb, coverage=1.0, start_days=[0], decay=270):

    waning = {
        "Killing_Config": {
            "Initial_Effect": 0.6,
            "Decay_Time_Constant": decay,
            "class": "WaningEffectExponential"
        },
        "Blocking_Config": {
            "Initial_Effect": 0.0,
            "Decay_Time_Constant": 730,
            "class": "WaningEffectExponential"
        }}

    for start in start_days:
        add_IRS(cb, start, [{"min": 0, "max": 200, "coverage": coverage}],
                waning=waning)

    return {"IRS_Halflife": decay, "IRS_Start": start_days[0], "IRS_Coverage": coverage}


# act
def add_healthseeking_by_coverage(cb, coverage=1.0):
    add_health_seeking(cb,
                       targets=[{"trigger": "NewClinicalCase",
                                 "coverage": coverage,
                                 "agemin": 0,
                                 "agemax": 100,
                                 "seek": 1.0,
                                 "rate": 0.15}],
                       drug=["Artemether", "Lumefantrine"],
                       dosing="FullTreatmentNewDetectionTech",
                       nodes={"class": "NodeSetAll"},
                       repetitions=1,
                       tsteps_btwn_repetitions=365,
                       broadcast_event_name="Received_Treatment")

    return {"ACT_Coverage": coverage}
