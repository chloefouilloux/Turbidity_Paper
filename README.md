# Turbidity_Paper
Data and Data Wrangling for Turbidity Paper

Hello! This is the raw data and code for the Fouilloux et al. (2023) paper exploring the effect of nursery turbidity on the (visual) perception of risk for tadpoles that develop in epehemeral pools of water, called phytotelmata. I did not upload the spectral data because the files are so large, but am happy to send them your way upon request.


## Code ##

**Pumilio_Turb_Final** is the turbidity data from La Selva, Costa Rica. 

**Tinctorius_Turb_Final** is the turbidity data from Les Nouragues, French Guiana. 

**water_spectra** are the spectrophotometry measurements/analysis from nursery water samples from La Selva, CR.



## Data ##

### **LAS_VISION_OCT2022 is the raw data from La Selva, Costa Rica** ###

*Trial* is the trial number (includes all permutations)

*Mean_Abs* is the average spectrophotometry wavelength absorption across three samples of water collected in nurseries.

*Mean_Trans* is the is the average maximum transmission value from technical replicates of spectral data.

*Tad_ID* individual ID given to each tadpole

*Mass* is the mass of tadpoles in grams.

*Stage* is the Gosner (1961) stage to assess tadpole deveopment

*Condition* is the experimental background colour. Either "background" for checkered choice or "black/white" for experiments with predators.

*Predator* is the predator with which tadpole was faced.

*Exploration* is a count of how many times a tadpole changed zones during each trial.

*Cons_g* is the mass in grams of the conspecific visual stimulus

*Spider_mm* is the abdominal length of the spider visual stimulus

*Black* is how many times the tadpole was on a black square during checkered choice trial

*White* is how many times the tadpole was on a white square during checkered choice trial

*Rest* is how many times tadpole was resting

*Swim* is how many times tadpole was swimming

*Pivoting* is how many times tadpole was pivoting (turn 90°)

*Zone_1* (_2, _3) is how many times tadpole was in specificed zone

*rMean_OG* (b, g) are the mean red, blue, or green channel percent reflectance from photographs.

*R_spec*(G_,B_) are the red, green, and blue pixel counts from the PAVO package  conversion estimates of RGB colour from spectrophotometer data.

### **FG_TURB_JAN2023 is the raw data from Les Nouragues (Parare), French Guiana** ###

*Tad_ID* individual ID given to each tadpole

*Pool_Turbidity* is the "tubidity" of a pool based on the readings from a turbidity meter that I don't trust at all.

*Pool_Type* is a categorical variable describing the kind of nursery where tapoles were samples from 

*Tas_Density_Cons* is the number of conspecifics in a nursery

*Predator_Y_N* is the presence of a heterospecific predator in the nursery (Odonata larvae)

*Depth/Length/Width* are the physical measurements of the phytotelmata in cm

*Pool_ID* is the unique ID given to tadpole nursery

*Mass* is the tadpole mass in grams

*Stage* is the Gosner (1961) stage to assess tadpole deveopment

*Condition* is the experimental background colour. Either "background" for checkered choice or "black/white" for experiments with predators.

*Predator* is the predator with which tadpole was faced in the arena.

*Exploration* is a count of how many times a tadpole changed zones during each trial.

*Cons_g* is the mass in grams of the conspecific visual stimulus

*Odon_g* is the mass in grams of the Odonata visual stimulus

*Black* is how many times the tadpole was on a black square during checkered choice trial

*White* is how many times the tadpole was on a white square during checkered choice trial

*Rest* is how many times tadpole was resting

*Swim* is how many times tadpole was swimming

*Zone_1* (_2, _3) is how many times tadpole was in specificed zone

*rMean_OG* (b, g) are the mean red, blue, or green channel percent reflectance from photographs.





