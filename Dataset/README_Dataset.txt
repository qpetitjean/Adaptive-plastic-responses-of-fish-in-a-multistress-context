Variable names	Short description	Format
ColorMark	Visible implant elastomers color combinations 	character strings
IndID	Identification number of each individual 	numerical
OriginSite	The name of the study sites where fish were sampled	character strings
CagingSite	The name of the study sites where fish were caged	character strings
OriginContam	Level of contamination in the study sites where fish come from - HC = High Contamination site; LC = Low Contamination site	character strings
SitesPair	Pair of site (replicate) used for reciprocal transplant 	character strings
CageID	Identification number of each cage, total number of cages used in this study is 24 	numerical
CageSiteID	Identification number of each cage within a given study site, total number of cages per site used in this study is 6 (used for nested random effect)	numerical
TransplantContam	Level of contamination in the study sites where fish are caged - HC = High Contamination site; LC = Low Contamination site	character strings
Transplant	The ; LC_LC or HC_HC = Fish originating from LC or HC site are caged in their own site: no transplant, LC_HC or HC_LC = Fish originating from LC or HC site are caged in HC or LC site respectively: tranplant 	character strings
Injection	Injection treatment: PBS = control-saline injection (10µL); AMIX = antigen mixture mimicking parasite attack. AMIX is composed of a mixture of 90µg of LPS and 90µg of PHA in 10µL of salin phosphate buffer solution (PBS). Injection is performed at the caudal peduncle	character strings
Treatment	The combination of transplant and injection treatments; see description of Transplant and Injection variables	character strings
WeightStart	Fish weight at the start of the experiment (on the day of the transplant), expressed in grams	numerical
SizeStart	Fish size at the start of the experiment (on the day of the transplant), expressed in centimeters	numerical
Weight7D	Fish weight 7 day after the start of the experiment (on the day of the Injection), expressed in grams	numerical
Size7D	Fish size 7 day after the start of the experiment (on the day the Injection), expressed in centimeters	numerical
Weight9D	Fish weight 9 day after the start of the experiment (on the day of caudal peduncle measurement = Skin swelling: local inflammatory response), expressed in grams	numerical
Size9D	Fish size 9 day after the start of the experiment (on the day of caudal peduncle measurement = Skin swelling: local inflammatory response), expressed in centimeters	numerical
WeightEnd	Fish weight 14 days after the start of the experiment (the end of the experiment), expressed in grams	numerical
SizeEnd	Fish size 14 days after the start of the experiment (the end of the experiment), expressed in centimeters	numerical
DailyMassChange	Fish weight change over the experiment expressed in percent per day according to WeightStart and the number of day spent alive for each fish: (WeightEnd-WeightStart)*100/WeightStart/DaysAlive. In case fish are found dead at day 7 the calculation becomes (Weight7D-WeightStart)*100/WeightStart/DaysAlive	numerical
Death	Indicate if fish lived (0) until the end of the experiment or died (1) before the end of the experiment	binomial 
DeathDate	If fish died before the end of the experiment the date at which fish is found dead in the cage is reported here	date
MarkDate	The date at which fish were marked using visible implant elastomers color combinations 	date
InjectionDate	The date at which the initial measurements of the caudal peduncle and injection treatment (either PBS or AMIX) were performed, it corresponds to the 7 days of the experiment	date
EuthanasiaDate	The date at which fish were euthanized, it corresponds to the 14 days of the experimen. In case fish were found dead within the cage before the end of the experiment it corresponds to the DeathDate	date
markDate	The number of day fish spent alive in the experiment (max value is 14 days)	date
InjectionTime	The time at which the initial set of measurements of the caudal peduncle and the injection treatment were performed (on the 7 days of the experiment)	HH:MM:SS
Pcaud1D7	First repetition of the initial set of caudal peduncle measurement (on the 7 days of the experiment), the thickness if the peduncle is expressed in millimeters 	numerical
Pcaud2D7	Second repetition of the initial set of caudal peduncle measurement (on the 7 days of the experiment),  the thickness if the peduncle is expressed in millimeters 	numerical
Pcaud3D7	Third repetition of the initial set of caudal peduncle measurement (on the 7 days of the experiment),  the thickness if the peduncle is expressed in millimeters 	numerical
MeanPcaudD7	Mean value of the initial set of caudal peduncle measurement (on the 7 days of the experiment),  the thickness if the peduncle is expressed in millimeters. Corresponds to the mean of Pcaud1D7, Pcaud2D7, Pcaud3D7	numerical
PcaudDate	The date at which the second set of measurements of the caudal peduncle was performed, it corresponds to the 9 days of the experiment	date
PcaudTime	The time at which the second set of measurements of the caudal peduncle was performed (on the 9 days of the experiment)	HH:MM:SS
Pcaud1D9	First repetition of the second set of caudal peduncle measurement (on the 9 days of the experiment), the thickness if the peduncle is expressed in millimeters 	numerical
Pcaud2D9	Second repetition of the second set of caudal peduncle measurement (on the 9 days of the experiment), the thickness if the peduncle is expressed in millimeters  	numerical
Pcaud3D9	Thrid repetition of the second set of caudal peduncle measurement (on the 9 days of the experiment), the thickness if the peduncle is expressed in millimeters 	numerical
MeanPcaudD9	Mean value of the second set of caudal peduncle measurement (on the 9 days of the experiment),  the thickness if the peduncle is expressed in millimeters. Corresponds to the mean of Pcaud1D9, Pcaud2D9, Pcaud3D9	numerical
IR	The intensity of the local immune response computed as the difference of peduncle thickness before and after injection divided by the thickness before injection × 100 or (MeanPcaudD7 - MeanPcaudD9)/MeanPcaudD7*100	numerical
IRTime	Elapsed time between the inital measurements of the caudal peduncle of fish on the day of injection (7 days after the start of the experiment) and measurements 2 days later (9 days after the start of the experiment), expressed in hours	numerical
GonadsWeight	The weight of fish gonads at the end of the experiment (14 days after the start of the experiment), expressed in grams	numerical
Sex	Fish sex determined from visual inspection of the gonads at the end of the experiments (14 days after the start of the experiment). M = Male, F = Female	character strings
LiverWeight	The weight of fish liver at the end of the experiment (14 days after the start of the experiment), expressed in grams	numerical
GSI	Gonadosomatic Index of fish corresponding to GonadsWeight/WeightEnd*100	numerical
HSI	Hepatosomatic Index of fish corresponding to LiverWeight/WeightEnd*100	numerical
Lymphocytes	Number of Lymphocytes counted on blood smears stained with May-Grunwald Giemsa solution at the end of the experiment (14 days after the start of the experiment). It corresponds to the relative number of Lymphocytes counted according to 100 leukocytes (i.e., monocytes, neutrophils, and lymphocytes) 	numerical
Monocytes	Number of Monocytes counted on blood smears stained with May-Grunwald Giemsa solution at the end of the experiment (14 days after the start of the experiment). It corresponds to the relative number of Monocytes counted according to 100 leukocytes (i.e., monocytes, neutrophils, and lymphocytes) 	numerical
Neutrophils	Number of Neutrophils counted on blood smears stained with May-Grunwald Giemsa solution at the end of the experiment (14 days after the start of the experiment). It corresponds to the relative number of Neutrophils counted according to 100 leukocytes (i.e., monocytes, neutrophils, and lymphocytes) 	numerical
mMH2O2	Oxidative damage measured in fish plasma using d-ROM test (Diacron International, Grosseto, Italy) at the end of the experiment (14 days after the start of the experiment) expressed in mMeq H2O2	numerical
mMHCLO	Antioxidant capacity of fish plasma measured using OXY-adsorbent test (Diacron International, Grosseto, Italy) at the end of the experiment (14 days after the start of the experiment) expressed in mMeq HClO	numerical
MuscleCarbohydrate	Total amount of carbohydrate in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in energetic values mJ.mg-1	numerical
MuscleProtein	Total amount of Protein in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in energetic values mJ.mg-1	numerical
MuscleLipid	Total amount of Lipid in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in energetic values mJ.mg-1	numerical
AvailableEnerJ	Total amount of available energy in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in energetic values mJ.mg-1. It corresponds to the sum of total amount of muscle carbohydrate, lipid and protein.	numerical
CqActb	Cq (quantification cycle) value for Beta actin (Actb) housekeeping gene from fish liver (PCR cycle number at Actb gene's reaction curve intersects the threshold line)	numerical
DeltaCtCat	Delta Ct value for Catalase gene (Cat), corresponds to the relative expression of the CAT according to Actb Cq value (Delta Ct = 2^-(Cq (gene of interest)-Cq (housekeeping gene))	numerical
DeltaCtCasp3	Delta Ct value for Caspase 3 gene (Casp3), corresponds to the relative expression of the Casp3 according to Actb Cq value (Delta Ct = 2^-(Cq (gene of interest)-Cq (housekeeping gene))	numerical
DeltaCtGpx	Delta Ct value for Glutathione peroxidase gene (Gpx), corresponds to the relative expression of the Gpx according to Actb Cq value (Delta Ct = 2^-(Cq (gene of interest)-Cq (housekeeping gene))	numerical
DeltaCtMtl	Delta Ct value for Metallothionein gene (Mtl), corresponds to the relative expression of the Mtl according to Actb Cq value (Delta Ct = 2^-(Cq (gene of interest)-Cq (housekeeping gene))	numerical
DeltaCtPcx	Delta Ct value for Pyruvate carboxylase gene (Pcx), corresponds to the relative expression of the Pcx according to Actb Cq value (Delta Ct = 2^-(Cq (gene of interest)-Cq (housekeeping gene))	numerical
DeltaCtPygl	Delta Ct value for Glycogen Phosphorylase gene (Pygl), corresponds to the relative expression of the Pygl according to Actb Cq value (Delta Ct = 2^-(Cq (gene of interest)-Cq (housekeeping gene))	numerical
MuscleAl	Concentration of Aluminium (Al) in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in ppm (µg.g-1)	numerical
MuscleCr	Concentration of chromium (Cr) in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in ppm (µg.g-1)	numerical
MuscleCo	Concentration of Cobalt (Co) in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in ppm (µg.g-1)	numerical
MuscleNi	Concentration of Nickel (Ni) in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in ppm (µg.g-1)	numerical
MuscleCu	Concentration of Copper (Cu) in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in ppm (µg.g-1)	numerical
MuscleZn	Concentration of Zinc (Zn) in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in ppm (µg.g-1)	numerical
MuscleAs	Concentration of Arsenic (As) in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in ppm (µg.g-1)	numerical
MuscleCd	Concentration of Cadmium (Cd) in fish white muscle at the end of the experiment (14 days after the start of the experiment), expressed in ppm (µg.g-1)	numerical
