#C Preliminary North Atlantic Blue Shark SS3 model run 06
1	#_N_Growth_Patterns																																																																																																																												
1	#_N_Morphs_Within_GrowthPattern 																																																																																																																												
0	#_Nblock_Designs																																																																																																																												
0.5	#_fracfemale 																																																																																																																												
4	#_natM_type:_0=1Parm; 1=N_breakpoints;_2=Lorenzen;_3=agespecific;_4=agespec_withseasinterpolate																																																																																																																												
# Male;Female	0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	#																																																																																																											
	0.3788	0.3041	0.2693	0.2493	0.2364	0.2274	0.2209	0.2160	0.2123	0.2093	0.2069	0.2059	0.2050	0.2043	0.2037	0.2032	0.2028	#																																																																																																											
1	# GrowthModel: 1=vonBert with L1&L2; 2=vonBert with A0&Linf; 3=Richards; 4=readvector																																																																																																																												
0	#_Growth_Age_for_L1  																																																																																																																												
999	#_Growth_Age_for_L2 (999 to use as Linf)																																																																																																																												
0.0	#_SD_add_to_LAA (set to 0.1 for SS2 V1.x compatibility)																																																																																																																												
0	#_CV_Growth_Pattern:  0 CV=f(LAA); 1 CV=F(A); 2 SD=F(LAA); 3 SD=F(A)																																																																																																																												
3	#_maturity_option:  1=length logistic; 2=age logistic; 3=read age-maturity matrix by growth_pattern Nages+1; (4) read age-fecundity for each female GP; 5: read empirical age-fecundity and body weight-at-age from separate file, "wtatage.ss" See section Empirical Wt-at-Age for details.NOTE: need to read 2 parameters even if option 3, 4, or 5	TO DO explore model sensativity to age based fecundity estimate from Conrath 2002: use maturity option (4) "read age-fecundity for each female GP"																																																																																																																											
#) COND If Maturity option =3 or =4; Age_Maturity by growth pattern Nages +1 (0,1, 2, …,16)																																																																																																																													
# Female	0	1	2	3	4	5	6	7	8	9	10	11	12	13	14	15	16	#																																																																																																											
 	0.00	0.00	0.00	0.00	0.00	0.00	0.00	0.50	1.00	1.00	1.00	1.00	1.00	1.00	1.00	1.00	1.00	#																																																																																																											
0	#_First_Mature_Age; Overridden if maturity option = 3, 4 or 5 but still must exist here																																																																																																																												
2	#_fecundity option:(1)eggs=Wt*(a+b*Wt);(2)eggs=a*L^b;(3)eggs=a*Wt^b; (4) fecundity =a+b*L; (5) E=a+B*W; This option is irrelevant if Maturity_Option =4 or 5																																																																																																																												
0	# Hermaphroditism option 0 = no; 1 = invoke female to male transition																																																																																																																												
1	#_parameter_offset_approach (1=none, 2= M, G, CV_G as offset from female-GP1, 3=like SS2 V1.x)																																																																																																																												
1	#_env/block/dev_adjust_method (1=standard; 2=with logistic trans to keep within base parm bounds)																																																																																																																												
#If MGparm_def option =1 (direct estimate, not offset), then setting a male growth or natural mortality parameter value to 0.0 and not estimating it will cause SS to use the corresponding female parameter values for the males																																																																																																																													
#_growth_parms																																																																																																																													
5	100	62.27732	62.27732	0	1000	-3	0	0	0	0	0.5	0	0	# cm FL	L_at_Amin_Fem_GP_1_																																																																																																														
50	600	296.00000	296.00000	0	1000	-4	0	0	0	0	0.5	0	0	# cm FL	L_at_Amax_Fem_GP_1_																																																																																																														
0.01	0.65	0.15500	0.15500	0	0.2	-5	0	0	0	0	0.5	0	0	# cm FL	VonBert_K_Fem_GP_1_																																																																																																														
0.01	0.3	0.15	0.15	0	999	-2	0	0	0	0	0.5	0	0	#	CV_young_Fem_GP_1_																																																																																																														
0.01	0.3	0.12	0.12	0	0.8	-3	0	0	0	0	0.5	0	0	#	CV_old_Fem_GP_1_																																																																																																														
-3	3	3.18E-06	3.18E-06	0	0.8	-3	0	0	0	0	0.5	0	0	# cm FL	Wtlen1_Fem  																																																																																																														
-3	5	3.13E+00	3.13E+00	0	0.8	-3	0	0	0	0	0.5	0	0	# cm FL	Wtlen2_Fem  																																																																																																														
1	10	4.49482	4.49482	0	0.8	-3	0	0	0	0	0.5	0	0	# age (yr)	Mat50_Fem																																																																																																														
-200	3	-104.21221	-104.21221	0	0.8	-3	0	0	0	0	0.5	0	0	# age (yr)	Mat_slope_Fem																																																																																																														
-3	50	39.00000	39.00000	0	0.8	-3	0	0	0	0	0.5	0	0	#	Eggs_scalar_Fem																																																																																																														
-3	3	0.00000	0.00000	0	0.8	-3	0	0	0	0	0.5	0	0	#	Eggs_exp_len_Fem																																																																																																														
-4	4	0.00000	1.00000	-1	99	-3	0	0	0	0	0.5	0	0	#	RecrDist_GP_1_																																																																																																														
-4	4	0.00000	1.00000	-1	99	-3	0	0	0	0	0.5	0	0	#	RecrDist_Area_1_																																																																																																														
-4	4	0.00000	1.00000	-1	99	-3	0	0	0	0	0.5	0	0	#	RecrDist_Seas_1_																																																																																																														
-4	4	0	1	-1	99	-3	0	0	0	0	0.5	0	0	#	CohortGrowDev																																																																																																														
#_seasonal_effects_on_biology_parms																																																																																																																													
#_femwtlen1	femwtlen2	mat1	mat2	fec1	fec2	Malewtlen1	malewtlen2	L1	K																																																																																																																				
0	0	0	0	0	0	0	0	0	0																																																																																																																				
#_Spawner-Recruitment																																																																																																																													
# Changed in SSv321 option 1 now option 6																																																																																																																													
3	#_SR_function																																																																																																																												
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE																																																																																																																							
2.30	13.82	7.04	7.04	0.00	1000.00	1.00	#	SR_R0																																																																																																																					
0.20	0.99	0.73	0.73	0.00	1000.00	-2.00	#	SR_steep																																																																																																																					
0.20	1.90	0.4	0.40	0.00	1000.00	-4.00	#	SR_sigmaR																																																																																																																					
-5	5	0	0	0	2	-3	#	SR_envlink																																																																																																																					
-5	5	0	0	0	1	-4	#	SR_R1_offset																																																																																																																					
-5	5	0	0	0	-1	-4	#	SR_autocorr																																																																																																																					
0	#_SR_env_link																																																																																																																												
0	#_SR_env_target_0=none;1=devs;_2=R0;_3=steepness																																																																																																																												
#																																																																																																																													
1	#do_recdev:	0=none;	1=devvector;	2=simple	deviations																																																																																																																								
1991	# first year of main recr_devs; early devs can preceed this era																																																																																																																												
2010	# last year of main recr_devs; forecast devs start in following year																																																																																																																												
3	#_recdev phase 																																																																																																																												
# The format  has been expanded and re-arranged with V3.20. to 13 options																																																																																																																													
1	# (0/1) to read 13 advanced options																																																																																																																												
1968	#_recdev_early_start (0=none; neg value makes relative to recdev_start)																																																																																																																												
4	#_recdev_early_phase																																																																																																																												
0	#_forecast_recruitment phase (incl. late recr) (0 value resets to maxphase+1)																																																																																																																												
1	#_lambda for prior_fore_recr occurring before endyr+1																																																																																																																												
1964.3397	#_last_early_yr_nobias_adj_in_MPD
1978.775	#_first_yr_fullbias_adj_in_MPD
2010	#_last_yr_fullbias_adj_in_MPD
2013.0992	#_first_recent_yr_nobias_adj_in_MPD
0.4654	# Max bias adj, use value of 1 for compatibility with earlier versions
0	# Period for recruitment cycles, if > 0 used when seasons modeled as years																																																																																																																												
-15	#min rec_dev	minimum log rec dev																																																																																																																											
15	#max rec_dev	maximum log rec dev																																																																																																																											
0																																																																																																																													
#																																																																																																																													
#Fishing Mortality info 																																																																																																																													
0.2	# F ballpark for tuning early phases disabled for option 3?																																																																																																																												
-2010	# F ballpark year(neg value to disable)																																																																																																																												
3	# F_Method:  1=Pope; 2=instan. F; 3=hybrid (hybrid is recommended)																																																																																																																												
4	# max F or harvest rate, depends on F_Method																																																																																																																												
# no additional F input needed for Fmethod 1																																																																																																																													
# read overall start F value; overall phase; N detailed inputs to read for Fmethod 2																																																																																																																													
# read N iterations for tuning for Fmethod 3 (recommend 3 to 7)																																																																																																																													
5	# N iterations for tuning F in hybrid method (recommend 3 to 7)																																																																																																																												
#Fleet Year Seas F_value se phase (for detailed setup of F_Method=2)																																																																																																																													
#																																																																																																																													
#_initial_F_parms do not try to estimate for fisheries with 0 equilibrium catch																																																																																																																													
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE																																																																																																																							
0	1.9	0.1	0.384903	0	1000	1	#																																																																																																																						
0	1.9	0	0.384903	0	1000	-1	#																																																																																																																						
0	1.9	0	0.384903	0	1000	-1	#																																																																																																																						
0	1.9	0	0.384903	0	1000	-1	#																																																																																																																						
0	1.9	0	0.384903	0	1000	-1	#																																																																																																																						
0	1.9	0	0.384903	0	1000	-1	#																																																																																																																						
0	1.9	0	0.384903	0	1000	-1	#																																																																																																																						
0	1.9	0	0.384903	0	1000	-1	#																																																																																																																						
0	1.9	0	0.384903	0	1000	-1	#																																																																																																																						
#																																																																																																																													
#_Q_setup																																																																																																																													
# SSv321 moved to data file: E=0=num/1=bio,F=err_type 																																																																																																																													
# A=do power, B=env-var, C=extra SD, D=devtype(<0=mirror, 0/1=none, 2=cons, 3=rand, 4=randwalk); 																																																																																																																													
#_A	B	C	D	SSv321	E	F																																																																																																																							
0	0	0	0	#	1	30		F1	Q for fleets (place holder) 																																																																																																																			
0	0	0	0	#	1	30		F2																																																																																																																					
0	0	0	0	#	1	30		F3																																																																																																																					
0	0	0	0	#	1	30		F4																																																																																																																					
0	0	0	0	#	1	30		F5																																																																																																																					
0	0	0	0	#	1	30		F6																																																																																																																					
0	0	0	0	#	1	30		F7																																																																																																																					
0	0	0	0	#	1	30		F8																																																																																																																					
0	0	0	0	#	1	30		F9																																																																																																																					
0	0	0	0	#	0	30		S1	Q for CPUE (not estimated with a prior) - just accept the analytical solution: Numbers, Biomass Lognormal approximation																																																																																																																				
0	0	0	0	#	0	30		S2																																																																																																																					
0	0	0	0	#	0	30		S3																																																																																																																					
0	0	0	0	#	0	30		S4																																																																																																																					
0	0	0	0	#	0	30		S5																																																																																																																					
0	0	0	0	#	0	30		S6																																																																																																																					
0	0	0	0	#	0	30		S7																																																																																																																					
0	0	0	0	#	0	30		S8																																																																																																																					
0	0	0	0	#	0	30		S9																																																																																																																					
0	0	0	0	#	0	30		S10																																																																																																																					
#_size_selex_types																																																																																																																													
# Pattern 	Retention	Male	Special (which index to mirror)												Fleet#																																																																																																														
9	0	0	0	#Patterns 9 (size) and 19 (age) -- simple double logistic, no defined peak 											1																																																																																																														
9	0	0	0	#Patterns 9 (size) and 19 (age) -- simple double logistic, no defined peak 											2																																																																																																														
1	0	0	0	# Patterns 1 (size) and 12 (age) --Simple logistic 											3																																																																																																														
9	0	0	0	#Patterns 9 (size) and 19 (age) -- simple double logistic, no defined peak 											4																																																																																																														
9	0	0	0	#Patterns 9 (size) and 19 (age) -- simple double logistic, no defined peak 											5																																																																																																														
5	0	0	4	#Pattern 5 (special) -- assing index to mirror											6																																																																																																														
5	0	0	3	#Pattern 5 (special) -- assing index to mirror											7																																																																																																														
5	0	0	5	#Pattern 5 (special) -- assing index to mirror											8																																																																																																														
5	0	0	3	#Pattern 5 (special) -- assing index to mirror											9																																																																																																														
5	0	0	4	#Pattern 5 (special) -- assing index to mirror											10																																																																																																														
5	0	0	4	#Pattern 5 (special) -- assing index to mirror											11																																																																																																														
5	0	0	2	#Pattern 5 (special) -- assing index to mirror											12																																																																																																														
5	0	0	2	#Pattern 5 (special) -- assing index to mirror											13																																																																																																														
5	0	0	3	#Pattern 5 (special) -- assing index to mirror											14																																																																																																														
5	0	0	4	#Pattern 5 (special) -- assing index to mirror											15																																																																																																														
5	0	0	1	#Pattern 5 (special) -- assing index to mirror											16																																																																																																														
5	0	0	5	#Pattern 5 (special) -- assing index to mirror											17																																																																																																														
5	0	0	1	#Pattern 5 (special) -- assing index to mirror											18																																																																																																														
5	0	0	3	#Pattern 5 (special) -- assing index to mirror											19																																																																																																														
#_age_selex_types																																																																																																																													
# Pattern 	Retention	Male	Special (which index to mirror)												Fleet#																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											1																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											2																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											3																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											4																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											5																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											6																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											7																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											8																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											9																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											10																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											11																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											12																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											13																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											14																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											15																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											16																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											17																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											18																																																																																																														
11	0	0	0	#Pattern 11 (Page 70) if want selecitivity at age = 1 for all ages including 0, then use pattern 11 and set Min age  to 0.1  in selectivity pattern for age											19																																																																																																														
#				#																																																																																																																									
################################################################################################################################################################################################################																																																																																																																													
# Length_Sel															# Length_Sel																																																																																																														
# 															# Length_Sel																																																																																																														
# F1															# Length_Sel																																																																																																														
#Patterns 9 (size) and 19 (age) -- simple double logistic, no defined peak 															# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
1	500	100	100	1	0.05	2	0	0	0	0	0	0	0		 # leftside (ascending) inflection in real (cm) units																																																																																																														
0	1	0.15	0.15	1	0.05	3	0	0	0	0	0	0	0		 # leftside (ascending) slope																																																																																																														
1	500	243	243	1	0.05	2	0	0	0	0	0	0	0		 # rightside (descending) inflection in real (cm) units																																																																																																														
0	1	0.08	0.08	1	0.05	3	0	0	0	0	0	0	0		 # rightside (descending) slope																																																																																																														
1	24	1	1	1	0.05	-88	0	0	0	0	0	0	0		 # integer setting first bin with selectivity																																																																																																														
0	1	0	0	1	0.05	-88	0	0	0	0	0	0	0		# value of 0 or 1 to select whether rightside infl is offset from leftside infl (1) or not (0)																																																																																																														
#															# Length_Sel																																																																																																														
# F2															# Length_Sel																																																																																																														
#Patterns 9 (size) and 19 (age) -- simple double logistic, no defined peak 															# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
1	500	120	120	1	0.05	2	0	0	0	0	0	0	0		 # leftside (ascending) inflection in real (cm) units																																																																																																														
0	1	0.15	0.15	1	0.05	3	0	0	0	0	0	0	0		 # leftside (ascending) slope																																																																																																														
1	500	220	220	1	0.05	2	0	0	0	0	0	0	0		 # rightside (descending) inflection in real (cm) units																																																																																																														
0	1	0.07	0.07	1	0.05	3	0	0	0	0	0	0	0		 # rightside (descending) slope																																																																																																														
1	24	1	1	1	0.05	-88	0	0	0	0	0	0	0		 # integer setting first bin with selectivity																																																																																																														
0	1	0	0	1	0.05	-88	0	0	0	0	0	0	0		# value of 0 or 1 to select whether rightside infl is offset from leftside infl (1) or not (0)																																																																																																														
#															# Length_Sel																																																																																																														
# F3															# Length_Sel																																																																																																														
# Patterns 1 (size) and 12 (age) --Simple logistic 															# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
5	500	200	200	1	0.05	2	0	0	0	0	0	0	0		#infl_for_logistic																																																																																																														
0.01	60	25	25	1	0.05	3	0	0	0	0	0	0	0		#95%width_for_logistic																																																																																																														
# 															# Length_Sel																																																																																																														
# F4															# Length_Sel																																																																																																														
#Patterns 9 (size) and 19 (age) -- simple double logistic, no defined peak 															# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
1	500	110	110	1	0.05	2	0	0	0	0	0	0	0		 # leftside (ascending) inflection in real (cm) units																																																																																																														
0	1	0.09	0.09	1	0.05	3	0	0	0	0	0	0	0		 # leftside (ascending) slope																																																																																																														
1	500	120	120	1	0.05	2	0	0	0	0	0	0	0		 # rightside (descending) inflection in real (cm) units																																																																																																														
0	1	0.05	0.05	1	0.05	3	0	0	0	0	0	0	0		 # rightside (descending) slope																																																																																																														
1	24	1	1	1	0.05	-88	0	0	0	0	0	0	0		 # integer setting first bin with selectivity																																																																																																														
0	1	0	0	1	0.05	-88	0	0	0	0	0	0	0		# value of 0 or 1 to select whether rightside infl is offset from leftside infl (1) or not (0)																																																																																																														
#															# Length_Sel																																																																																																														
# F5															# Length_Sel																																																																																																														
#Patterns 9 (size) and 19 (age) -- simple double logistic, no defined peak 															# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
1	500	210	210	1	0.05	2	0	0	0	0	0	0	0		 # leftside (ascending) inflection in real (cm) units																																																																																																														
0	1	0.05	0.05	1	0.05	3	0	0	0	0	0	0	0		 # leftside (ascending) slope																																																																																																														
1	500	210	210	1	0.05	2	0	0	0	0	0	0	0		 # rightside (descending) inflection in real (cm) units																																																																																																														
0	1	0.05	0.05	1	0.05	3	0	0	0	0	0	0	0		 # rightside (descending) slope																																																																																																														
1	24	1	1	1	0.05	-88	0	0	0	0	0	0	0		 # integer setting first bin with selectivity																																																																																																														
0	1	0	0	1	0.05	-88	0	0	0	0	0	0	0		# value of 0 or 1 to select whether rightside infl is offset from leftside infl (1) or not (0)																																																																																																														
#															# Length_Sel																																																																																																														
# F6															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# F7															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# F8															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# F9															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# S1															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# S2															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# S3															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# S4															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# S5															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# S6															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# S7															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# S8															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# S9															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# S10															# Length_Sel																																																																																																														
# mirror (Mirrored Fleet Identified Above)				Note: Match from length bin 1 to 71 (maximum population length bin #)											# Length_Sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		# Length_Sel																																																																																																														
0	10	1	1	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
10	100	71	71	0	25	-99	0	0	0	0	0.5	0	0		# Length_Sel																																																																																																														
# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel	# Length_Sel																																																																																																														
################################################################################################################################################################################################################																																																																																																																													
#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel																																																																																																														
#															#Age_sel																																																																																																														
# Using Length Based Selectivity (any pattern) and Age based selectivity Pattern 11: for Age based selectivity Pattern 11 (SS Manual Page 70) set selecitivity at age = 1 for all ages including 0, by using pattern 11 and set Min age  to 0.1  															#Age_sel																																																																																																														
#															#Age_sel																																																																																																														
# F1															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# F2															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# F3															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.1	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# F4															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# F5															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# F6															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# F7															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# F8															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# F9															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# S1															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# S2															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# S3															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# S4															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# S5															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# S6															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# S7															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# S8															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# S9															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
# S10															#Age_sel																																																																																																														
#_LO	HI	INIT	PRIOR	PR_type	SD	PHASE	env-var	use_dev	dev_minyr	dev_maxyr	dev_stddev	Block	Block_Fxn		#Age_sel																																																																																																														
0	10	0.01	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
10	100	30	0	0	25	-99	0	0	0	0	0.5	0	0		#Age_sel																																																																																																														
#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel	#Age_sel																																																																																																														
############################################################################################################################################################################################################																																																																																																																													
#2	#_env/block/dev_adjust_method (1=standard; 2=logistic trans to keep in base parm bounds)										3=standard	w/	no	bound																																																																																																															
#																																																																																																																													
#																																																																																																																													
# Tag loss and Tag reporting parameters go next																																																																																																																													
0  # TG_custom:  0=no read; 1=read if tags exist																																																																																																																													
#																																																																																																																													
1 #_Variance_adjustments_to_input_values																																																																																																																													
#F1	#F2	#F3	#F4	#F5	#F6	#F7	#F8	#F9	S1	S2	S3	S4	S5	S6	S7	S8	S9	S10																																																																																																											
0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	#_add_to_survey_CV; value added to survey CV (set to zero for no effect)																																																																																																										
0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	#_add_to_discard_CV																																																																																																										
0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	#_add_to_bodywt_CV																																																																																																										
0.0019	0.0047	0.0046	0.0573	0.0403	1	1	1	1	1	1	1	1	1	1	1	1	1	1	#_mult_by_lencomp_N; multiplier for lencomp effective N (set to 1.0 for no effect)
0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	#_mult_by_agecomp_N																																																																																																							
0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	0	#_mult_by_size-at-age_N																																																																																																						
#																																																																																																																													
1	#_maxlambdaphase		read this number of lambda values for each element below The last lambda value is used for all higher numbered phases																																																																																																																										
1	#_sd_offset		value=0 causes log(like) to omit the +log(s) term; value=1 causes log(like) to include the log(s) term for CPUE, discard, meanbodywt, recruitment deviations.																																																																																																																										
#																																																																																																																													
21	# number of changes to make to default Lambdas (default value is 1.0)																																																																																																																												
# Like_comp codes:  1=surv; 2=disc; 3=mnwt; 4=length; 5=age; 6=WtFreq; 7=sizeage; 8=catch; 																																																																																																																													
# 9=init_equ_catch; 10=recrdev; 11=parm_prior; 12=parm_dev; 13=CrashPen; 14=Morphcomp; 15=Tag-comp; 16=Tag-negbin																																																																																																																													
#like_comp fleet/survey  phase  value  wtfreq_method																																																																																																																													
#																																																																																																																													
# lambdas																																																																																																																													
#Likelihood-Component	Fleet/Survey	Phase	Lambda-value	Sizefreqmethod					Fleet Index	Fleet Code	Fleet Name	Description																																																																																																																	
1	10	1	0	1	#	#CPUE_survey			10	S1	US-Log*	US logbook (1986-2013)		numbers	*Not fit in likelihood (Same data represented by US-Obs)																																																																																																														
1	11	1	1	1	#	#CPUE_survey			11	S2	US-Obs	US observer (1992-2013)		numbers																																																																																																															
1	12	1	1	1	#	#CPUE_survey			12	S3	JPN-LL-N-e	JP-LL (1971-1993)		numbers																																																																																																															
1	13	1	1	1	#	#CPUE_survey			13	S4	JPN-LL-N-l	JP-LL (1994-2013)		numbers																																																																																																															
1	14	1	0	1	#	#CPUE_survey			14	S5	IRL-Rec*	IRL-Rec (1980-2006)		numbers	*Prelininary - not fit in likelihood																																																																																																														
1	15	1	1	1	#	#CPUE_survey			15	S6	US-Obs-cru** (Aires-da-Silva et al)	[1957-1970], (1971-1991) [1992-2000]		numbers	** Only the years 1971-1991 included in the likelihood																																																																																																														
1	16	1	1	1	#	#CPUE_survey			16	S7	PRT-LL	POR-LL (1997-2013)		biomass																																																																																																															
1	17	1	1	1	#	#CPUE_survey			17	S8	VEN-LL	VEN-LL (1994-2013)		numbers																																																																																																															
1	18	1	1	1	#	#CPUE_survey			18	S9	ESP-LL-N	ESP-LL-N (1997-2013)		biomass																																																																																																															
1	19	1	1	1	#	#CPUE_survey			19	S10	CTP-LL-N (Pending)*	Chinese Taipei (2004-2013)		numbers	Prelininary - but also fit in likelihood																																																																																																														
4	1	1	1	1	#	#_lencomp			1	F1	EU	EU España+Portugal (1997-2013)																																																																																																																	
4	2	1	1	1	#	#_lencomp			2	F2	JPN	Japan (1994-2013)																																																																																																																	
4	3	1	1	1	#	#_lencomp			3	F3	CTP	Chinese Taipei (2004-2013)																																																																																																																	
4	4	1	1	1	#	#_lencomp			4	F4	USA	USA (1992-2013)																																																																																																																	
4	5	1	1	1	#	#_lencomp			5	F5	VEN	Venezuela (1994-2013)																																																																																																																	
9	6	1	1	1	#	#_init_equ_catch			Estimate initial F	F1	EU	Average catch of EU España+Portugal+Japan+Chinese Taipei (1971-1980)																																																																																																																	
10	1	1	1	1	#	#_recruitments																																																																																																																							
11	1	1	1	1	#	#_parameter-priors		Set this to 0 to test the influence of priors																																																																																																																					
12	1	1	1	1	#	#_parameter-dev-vectors																																																																																																																							
13	1	1	1	1	#	#_crashPenLambda																																																																																																																							
17	1	1	0	1	#	Previously Fballpark automatically decayed to a lambda of 0.0 in the final phase.  Now it is under explicit control as lambda index 17, so can remain positive through the end of the run unless set to 0 here
0	# (0/1) read specs for extra stddev reporting 																																																																																																																												
 # 0 1 -1 5 1 5 1 -1 5 # placeholder for selex type, len/age, year, N selex bins, Growth pattern, N growth ages, NatAge_area(-1 for all), NatAge_yr, N Natages																																																																																																																													
 # -1 1 1 1 1 # placeholder for vector of selex bins to be reported																																																																																																																													
 # -1 1 1 1 1 # placeholder for vector of growth ages to be reported																																																																																																																													
 # -1 1 1 1 1 # placeholder for vector of NatAges ages to be reported																																																																																																																													
999																																																																																																																													
