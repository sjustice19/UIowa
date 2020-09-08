Edgelist Corrections:
 The following document details the getCorrectedElist() function written in R which takes an uncorrected edgelist of patient transfers from a state with VisitLink missingness and returns a corrected edgelist resulting from the EM algorithm. Note that the algorithm is computationally intensive, and thus high performance computing environments are highly recommended for using the function, especially for large states, states with many years of data, and/or states with high rates of VisitLink missingness. The function relies heavily on the tidyverse package, and also uses the parallel and compiler packages to speed up the EM algortihm's convergence.

Preprocessing of edgelist: In preparation for our analysis, we performed some initial data cleaning and filtering actions on the edgelist for a given state before feeding it to the getCorrectedElist() function. These actions were in addition to the work that needs to be done to compile the raw HCUP SID data into the form of the edgelist detailed below. We list the actions for the information of the reader/user:

	Excluded any admissions that had missing observations for the dshospid, age, amonth, ayear, dmonth, or year variables (these are variables from
	the raw HCUP SID data), since all of these variables had to be present for an admission to be included in the analysis.

	Cleaned up the hospital ids since some states have inconsistencies (i.e., many-to-many relationships) between their dshospid and ahaid
	variables; this	cleaning resulted in a single ahaid for each admission (and thus a source and target ahaid for each transfer).

	Excluded any hospitals (i.e., ahaids) that did not record at least one admission in each month over the timespan of the data.

	Excluded any transfers that fell into different age groups at their source and target hospitals (see the definition of the variable age_group
	below); this was necessary since the EM algorithm estimates were stratified by age group.

	Excluded all admissions and discharges in the last month of the data (for example, since we analyzed Iowa data from 2009-2015, we excluded all
	December 2015 admissions/discharges); this was done since some admissions that take place during the last month of the data are not discharged
	in this month and hence do not appear in the data, resulting in an undercount of transfers for the month.

Inputs to function:

	wd: The working directory in which to write intermediate EM algorithm estimates as csv files; if this argument is left as its default NULL
	value, the function does not create these files.

	edgelist: A tibble with the following variables (note that the tibble is assumed to have a row corresponding to each unique combination of
	age_group, source_ahaid, target_ahaid, year, and month in the data):

		age_group: a factor consisting of the four levels "0", "1-17", "18-64", and "65+"

		source_ahaid: the id for the transfer's source hospital

		target_ahaid: the id for the transfer's target hospital

		year: the year in which the transfer took place

		month: the month in which the transfer took place

		visitlink_dis: the total number of discharges with a VisitLink present (for a particular combination of age_group, source_ahaid,
		year, and month)

		no_visitlink_dis: the total number of discharges with no VisitLink present (again, for a particular combination of age_group,
		source_ahaid, year, and month)

		total_dis: the overall number of discharges (sum of the previous two variables)

		visitlink_ad: the total number of admissions with a VisitLink present (for a particular combination of age_group, target_ahaid,
		year, and month)

		no_visitlink_ad: the total number of admissions with no VisitLink present (again, for a particular combination of age_group,
		target_ahaid, year, and month)

		total_ad: the overall number of admissions (sum of the previous two variables)

		count: the observed number of transfers (for a particular combination of age_group, source_ahaid, target_ahaid, year, and month)

	epsilon: convergence criterion for EM algorithm (1e-4 by default)

Output of function: A tibble with the variables source_ahaid, target_ahaid, age_group, year, month, count, and estimated_count. The first six variables are as detailed above. The estimated_count variable gives the corrected number of transfers as estimated by the EM algorithm (note that estimated_count >= count).