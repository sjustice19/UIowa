library(tidyverse)
ca = src_sqlite("~/ca_20181129.db")
years = 2003 : 2011
# Pull data from core tables
core = vector('list', length(years))
p = progress_estimated(length(years))
for(year in years)
{
    d = tbl(ca, paste0("CA_SID_", year, "_core")) %>%
        select(dshospid, key, ayear, amonth, age, npr, pay1,
               race, died, los_x, ndx, female, totchg, asource)
    core[[year - (years[1] - 1)]] = d %>% collect(n = Inf)
    p$tick()$print()
}
core_data = bind_rows(core)
core_data
# Fix AHAIDs
load("~/ca_hosps.RData")
hosps
core_data_new = inner_join(core_data, hosps, by = c("dshospid" = "dshospid"))
# Group data
grouped = core_data_new %>% filter(is.na(amonth) == FALSE) %>% group_by(ahaid, ayear, amonth)
# Pull data from dx tables
dx = vector('list', length(years))
p = progress_estimated(length(years))
for(year in years)
{
    d = ca %>%
        tbl(paste0("CA_SID_", year, "_dx")) %>%
        select(key, dxccs, dx, dx_number) %>%
        collect(n = Inf)
    dx[[year - (years[1] - 1)]] = d
    p$tick()$print()
}
dx_data = bind_rows(dx)
# Create main hospital summary (corresponding to primary diagnoses)
dx_data_primary = dx_data %>% filter(dx_number == 1)
summ = inner_join(grouped, dx_data_primary, by = c("key" = "key")) %>%
       mutate(over_65 = as.numeric(age > 65),
              procedures = as.numeric(npr > 0),
              medicare = as.numeric(pay1 == 1),
              medicaid = as.numeric(pay1 == 2),
              private_ins = as.numeric(pay1 == 3),
              self_pay = as.numeric(pay1 == 4),
              white = as.numeric(race == 1),
              black = as.numeric(race == 2),
              hispanic = as.numeric(race == 3),
              asian = as.numeric(race == 4),
              cdi = as.numeric(dx == "00845"),
              ami = as.numeric(dxccs == 100),
              emergency_department = as.numeric(asource == 1),
              another_hospital = as.numeric(asource == 2),
              other_health_facility = as.numeric(asource == 3),
              court_or_law_enforcement = as.numeric(asource == 4),
              routine = as.numeric(asource == 5)) %>%
       summarise(total_admissions = n(), mean_age = mean(age, na.rm = TRUE),
                 pct_over_65 = mean(over_65, na.rm = TRUE), pct_died = mean(died, na.rm = TRUE),
                 mean_los_x = mean(los_x, na.rm = TRUE), median_los_x = median(los_x, na.rm = TRUE),
                 mean_ndx = mean(ndx, na.rm = TRUE), median_ndx = median(ndx, na.rm = TRUE),
                 pct_with_procedures = mean(procedures, na.rm = TRUE), mean_npr = mean(npr, na.rm = TRUE),
                 median_npr = median(npr, na.rm = TRUE), pct_medicare = mean(medicare, na.rm = TRUE),
                 pct_medicaid = mean(medicaid, na.rm = TRUE),
                 pct_private_ins = mean(private_ins, na.rm = TRUE),
                 pct_self_pay = mean(self_pay, na.rm = TRUE), pct_female = mean(female, na.rm = TRUE),
                 pct_white = mean(white, na.rm = TRUE), pct_black = mean(black, na.rm = TRUE),
                 pct_hispanic = mean(hispanic, na.rm = TRUE), pct_asian = mean(asian, na.rm = TRUE),
                 mean_total_charge = mean(totchg, na.rm = TRUE), cases_cdi_primary = sum(cdi, na.rm = TRUE),
                 cases_ami_primary = sum(ami, na.rm = TRUE),
                 emergency_department = sum(emergency_department, na.rm = TRUE),
                 another_hospital = sum(another_hospital, na.rm = TRUE),
                 other_health_facility = sum(other_health_facility, na.rm = TRUE),
                 court_or_law_enforcement = sum(court_or_law_enforcement, na.rm = TRUE),
                 routine = sum(routine, na.rm = TRUE))
# Turn NaNs into NAs
is.nan.data.frame = function(x)
{
    do.call(cbind, lapply(x, is.nan))
}
summ[is.nan.data.frame(summ)] = NA
# Count secondary diagnoses of CDI and AMI
dx_data_secondary = dx_data %>% filter(dx_number > 1)
cdi_secondary = grouped %>% select(key, ahaid, ayear, amonth) %>%
                inner_join(dx_data_secondary, by = "key") %>%
                mutate(cdi = as.numeric(dx == "00845"), ami = as.numeric(dxccs == 100)) %>%
                summarise(cases_cdi_secondary = sum(cdi, na.rm = TRUE),
                          cases_ami_secondary = sum(ami, na.rm = TRUE))
# Join up main hospital summary with secondary diagnoses to create final summary
both = full_join(summ, cdi_secondary,
                 by = c("ahaid" = "ahaid", "ayear" = "ayear", "amonth" = "amonth"))
both$cases_cdi_secondary[is.na(both$cases_cdi_secondary) == TRUE] = 0
both$cases_ami_secondary[is.na(both$cases_ami_secondary) == TRUE] = 0
both
write_csv(both, "~/ca_hospital_summary.csv")