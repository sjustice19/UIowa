getCorrectedElist = function(wd = NULL, edgelist, epsilon = 1e-4)
{
    library(tidyverse)
    library(parallel)
    library(compiler)
    # Calculate initial estimates of q's, p's, and ptilde's
    combined1 = edgelist
    ages = unique(combined1$age_group)
    ahaids = unique(combined1 %>% pull(source_ahaid))
    J = length(ahaids)
    ahaids_full = unique(combined1 %>% pull(target_ahaid))
    combined2 = combined1 %>% mutate(q = (visitlink_dis * visitlink_ad) / (total_dis * total_ad))
    combined2$q[is.na(combined2$q) == TRUE] = 0
    dis1 = combined2 %>% group_by(age_group, source_ahaid) %>%
           summarise(total_count = sum(count, na.rm = TRUE))
    dis2 = combined2 %>% filter(target_ahaid == "HOME") %>%
           group_by(age_group, source_ahaid, target_ahaid) %>%
           summarise(total_dis = sum(total_dis, na.rm = TRUE)) %>%
           inner_join(dis1, by = c("source_ahaid" = "source_ahaid", "age_group" = "age_group")) %>%
           mutate(p = (total_dis - total_count) / total_dis) %>%
           select(age_group, source_ahaid, target_ahaid, p)
    dis2$p[is.na(dis2$p) == TRUE] = 0
    combined3 = combined2 %>% group_by(age_group, source_ahaid, target_ahaid) %>%
                summarise(total_dis = sum(total_dis, na.rm = TRUE),
                          total_count = sum(count, na.rm = TRUE)) %>%
                left_join(dis2, by = c("source_ahaid" = "source_ahaid",
                                       "target_ahaid" = "target_ahaid",
                                       "age_group" = "age_group")) %>%
                mutate(pnew = total_count / total_dis)
    combined3$p[is.na(combined3$p) == TRUE] = 0
    combined3$pnew[is.na(combined3$pnew) == TRUE] = 0
    combined4 = combined3 %>% mutate(p = p + pnew) %>% select(-total_dis, -total_count, -pnew)
    combined5 = combined2 %>% left_join(combined4, by = c("source_ahaid" = "source_ahaid",
                                                          "target_ahaid" = "target_ahaid",
                                                          "age_group" = "age_group")) %>%
                mutate(ptilde = p * (1 - q)) %>%
                select(-visitlink_dis, -no_visitlink_dis, -visitlink_ad, -no_visitlink_ad, -total_ad, -q,
                       -p)
    ptildesum = combined5 %>% group_by(age_group, source_ahaid, year, month) %>%
                summarise(ptilde_s = sum(ptilde, na.rm = TRUE))
    combined6 = combined5 %>% left_join(ptildesum, by = c("source_ahaid" = "source_ahaid",
                                                          "age_group" = "age_group",
                                                          "year" = "year",
                                                          "month" = "month")) %>%
                mutate(ptilde = ptilde / ptilde_s) %>% select(-ptilde_s)
    combined6$ptilde[is.na(combined6$ptilde) == TRUE] = 0
    store_probs = combined4 %>% mutate(pnew = 0)
    # Run EM Algorithm
    it = 1
    check = 1
    eps = epsilon
    while(check > eps)
    {
        nCores = detectCores()
        cl = makeCluster(nCores)
        clusterExport(cl, varlist = c("ages", "combined6", "J", "ahaids", "ahaids_full", "store_probs"),
                      envir = environment())
        clusterEvalQ(cl, library("tidyverse"))
        # Define function to iteratively update the p's (by age group)
        EM = function(i)
        {
            store_probs = filter(store_probs, age_group == ages[i])
            temp1 = filter(combined6, age_group == ages[i])
            for(j in 1 : J)
            {
                temp2 = filter(temp1, source_ahaid == ahaids[j])
                c = rep(0, J + 1)
                for(k in 1 : (J + 1))
                {
                    temp3 = filter(temp2, target_ahaid == ahaids_full[k])
                    for(m in 1 : nrow(temp3))
                    {
                        n = temp3 %>% filter(year == temp3$year[m], month == temp3$month[m]) %>%
                            select(count)
                        delta = temp2 %>% filter(year == temp3$year[m], month == temp3$month[m]) %>%
                                select(total_dis)
                        sum_n = temp2 %>% filter(year == temp3$year[m], month == temp3$month[m],
                                                 target_ahaid != "HOME") %>%
                                summarise(count = sum(count, na.rm = TRUE))
                        p = temp3 %>% filter(year == temp3$year[m], month == temp3$month[m]) %>%
                            select(ptilde)
                        c[k] = c[k] + n$count[1] + (delta$total_dis[1] - sum_n$count[1]) * p$ptilde[1]
                    }
                }
                if(sum(c) != 0)
                {
                    for(k in 1 : (J + 1))
                    {
                        store_probs$pnew[store_probs$source_ahaid == ahaids[j] &
                                         store_probs$target_ahaid == ahaids_full[k]] = c[k] / sum(c)
                    }
                }
                else
                {
                    for(k in 1 : (J + 1))
                    {
                        store_probs$pnew[store_probs$source_ahaid == ahaids[j] &
                                         store_probs$target_ahaid == ahaids_full[k]] = 0
                    }
                }
            }
            return(store_probs)
        }
        EMC = cmpfun(EM)
        store_probs = bind_rows(parLapply(cl, 1 : length(ages), EMC))
        stopCluster(cl)
        # Update ptilde's based on new estimates for p's
        combined5 = combined2 %>% left_join(store_probs, by = c("source_ahaid" = "source_ahaid",
                                                                "target_ahaid" = "target_ahaid",
                                                                "age_group" = "age_group")) %>%
                    mutate(ptilde = pnew * (1 - q)) %>%
                    select(-visitlink_dis, -no_visitlink_dis, -visitlink_ad, -no_visitlink_ad, -total_ad,
                           -q, -p, -pnew)
        ptildesum = combined5 %>% group_by(age_group, source_ahaid, year, month) %>%
                    summarise(ptilde_s = sum(ptilde, na.rm = TRUE))
        combined6 = combined5 %>% left_join(ptildesum, by = c("source_ahaid" = "source_ahaid",
                                                              "age_group" = "age_group",
                                                              "year" = "year",
                                                              "month" = "month")) %>%
                    mutate(ptilde = ptilde / ptilde_s) %>% select(-ptilde_s)
        combined6$ptilde[is.na(combined6$ptilde) == TRUE] = 0
        # Update convergence criterion and write latest estimates of p's to csv file as precaution (if
        # desired)
        check = max(abs(store_probs$pnew - store_probs$p))
        if(is.null(wd) == FALSE)
        {
            write_csv(store_probs, paste0(wd, "store_probs_", it, ".csv"))
        }
        store_probs = store_probs %>% mutate(p = pnew)
        print(check)
        print(it)
        it = it + 1
    }
    # Compute and return estimated (i.e., corrected) transfer counts using final estimates of p's from EM
    # algorithm
    total_count = combined6 %>% filter(target_ahaid != "HOME") %>%
                  group_by(age_group, source_ahaid, year, month) %>%
                  summarise(total_count = sum(count, na.rm = TRUE))
    combined7 = combined6 %>% left_join(total_count, by = c("source_ahaid" = "source_ahaid",
                                                            "age_group" = "age_group",
                                                            "year" = "year",
                                                            "month" = "month"))
    combined8 = combined7 %>% mutate(expected = (total_dis - total_count) * ptilde)
    complete = combined8 %>% mutate(estimated_count = count + expected) %>%
               filter(source_ahaid != target_ahaid, target_ahaid != "HOME") %>%
               select(source_ahaid, target_ahaid, age_group, year, month, count, estimated_count)
    return(complete)
}