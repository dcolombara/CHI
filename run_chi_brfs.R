#generate BRFSS data for CHI with custom data
rm(list=ls())
pacman::p_load(dplyr, foreign, survey, srvyr, epiDisplay, data.table, janitor, rads, naniar, stringr)
library(labelled, dtsurvey)
setwd("c:/R_learning/CHI_R")

#Read Stata file, can only translate Stata version 12 or earlier, other packages can read later versions
brfsraw <- read.dta(file="S:/WORK/surveys/brfs/prog_all/kc0020_finalz.dta", warn.missing.labels = FALSE)
brfsraw <- subset(brfsraw, year>=2016)

#2016-2018: diab2, disab2, medcost, fmd, obese, smoker1
#2016, 2018, 2020: _crcrec, denvst1, firearm4
#fnotlast (2018, 2019, 2020); #soda1 (2018, 2019) continuous
names(brfsraw)<-gsub("^_", "x_", names(brfsraw)) #change variable names start with "_"
brfs <- brfsraw[ , c("year", "x_ststr", "kcwt_llcp", "hracode", "hra2code", "ccreg","income6",
                     "age", "age7", "sex", "sexorien", "mrace", "mracex", "hispanic", "veteran",
                     "x_crcrec", "denvst1", "firearm4", "fnotlast", "diab2", "disab2",
                     "medcost", "menthlth", "obese", "smoker1", "soda1")]
str(brfs)
#recode the following integer variables into character variables
brfs <- brfs %>% mutate_at(vars("x_crcrec", "disab2"), list(~recode(., '0'='No', '1'="Yes", .default ="NA")))
brfs <- brfs %>% mutate(fnotlast = case_when(fnotlast=='Often true' | fnotlast=='Sometimes true' ~'Yes',
        fnotlast=='Never true' ~ 'No', fnotlast=="Don't know/Not sure" | fnotlast=='Refused' ~'NA'))
brfs <- brfs %>% mutate(menthlth = case_when(menthlth<=13 ~'No', menthlth<=30 ~ 'Yes'))
brfs <- brfs %>% mutate(soda1 = case_when(soda1==888 ~'No', 
                                        soda1>=201 & soda1<=206 ~'No',
                                        soda1>=301 & soda1<=330 ~'No',
                                        soda1>=101 & soda1<=199 ~'Yes',
                                        soda1>=207 & soda1<=220 ~'Yes',
                                        soda1>=330 & soda1<=380 ~'Yes'))

#recode demographic variable
brfs <- brfs %>% mutate(age7 = case_when(age7=="18-24" ~ "18-24",
                                         age7=="25-34" | age7=="35-44" ~"25-44", 
                                         age7=="45-54" | age7=="55-64" ~"45-64",
                                         age7=="65-74" ~ "65074",
                                         age7=="75+" ~ "75+",
                                         TRUE ~ 'NA'))

brfs <- brfs %>% mutate(sexorien = case_when(sexorien=="A. Heterosexual, that is, straight" ~"Heterosexual", 
                                          sexorien=="B. Homosexual, that is gay or lesbian" | 
                                            sexorien=="C. Bisexual" ~"LGB", 
                                          TRUE ~ 'NA'))
brfs$mrace <- as.character(brfs$mrace)
brfs <- brfs %>% mutate(mrace = case_when(mrace=="Other" | mrace=="Unknown" ~"Other/Unknown", 
                                          TRUE ~ mrace))

brfs$veteran <- as.character(brfs$veteran)
brfs <- brfs %>% mutate(veteran = case_when(veteran=="Don't know/Not sur" | veteran=="Refused" ~"", 
                                          TRUE ~ veteran))

brfs <- subset(brfs, hracode!="NA") #exclude cases with hracode missing
brfs <- brfs %>% mutate(hra2  = case_when(hra2code=='Auburn' ~'Auburn city',
                                          hra2code=='Seattle' ~'Seattle city',
                                          hra2code=='Bellevue' ~'Bellevue city',
                                          hra2code=='Federal Way' ~'Federal Way city',
                                          hra2code=='Kent' ~'Kent city',
                                          hra2code=='Kirkland' ~'Kirkland city',
                                          hra2code=='Renton' ~'Renton city',
                                          TRUE ~'Other cities'))

brfs <- brfs %>% rename("FMD"="menthlth")
brfs <- brfs %>% rename("SSB" = "soda1")
brfs <- brfs %>% rename("age5"="age7")
brfs <- brfs %>% rename("LGB" = "sexorien")
brfs <- brfs %>% rename("race3" = "mrace")
brfs <- brfs %>% rename("race4" = "mracex")     
brfs <- brfs %>% rename("bigcities" = "hra2")     
brfs$all <- as.character("Total")

#--------------------------------
brfs <- brfs[complete.cases(brfs$kcwt_llcp), ]     #drop cases with missing weight
#myvars <- c("diab2", "disab2","medcost", "FMD", "obese", "smoker1", 
#             "x_crcrec", "denvst1", "firearm4", "fnotlast", "SSB")
myvars <- c("diab2", "smoker1") 

#brfs[, myvars] <- lapply(brfs[, myvars], as.character)
#brfs$hracode <- as.character(brfs$hracode)
#byvars <- c("all", "age5", "sex", "race3", "hispanic", "race4", "LGB", "income6", "veteran",
#            "ccreg",  "bigcities",  "hracode")
byvars <- c("all", "age5")

options(survey.lonely.psu = "adjust")
brfskc <- dtsurvey(brfs, 1, weight= "kcwt_llcp", strata="x_ststr")
#-----Run data for 2016-2020-----

calc(ph.data = brfskc,
     what = c("mean", "se", "rse", "numerator", "denominator"),
     metrics = c("all", "age5"),
     proportion = T,
     by = "all", "age5")[]


#----------------------------------------
result <- subset(result, byvar_level!="Other cities")
res_x <- subset(result, subset=level=="Yes", select= -c(level))
res_x <- res_x %>% rename("indicator_key"="variable", "cat1" = "byvar", "cat1_group"="byvar_level", 
                          "result"="mean", "se"="mean_se", "lower_bound"="mean_lower", "upper_bound"="mean_upper")

res_x <- res_x %>% mutate(tab = case_when(cat1=='all' ~'_kingcounty', TRUE ~as.character("demgroups")))
res_x <- res_x %>% mutate(cat1 = case_when(cat1=='all' ~'King County', cat1=="hra2" ~"Big cities",
                                                   TRUE ~"Cities/neighborhoods"))
res_x <- res_x %>% mutate(cat1_group= case_when(cat1_group=='Total' ~'King County', TRUE ~as.character(cat1_group)))

#-----King County average for significance comparison-----
res_kc <- subset(res_x, subset=res_x$tab=="_kingcounty", 
                    select= c("indicator_key", "result", "lower_bound", "upper_bound" ))
res_kc <- res_kc %>% rename("kc_result" = "result", "kc_lower" = "lower_bound", "kc_upper"="upper_bound")

#-----ranking HRA level data by result
res_hra <- subset(res_x, subset=res_x$cat1=="Cities/neighborhoods")
res_hra <- res_hra %>% group_by(year, indicator_key) %>% mutate(ranks=rank(-result))
res_oth <- subset(res_x, subset=res_x$cat1!="Cities/neighborhoods")
res_all1 <- plyr::rbind.fill(res_hra, res_oth)

res_all2 <-merge(res_all1, res_kc, by="indicator_key")
res_all2 <- res_all2 %>% mutate(comparison_with_kc = case_when(upper_bound < kc_lower ~as.character("lower"), 
                                          lower_bound > kc_upper ~as.character("higher"), 
                                          TRUE ~as.character("not different")))

res_all2 <- res_all2 %>% mutate(significance = case_when(upper_bound < kc_lower ~as.character("*"), 
                                                  lower_bound > kc_upper ~as.character("*"), 
                                                  TRUE ~as.character("")))
res_all2 <- res_all2 %>% mutate(suppression= case_when(denominator < 50 ~as.character("^"), TRUE ~as.character("NA")))
res_all2 <- res_all2 %>% mutate(caution = case_when(rse >=30 ~as.character("*"), TRUE ~as.character("NA")))
res_all2[c("result", "lower_bound", "upper_bound")][which(res_all2$denominator<50), ] <-NA

res_all2 <- res_all2 %>% mutate(comparison_with_kc = case_when(tab=='_kingcounty' ~"NA", TRUE ~comparison_with_kc))
res_all2 <- res_all2 %>% mutate(significance = case_when(tab=='_kingcounty' ~"NA", TRUE ~significance))
res_all2 <- res_all2 %>% mutate(caution = case_when(rse >=30 ~as.character("*"), TRUE ~as.character("NA")))
res_all2 <- subset(res_all2, select=-c(kc_result, kc_lower, kc_upper))
res_all2 <- res_all2 %>% dplyr::filter(!is.na(cat1_group))

#res_wa <- read.csv("resultwa.csv")
#res_all3 <- bind_rows(res_all2, res_wa)  #function in dplyr to merge 2 dfs with different number of columns

res_brfs <- res_all2
res_brfs$year <- "2016-2020"
res_brfs <- res_brfs %>% mutate(year= case_when(indicator_key=='fnotlast' ~'2018, 2019 & 2020', 
                                                indicator_key=="SSB" ~'2018, 2019',
                                                TRUE ~as.character(year)))

res_brfs$data_source <- "brfss"
res_brfs$source_date <- "2021-0901"
res_brfs$run_date <- "2022-0205"

res_brfs <- res_brfs[, c("data_source", "indicator_key", "tab", "year", "cat1", "cat1_group", "result",
                         "lower_bound", "upper_bound", "se", "rse", "numerator", "denominator", "comparison_with_kc",
                         "significance", "caution", "suppression", "ranks", "source_date", "run_date")]
res_brfs[order(res_brfs$indicator_key, res_brfs$tab, res_brfs$cat1), ]
write.csv(res_brfs, "chi_brfs.csv", row.names = F)