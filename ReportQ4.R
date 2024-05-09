


### All the data and configurations: 
##############################################################
library(tidyverse)
library(readr)
library(here)
library(haven)
library(emmeans)
library(lmerTest)
library(lme4)
library(plotly)
library(maps)
library(mapproj)
library(usmap)
library(knitr)
library(broom)
library(modelr)
library(gridExtra)

twenty022 <- read_xpt(here::here("LLCP2022.XPT ")) ## space after T 
twenty021 <- read_xpt(here::here("LLCP2021.XPT"))
twenty020 <- read_xpt(here::here("LLCP2020.XPT"))
twenty019 <- read_xpt(here::here("LLCP2019.XPT"))

## 2022: 
twenty022 = twenty022 |> select(
  IYEAR, '_STATE', SEXVAR, MARITAL, VETERAN3, INCOME3, "_BMI5", 
  EDUCA, RENTHOM1, EMPLOY1, CHILDREN, CAREGIV1, CRGVREL4, CRGVLNG1, CRGVPRB3, SDHEMPLY, FOODSTMP, SDHFOOD1, SDHBILLS, SDHUTILS, SOMALE, TRNSGNDR, RRCLASS3, "_METSTAT", "_URBSTAT", "_IMPRACE", "_AGE_G",
  GENHLTH, PHYSHLTH, MENTHLTH, POORHLTH, EXERANY2, SLEPTIM1, LASTDEN4, CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, CHCSCNC1, CHCOCNC1, CHCCOPD3, ADDEPEV3, CHCKDNY2, HAVARTH4, DIABETE4, DEAF, BLIND, DIFFWALK, DIFFDRES, DIFFALON, PREDIAB2,
  DIABTYPE, CNCRTYP2,  LSATISFY, EMTSUPRT, SDHISOLT, SDHSTRE1, 
  SMOKE100, SMOKDAY2, USENOW3, ECIGNOW2, LCSFIRST, LCSLAST, LCSNUMCG, LCSSCNCR, ALCDAY4, AVEDRNK3, DRNK3GE5, MAXDRNKS, COPDSMOK, MARIJAN1, MARJSMOK, LASTSMK2, 
  PRIMINSR, MEDCOST1, 
  ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, 
  ACEDIVRC, ACEPUNCH, ACEHURT1, ACESWEAR,
  ACETOUCH, ACETTHEM, ACEHVSEX, ACEADSAF, 
  ACEADNED) |> 
  filter(!is.na(ACEDEPRS) | !is.na(ACEDRINK) | !is.na(ACEDRUGS) | !is.na(ACEPRISN) | 
           !is.na(ACEDIVRC) | !is.na(ACEPUNCH) | !is.na(ACEHURT1) | !is.na(ACESWEAR) | 
           !is.na(ACETOUCH) | !is.na(ACETTHEM) | !is.na(ACEHVSEX) | !is.na(ACEADSAF) |
           !is.na(ACEADNED)) |>
  rename("state" = "_STATE",
         "age" = "_AGE_G",
         "race" = "_IMPRACE",
         "sex" = SEXVAR,
         "MentalIllness" = ACEDEPRS,
         "Alcoholic" = ACEDRINK,
         "SubstanceAddiction" = ACEDRUGS,
         "Incarcerated" = ACEPRISN,
         "SeparationDivorce" = ACEDIVRC,
         "DomesticViolence" = ACEPUNCH,
         "PhysicalAbuse" = ACEHURT1,
         "EmotionalAbuse" = ACESWEAR,
         "SexualAbuse1" = ACETOUCH,
         "SexualAbuse2" = ACETTHEM,
         "SexualAbuse3" = ACEHVSEX,
         "Neglect1" = ACEADSAF,
         "Neglect2" = ACEADNED) |>
  mutate(MentalIllness = case_when(
    MentalIllness == 1 ~ 1,
    MentalIllness == 2 ~ 0,
    MentalIllness == 7 ~ NA,
    MentalIllness == 9 ~ NA)) |>
  mutate(Alcoholic = case_when(
    Alcoholic == 1 ~ 1,
    Alcoholic == 2 ~ 0,
    Alcoholic == 7 ~ NA,
    Alcoholic == 9 ~ NA)) |> 
  mutate(SubstanceAddiction = case_when(
    SubstanceAddiction == 1 ~ 1,
    SubstanceAddiction == 2 ~ 0,
    SubstanceAddiction == 7 ~ NA,
    SubstanceAddiction == 9 ~ NA)) |>
  mutate(Incarcerated = case_when(
    Incarcerated == 1 ~ 1,
    Incarcerated == 2 ~ 0,
    Incarcerated == 7 ~ NA,
    Incarcerated == 9 ~ NA)) |> 
  mutate(SeparationDivorce = case_when(
    SeparationDivorce == 1 ~ 1,
    SeparationDivorce == 2 ~ 0,
    SeparationDivorce == 7 ~ NA,
    SeparationDivorce == 8 ~ NA,
    SeparationDivorce == 9 ~ NA)) |>
  mutate(DomesticViolence = case_when(
    DomesticViolence == 1 ~ 0,
    DomesticViolence == 2 ~ 1, 
    DomesticViolence == 3 ~ 1,
    DomesticViolence == 7 ~ NA,
    DomesticViolence == 9 ~ NA)) |>
  mutate(PhysicalAbuse = case_when(
    PhysicalAbuse == 1 ~ 0,
    PhysicalAbuse == 2 ~ 1,
    PhysicalAbuse == 3 ~ 1,
    PhysicalAbuse == 7 ~ NA,
    PhysicalAbuse == 9 ~ NA)) |>
  mutate(EmotionalAbuse = case_when(
    EmotionalAbuse == 1 ~ 0,
    EmotionalAbuse == 2 ~ 1, 
    EmotionalAbuse == 3 ~ 1,
    EmotionalAbuse == 7 ~ NA,
    EmotionalAbuse == 9 ~ NA)) |>
  mutate(SexualAbuse1 = case_when(
    SexualAbuse1 == 1 ~ 0,
    SexualAbuse1 == 2 ~ 1,
    SexualAbuse1 == 3 ~ 1,
    SexualAbuse1 == 7 ~ NA,
    SexualAbuse1 == 9 ~ NA)) |>
  mutate(SexualAbuse2 = case_when(
    SexualAbuse2 == 1 ~ 0,
    SexualAbuse2 == 2 ~ 1,
    SexualAbuse2 == 3 ~ 1,
    SexualAbuse2 == 7 ~ NA,
    SexualAbuse2 == 9 ~ NA)) |> 
  mutate(SexualAbuse3 = case_when(
    SexualAbuse3 == 1 ~ 0,
    SexualAbuse3 == 2 ~ 1,
    SexualAbuse3 == 3 ~ 1,
    SexualAbuse3 == 7 ~ NA,
    SexualAbuse3 == 9 ~ NA)) |>
  mutate(Neglect1 = case_when(
    Neglect1 == NA ~ NA,
    Neglect1 == 1 ~ 0,
    Neglect1 == 2 ~ 1,
    Neglect1 == 3 ~ 1,
    Neglect1 == 4 ~ 1,
    Neglect1 == 5 ~ 1,
    Neglect1 == 7 ~ NA,
    Neglect1 == 9 ~ NA)) |>
  mutate(Neglect2 = case_when(
    Neglect2 == 1 ~ 0,
    Neglect2 == 2 ~ 1,
    Neglect2 == 3 ~ 1,
    Neglect2 == 4 ~ 1,
    Neglect2 == 5 ~ 1,
    Neglect2 == NA ~ NA,
    Neglect2 == 7 ~ NA,
    Neglect2 == 9 ~ NA)) |>
  mutate(number = 1)

## 2021: 
twenty021 = twenty021 |> select(
  IYEAR, '_STATE', SEXVAR, MARITAL, VETERAN3, INCOME3, "_BMI5",
  EDUCA, RENTHOM1, EMPLOY1, CHILDREN, CAREGIV1, CRGVREL4, CRGVLNG1, CRGVPRB3, SOMALE, TRNSGNDR, "_METSTAT", "_URBSTAT", "_IMPRACE", "_AGE_G",
  GENHLTH, PHYSHLTH, MENTHLTH, POORHLTH, EXERANY2, CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, ADDEPEV3, CHCKDNY2, DIABETE4, DEAF, BLIND, DIFFWALK, DIFFDRES, DIFFALON,
  SMOKE100, SMOKDAY2, USENOW3, LCSFIRST, LCSLAST, LCSNUMCG, ECIGNOW1, AVEDRNK3, ALCDAY5, DRNK3GE5, MAXDRNKS, MARIJAN1, LASTSMK2, 
  PRIMINSR, MEDCOST1, 
  ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, 
  ACEDIVRC, ACEPUNCH, ACEHURT1, ACESWEAR,
  ACETOUCH, ACETTHEM, ACEHVSEX, ACEADSAF, 
  ACEADNED) |> 
  filter(!is.na(ACEDEPRS) | !is.na(ACEDRINK) | !is.na(ACEDRUGS) | !is.na(ACEPRISN) | 
           !is.na(ACEDIVRC) | !is.na(ACEPUNCH) | !is.na(ACEHURT1) | !is.na(ACESWEAR) | 
           !is.na(ACETOUCH) | !is.na(ACETTHEM) | !is.na(ACEHVSEX) | !is.na(ACEADSAF) |
           !is.na(ACEADNED)) |>
  rename("state" = "_STATE",
         "age" = "_AGE_G",
         "race" = "_IMPRACE",
         "sex" = SEXVAR,
         "MentalIllness" = ACEDEPRS,
         "Alcoholic" = ACEDRINK,
         "SubstanceAddiction" = ACEDRUGS,
         "Incarcerated" = ACEPRISN,
         "SeparationDivorce" = ACEDIVRC,
         "DomesticViolence" = ACEPUNCH,
         "PhysicalAbuse" = ACEHURT1,
         "EmotionalAbuse" = ACESWEAR,
         "SexualAbuse1" = ACETOUCH,
         "SexualAbuse2" = ACETTHEM,
         "SexualAbuse3" = ACEHVSEX,
         "Neglect1" = ACEADSAF,
         "Neglect2" = ACEADNED) |> 
  mutate(MentalIllness = case_when(
    MentalIllness == 1 ~ 1,
    MentalIllness == 2 ~ 0,
    MentalIllness == 7 ~ NA,
    MentalIllness == 9 ~ NA)) |>
  mutate(Alcoholic = case_when(
    Alcoholic == 1 ~ 1,
    Alcoholic == 2 ~ 0,
    Alcoholic == 7 ~ NA,
    Alcoholic == 9 ~ NA)) |> 
  mutate(SubstanceAddiction = case_when(
    SubstanceAddiction == 1 ~ 1,
    SubstanceAddiction == 2 ~ 0,
    SubstanceAddiction == 7 ~ NA,
    SubstanceAddiction == 9 ~ NA)) |>
  mutate(Incarcerated = case_when(
    Incarcerated == 1 ~ 1,
    Incarcerated == 2 ~ 0,
    Incarcerated == 7 ~ NA,
    Incarcerated == 9 ~ NA)) |> 
  mutate(SeparationDivorce = case_when(
    SeparationDivorce == 1 ~ 1,
    SeparationDivorce == 2 ~ 0,
    SeparationDivorce == 7 ~ NA,
    SeparationDivorce == 8 ~ NA,
    SeparationDivorce == 9 ~ NA)) |>
  mutate(DomesticViolence = case_when(
    DomesticViolence == 1 ~ 0,
    DomesticViolence == 2 ~ 1, 
    DomesticViolence == 3 ~ 1,
    DomesticViolence == 7 ~ NA,
    DomesticViolence == 9 ~ NA)) |>
  mutate(PhysicalAbuse = case_when(
    PhysicalAbuse == 1 ~ 0,
    PhysicalAbuse == 2 ~ 1,
    PhysicalAbuse == 3 ~ 1,
    PhysicalAbuse == 7 ~ NA,
    PhysicalAbuse == 9 ~ NA)) |>
  mutate(EmotionalAbuse = case_when(
    EmotionalAbuse == 1 ~ 0,
    EmotionalAbuse == 2 ~ 1, 
    EmotionalAbuse == 3 ~ 1,
    EmotionalAbuse == 7 ~ NA,
    EmotionalAbuse == 9 ~ NA)) |>
  mutate(SexualAbuse1 = case_when(
    SexualAbuse1 == 1 ~ 0,
    SexualAbuse1 == 2 ~ 1,
    SexualAbuse1 == 3 ~ 1,
    SexualAbuse1 == 7 ~ NA,
    SexualAbuse1 == 9 ~ NA)) |>
  mutate(SexualAbuse2 = case_when(
    SexualAbuse2 == 1 ~ 0,
    SexualAbuse2 == 2 ~ 1,
    SexualAbuse2 == 3 ~ 1,
    SexualAbuse2 == 7 ~ NA,
    SexualAbuse2 == 9 ~ NA)) |> 
  mutate(SexualAbuse3 = case_when(
    SexualAbuse3 == 1 ~ 0,
    SexualAbuse3 == 2 ~ 1,
    SexualAbuse3 == 3 ~ 1,
    SexualAbuse3 == 7 ~ NA,
    SexualAbuse3 == 9 ~ NA)) |>
  mutate(Neglect1 = case_when(
    Neglect1 == NA ~ NA,
    Neglect1 == 1 ~ 0,
    Neglect1 == 2 ~ 1,
    Neglect1 == 3 ~ 1,
    Neglect1 == 4 ~ 1,
    Neglect1 == 5 ~ 1,
    Neglect1 == 7 ~ NA,
    Neglect1 == 9 ~ NA)) |>
  mutate(Neglect2 = case_when(
    Neglect2 == 1 ~ 0,
    Neglect2 == 2 ~ 1,
    Neglect2 == 3 ~ 1,
    Neglect2 == 4 ~ 1,
    Neglect2 == 5 ~ 1,
    Neglect2 == NA ~ NA,
    Neglect2 == 7 ~ NA,
    Neglect2 == 9 ~ NA)) |>
  mutate(number = 2)

## 2020: 

twenty020 = twenty020 |> select(
  IYEAR, '_STATE', SEXVAR, MARITAL, VETERAN3, INCOME2, WEIGHT2, HEIGHT3, 
  EDUCA, RENTHOM1, EMPLOY1, CHILDREN, CAREGIV1, CRGVREL4, CRGVLNG1, CRGVPRB3, SOMALE, TRNSGNDR, "_METSTAT", "_URBSTAT", "_IMPRACE", "_AGE_G",
  GENHLTH, PHYSHLTH, MENTHLTH, POORHLTH, EXERANY2, CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, ADDEPEV3, CHCKDNY2, DIABETE4, DEAF, BLIND, DIFFWALK, DIFFDRES, DIFFALON,
  SMOKE100, SMOKDAY2, USENOW3, LCSFIRST, LCSLAST, LCSNUMCG, ECIGNOW, AVEDRNK3, ALCDAY5, DRNK3GE5, MAXDRNKS, MARIJAN1, LASTSMK2, 
  HLTHCVR1,
  ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, 
  ACEDIVRC, ACEPUNCH, ACEHURT1, ACESWEAR,
  ACETOUCH, ACETTHEM, ACEHVSEX) |> 
  filter(!is.na(ACEDEPRS) | !is.na(ACEDRINK) | !is.na(ACEDRUGS) | !is.na(ACEPRISN) | 
           !is.na(ACEDIVRC) | !is.na(ACEPUNCH) | !is.na(ACEHURT1) | !is.na(ACESWEAR) | 
           !is.na(ACETOUCH) | !is.na(ACETTHEM) | !is.na(ACEHVSEX)) |>
  rename("state" = "_STATE",
         "age" = "_AGE_G",
         "race" = "_IMPRACE",
         "sex" = SEXVAR,
         "MentalIllness" = ACEDEPRS,
         "Alcoholic" = ACEDRINK,
         "SubstanceAddiction" = ACEDRUGS,
         "Incarcerated" = ACEPRISN,
         "SeparationDivorce" = ACEDIVRC,
         "DomesticViolence" = ACEPUNCH,
         "PhysicalAbuse" = ACEHURT1,
         "EmotionalAbuse" = ACESWEAR,
         "SexualAbuse1" = ACETOUCH,
         "SexualAbuse2" = ACETTHEM,
         "SexualAbuse3" = ACEHVSEX) |>
  mutate(MentalIllness = case_when(
    MentalIllness == 1 ~ 1,
    MentalIllness == 2 ~ 0,
    MentalIllness == 7 ~ NA,
    MentalIllness == 9 ~ NA)) |>
  mutate(Alcoholic = case_when(
    Alcoholic == 1 ~ 1,
    Alcoholic == 2 ~ 0,
    Alcoholic == 7 ~ NA,
    Alcoholic == 9 ~ NA)) |> 
  mutate(SubstanceAddiction = case_when(
    SubstanceAddiction == 1 ~ 1,
    SubstanceAddiction == 2 ~ 0,
    SubstanceAddiction == 7 ~ NA,
    SubstanceAddiction == 9 ~ NA)) |>
  mutate(Incarcerated = case_when(
    Incarcerated == 1 ~ 1,
    Incarcerated == 2 ~ 0,
    Incarcerated == 7 ~ NA,
    Incarcerated == 9 ~ NA)) |> 
  mutate(SeparationDivorce = case_when(
    SeparationDivorce == 1 ~ 1,
    SeparationDivorce == 2 ~ 0,
    SeparationDivorce == 7 ~ NA,
    SeparationDivorce == 8 ~ NA,
    SeparationDivorce == 9 ~ NA)) |>
  mutate(DomesticViolence = case_when(
    DomesticViolence == 1 ~ 0,
    DomesticViolence == 2 ~ 1, 
    DomesticViolence == 3 ~ 1,
    DomesticViolence == 7 ~ NA,
    DomesticViolence == 9 ~ NA)) |>
  mutate(PhysicalAbuse = case_when(
    PhysicalAbuse == 1 ~ 0,
    PhysicalAbuse == 2 ~ 1,
    PhysicalAbuse == 3 ~ 1,
    PhysicalAbuse == 7 ~ NA,
    PhysicalAbuse == 9 ~ NA)) |>
  mutate(EmotionalAbuse = case_when(
    EmotionalAbuse == 1 ~ 0,
    EmotionalAbuse == 2 ~ 1, 
    EmotionalAbuse == 3 ~ 1,
    EmotionalAbuse == 7 ~ NA,
    EmotionalAbuse == 9 ~ NA)) |>
  mutate(SexualAbuse1 = case_when(
    SexualAbuse1 == 1 ~ 0,
    SexualAbuse1 == 2 ~ 1,
    SexualAbuse1 == 3 ~ 1,
    SexualAbuse1 == 7 ~ NA,
    SexualAbuse1 == 9 ~ NA)) |>
  mutate(SexualAbuse2 = case_when(
    SexualAbuse2 == 1 ~ 0,
    SexualAbuse2 == 2 ~ 1,
    SexualAbuse2 == 3 ~ 1,
    SexualAbuse2 == 7 ~ NA,
    SexualAbuse2 == 9 ~ NA)) |> 
  mutate(SexualAbuse3 = case_when(
    SexualAbuse3 == 1 ~ 0,
    SexualAbuse3 == 2 ~ 1,
    SexualAbuse3 == 3 ~ 1,
    SexualAbuse3 == 7 ~ NA,
    SexualAbuse3 == 9 ~ NA)) |>
  mutate(number = 3)

# 2019: 

twenty019 = twenty019 |> select(
  IYEAR, '_STATE', SEXVAR, MARITAL, VETERAN3, INCOME2, WEIGHT2, HEIGHT3, 
  EDUCA, RENTHOM1, EMPLOY1, CHILDREN, CAREGIV1, CRGVLNG1, CRGVPRB3, SOMALE, TRNSGNDR, "_METSTAT", "_URBSTAT", "_IMPRACE", "_AGE_G",
  GENHLTH, PHYSHLTH, MENTHLTH, POORHLTH, EXERANY2, CVDINFR4, CVDCRHD4, CVDSTRK3, ASTHMA3, ADDEPEV3, CHCKDNY2, DIABETE4, DEAF, BLIND, DIFFWALK, DIFFDRES, DIFFALON,
  SMOKE100, SMOKDAY2, USENOW3, LCSFIRST, LCSLAST, LCSNUMCG, ALCDAY5, AVEDRNK3, DRNK3GE5, MAXDRNKS, MARIJAN1, LASTSMK2, 
  HLTHCVR1,
  ACEDEPRS, ACEDRINK, ACEDRUGS, ACEPRISN, 
  ACEDIVRC, ACEPUNCH, ACEHURT1, ACESWEAR,
  ACETOUCH, ACETTHEM, ACEHVSEX) |> 
  filter(!is.na(ACEDEPRS) | !is.na(ACEDRINK) | !is.na(ACEDRUGS) | !is.na(ACEPRISN) | 
           !is.na(ACEDIVRC) | !is.na(ACEPUNCH) | !is.na(ACEHURT1) | !is.na(ACESWEAR) | 
           !is.na(ACETOUCH) | !is.na(ACETTHEM) | !is.na(ACEHVSEX)) |>
  rename("state" = "_STATE",
         "age" = "_AGE_G",
         "race" = "_IMPRACE",
         "sex" = SEXVAR,
         "MentalIllness" = ACEDEPRS,
         "Alcoholic" = ACEDRINK,
         "SubstanceAddiction" = ACEDRUGS,
         "Incarcerated" = ACEPRISN,
         "SeparationDivorce" = ACEDIVRC,
         "DomesticViolence" = ACEPUNCH,
         "PhysicalAbuse" = ACEHURT1,
         "EmotionalAbuse" = ACESWEAR,
         "SexualAbuse1" = ACETOUCH,
         "SexualAbuse2" = ACETTHEM,
         "SexualAbuse3" = ACEHVSEX) |>
  mutate(MentalIllness = case_when(
    MentalIllness == 1 ~ 1,
    MentalIllness == 2 ~ 0,
    MentalIllness == 7 ~ NA,
    MentalIllness == 9 ~ NA)) |>
  mutate(Alcoholic = case_when(
    Alcoholic == 1 ~ 1,
    Alcoholic == 2 ~ 0,
    Alcoholic == 7 ~ NA,
    Alcoholic == 9 ~ NA)) |> 
  mutate(SubstanceAddiction = case_when(
    SubstanceAddiction == 1 ~ 1,
    SubstanceAddiction == 2 ~ 0,
    SubstanceAddiction == 7 ~ NA,
    SubstanceAddiction == 9 ~ NA)) |>
  mutate(Incarcerated = case_when(
    Incarcerated == 1 ~ 1,
    Incarcerated == 2 ~ 0,
    Incarcerated == 7 ~ NA,
    Incarcerated == 9 ~ NA)) |> 
  mutate(SeparationDivorce = case_when(
    SeparationDivorce == 1 ~ 1,
    SeparationDivorce == 2 ~ 0,
    SeparationDivorce == 7 ~ NA,
    SeparationDivorce == 8 ~ NA,
    SeparationDivorce == 9 ~ NA)) |>
  mutate(DomesticViolence = case_when(
    DomesticViolence == 1 ~ 0,
    DomesticViolence == 2 ~ 1, 
    DomesticViolence == 3 ~ 1,
    DomesticViolence == 7 ~ NA,
    DomesticViolence == 9 ~ NA)) |>
  mutate(PhysicalAbuse = case_when(
    PhysicalAbuse == 1 ~ 0,
    PhysicalAbuse == 2 ~ 1,
    PhysicalAbuse == 3 ~ 1,
    PhysicalAbuse == 7 ~ NA,
    PhysicalAbuse == 9 ~ NA)) |>
  mutate(EmotionalAbuse = case_when(
    EmotionalAbuse == 1 ~ 0,
    EmotionalAbuse == 2 ~ 1, 
    EmotionalAbuse == 3 ~ 1,
    EmotionalAbuse == 7 ~ NA,
    EmotionalAbuse == 9 ~ NA)) |>
  mutate(SexualAbuse1 = case_when(
    SexualAbuse1 == 1 ~ 0,
    SexualAbuse1 == 2 ~ 1,
    SexualAbuse1 == 3 ~ 1,
    SexualAbuse1 == 7 ~ NA,
    SexualAbuse1 == 9 ~ NA)) |>
  mutate(SexualAbuse2 = case_when(
    SexualAbuse2 == 1 ~ 0,
    SexualAbuse2 == 2 ~ 1,
    SexualAbuse2 == 3 ~ 1,
    SexualAbuse2 == 7 ~ NA,
    SexualAbuse2 == 9 ~ NA)) |> 
  mutate(SexualAbuse3 = case_when(
    SexualAbuse3 == 1 ~ 0,
    SexualAbuse3 == 2 ~ 1,
    SexualAbuse3 == 3 ~ 1,
    SexualAbuse3 == 7 ~ NA,
    SexualAbuse3 == 9 ~ NA)) |>
  mutate(number = 4)

## Dataset that inclueds everything: 
everything = full_join(twenty019, full_join(twenty020, full_join(twenty021, twenty022)))


############################################


# MAXDRNKS
## Question: During the past 30 days, what is the largest number of drinks you had on any occasion?

maxdrinks = everything |> select(MentalIllness, Alcoholic, Incarcerated, SeparationDivorce, DomesticViolence, PhysicalAbuse, EmotionalAbuse, SexualAbuse1, SexualAbuse2, SexualAbuse3, Neglect1, Neglect2, SubstanceAddiction, MAXDRNKS, sex, age, race, number) |>
  filter(MAXDRNKS != 77 ) |>
  filter(MAXDRNKS != 99) |>
  mutate(sex = if_else(sex == 1, "Male", "Female")) |>
  mutate(sex = as.factor(sex),
         age = as.factor(age),
         race = as.factor(race),
         MentalIllness = as.factor(MentalIllness),
         Alcoholic = as.factor(Alcoholic),
         Incarcerated = as.factor(Incarcerated),
         SeparationDivorce = as.factor(SeparationDivorce),
         DomesticViolence = as.factor(DomesticViolence),
         PhysicalAbuse = as.factor(PhysicalAbuse),
         EmotionalAbuse = as.factor(EmotionalAbuse),
         SexualAbuse1 = as.factor(SexualAbuse1),
         SexualAbuse2 = as.factor(SexualAbuse2),
         SexualAbuse3 = as.factor(SexualAbuse3),
         Neglect1 = as.factor(Neglect1),
         Neglect2 = as.factor(Neglect2),
         SubstanceAddiction = as.factor(SubstanceAddiction)) |>
  filter(!is.na(MAXDRNKS)) |>
  mutate(age = case_when(
    age == 1 ~ "18 to 24",
    age == 2 ~ "25 to 34",
    age == 3 ~ "35 to 44",
    age == 4 ~ "45 to 54",
    age == 5 ~ "55 to 64",
    age == 6 ~ "65 or > 65")) |>
  rename(max = MAXDRNKS) |>
  mutate(age = factor(age, levels = c("65 or > 65", "55 to 64", "45 to 54", "35 to 44", "25 to 34", "18 to 24"))) |>
  mutate(sex = factor(sex,levels = c( "Male", "Female")))




maxall = glm(max ~ 
                 MentalIllness + 
                 Alcoholic + 
                 SubstanceAddiction + 
                 Incarcerated + 
                 SeparationDivorce + 
                 DomesticViolence +
                 PhysicalAbuse + 
                 EmotionalAbuse + 
                 SexualAbuse1 + 
                 SexualAbuse2 + 
                 SexualAbuse3 + 
                 Neglect1 + 
                 Neglect2, 
               family = "quasipoisson", data = maxdrinks)


maxall1 = glm(max ~ age + sex + race +
                  MentalIllness + 
                  Alcoholic + 
                  SubstanceAddiction + 
                  Incarcerated + 
                  SeparationDivorce + 
                  DomesticViolence +
                  PhysicalAbuse + 
                  EmotionalAbuse + 
                  SexualAbuse1 + 
                  SexualAbuse2 + 
                  SexualAbuse3 + 
                  Neglect1 + 
                  Neglect2, 
                family = "quasipoisson", data = maxdrinks)


maxall2 = glm(max ~ 
                  MentalIllness + MentalIllness*sex +
                  Alcoholic + Alcoholic*sex + 
                  SubstanceAddiction + SubstanceAddiction*sex + 
                  Incarcerated + Incarcerated*sex + 
                  SeparationDivorce + SeparationDivorce*sex + 
                  DomesticViolence + DomesticViolence*sex + 
                  PhysicalAbuse + PhysicalAbuse*sex  + 
                  EmotionalAbuse + EmotionalAbuse*sex +  
                  SexualAbuse1 + SexualAbuse1*sex + 
                  SexualAbuse2 + SexualAbuse2*sex + 
                  SexualAbuse3 + SexualAbuse3*sex + 
                  Neglect1 + Neglect1*sex + 
                  Neglect2 + Neglect2*sex, 
                family = "quasipoisson", data = maxdrinks)


maxall3 = glm(max ~ 
                  MentalIllness + MentalIllness*age + 
                  Alcoholic + Alcoholic*age + 
                  SubstanceAddiction + SubstanceAddiction*age + 
                  Incarcerated + Incarcerated*age + 
                  SeparationDivorce + SeparationDivorce*age + 
                  DomesticViolence + DomesticViolence*age + 
                  PhysicalAbuse + PhysicalAbuse*age + 
                  EmotionalAbuse + EmotionalAbuse*age + 
                  SexualAbuse1 + SexualAbuse1*age + 
                  SexualAbuse2 + SexualAbuse2*age + 
                  SexualAbuse3 + SexualAbuse3*age + 
                  Neglect1 + Neglect1*age + 
                  Neglect2 + Neglect2*age, 
                family = "quasipoisson", data = maxdrinks)



maxall4 = glm(max ~ 
                  MentalIllness + MentalIllness*sex + MentalIllness*age + 
                  Alcoholic + Alcoholic*sex + Alcoholic*age + 
                  SubstanceAddiction + SubstanceAddiction*sex + SubstanceAddiction*age + 
                  Incarcerated + Incarcerated*sex + Incarcerated*age + 
                  SeparationDivorce + SeparationDivorce*sex + SeparationDivorce*age + 
                  DomesticViolence + DomesticViolence*sex + DomesticViolence*age + 
                  PhysicalAbuse + PhysicalAbuse*sex  + PhysicalAbuse*age + 
                  EmotionalAbuse + EmotionalAbuse*sex + EmotionalAbuse*age + 
                  SexualAbuse1 + SexualAbuse1*sex + SexualAbuse1*age + 
                  SexualAbuse2 + SexualAbuse2*sex + SexualAbuse2*age + 
                  SexualAbuse3 + SexualAbuse3*sex + SexualAbuse3*age + 
                  Neglect1 + Neglect1*sex + Neglect1*age + 
                  Neglect2 + Neglect2*sex + Neglect2*age, 
                family = "quasipoisson", data = maxdrinks)

maxall5 = glm(max ~
                  MentalIllness + MentalIllness*sex + MentalIllness*age + MentalIllness*age*sex + 
                  Alcoholic + Alcoholic*sex + Alcoholic*age + Alcoholic*age*sex + 
                  SubstanceAddiction + SubstanceAddiction*sex + SubstanceAddiction*age + SubstanceAddiction*age*sex + 
                  Incarcerated + Incarcerated*sex + Incarcerated*age + Incarcerated*age*sex + 
                  SeparationDivorce + SeparationDivorce*sex + SeparationDivorce*age + SeparationDivorce*age*sex + 
                  DomesticViolence + DomesticViolence*sex + DomesticViolence*age + DomesticViolence*age*sex + 
                  PhysicalAbuse + PhysicalAbuse*sex  + PhysicalAbuse*age + PhysicalAbuse*age*sex + 
                  EmotionalAbuse + EmotionalAbuse*sex + EmotionalAbuse*age + EmotionalAbuse*age*sex + 
                  SexualAbuse1 + SexualAbuse1*sex + SexualAbuse1*age + SexualAbuse1*age*sex + 
                  SexualAbuse2 + SexualAbuse2*sex + SexualAbuse2*age + SexualAbuse2*age*sex + 
                  SexualAbuse3 + SexualAbuse3*sex + SexualAbuse3*age + SexualAbuse3*age*sex + 
                  Neglect1 + Neglect1*sex + Neglect1*age + Neglect1*age*sex + 
                  Neglect2 + Neglect2*sex + Neglect2*age + Neglect2*age*sex, 
                family = "quasipoisson", data = maxdrinks)


anova( maxall1, maxall, maxall2, maxall3, maxall4, maxall5, test = "Chisq") 
anova(maxall3, maxall4, maxall5, test = "Chisq") #maxall5 is the best model. lowest deviance.  

tidy(maxall5) |>
  mutate(estimate = round(exp(estimate), 2)) |>
  filter(p.value <= 0.01) |>
  mutate(std.error = round(std.error, 2)) |>
  mutate(statistic = round(statistic, 2)) |>
  kable(align = c('r', 'c', 'c', 'c', 'c')) 
  #filter(str_detect(term, "Incarcerated"))



maxdrinks2 = everything |> select(MentalIllness, Alcoholic, Incarcerated, SeparationDivorce, DomesticViolence, PhysicalAbuse, EmotionalAbuse, SexualAbuse1, SexualAbuse2, SexualAbuse3, Neglect1, Neglect2, SubstanceAddiction, MAXDRNKS, sex, age, race, number) |>
  filter(MAXDRNKS != 77 ) |>
  filter(MAXDRNKS != 99) |>
  mutate(sex = if_else(sex == 1, "Male", "Female")) |>
  filter(!is.na(MAXDRNKS)) |>
  rename(max = MAXDRNKS) 


maxdrinks2 = maxdrinks2 |> 
  select(c(1:13)) |>
  mutate(
    MentalIllness = if_else(is.na(MentalIllness), 0, MentalIllness),
    Alcoholic = if_else(is.na(Alcoholic), 0, Alcoholic),
    Incarcerated = if_else(is.na(Incarcerated), 0, Incarcerated),
    SeparationDivorce = if_else(is.na(SeparationDivorce), 0, SeparationDivorce),
    DomesticViolence = if_else(is.na(DomesticViolence), 0, DomesticViolence),
    PhysicalAbuse = if_else(is.na(PhysicalAbuse), 0, PhysicalAbuse),
    EmotionalAbuse = if_else(is.na(EmotionalAbuse), 0, EmotionalAbuse),
    SexualAbuse1 = if_else(is.na(SexualAbuse1), 0, SexualAbuse1),
    SexualAbuse2 = if_else(is.na(SexualAbuse2), 0, SexualAbuse2),
    SexualAbuse3 = if_else(is.na(SexualAbuse3), 0, SexualAbuse3),
    Neglect1 = if_else(is.na(Neglect1), 0, Neglect1), 
    Neglect2 = if_else(is.na(Neglect2), 0, Neglect2),
    SubstanceAddiction = if_else(is.na(SubstanceAddiction), 0, SubstanceAddiction)
  )


sumsACEmax= rowSums(maxdrinks2, na.rm = TRUE)


Categories = c()

one = ""
two = ""
three = ""
four = ""
five = ""
six = ""
seven = ""
eight = ""
nine = ""
ten = ""
eleven = ""
twelve =""
thirteen = ""

for (x in 1:160785) {
  part = maxdrinks2 |> slice(x)
  for (i in 1:13) {
    part1 = as.numeric(part[i])
    if ( part1 == 1 ) {
      if (i == 1) {
        one = "MI"
      }
      if (i == 2){
        two = "A"
      }
      if (i == 3){
        three = "I"
      }
      if (i == 4){
        four = "S/D"
      }
      if (i == 5){
        five = "DV"
      }
      if (i == 6){
        six = "PA"
      }
      if (i == 7){
        seven = "EA"
      }
      if (i == 8){
        eight = "SA1"
      }
      if (i == 9){
        nine = "SA2"
      }
      if (i == 10){
        ten = "SA3"
      }
      if (i == 11){
        eleven = "N1"
      }
      if (i == 12){
        twelve = "N2"
      }
      if (i == 13){
        thirteen = "SubAddic"
      }
    }
    
  }
  Categories[x] = paste(one, two, three, four, five, six, seven, eight, nine, ten, eleven, twelve, thirteen)
  one = ""
  two = ""
  three = ""
  four = ""
  five = ""
  six = ""
  seven = ""
  eight = ""
  nine = ""
  ten = ""
  eleven = ""
  twelve =""
  thirteen = ""
  
}


maxdrinks3 = maxdrinks |>
  mutate(Categories = Categories) |>
  mutate(Categories = str_squish(Categories)) |>
  mutate(Categories = as.factor(Categories)) |>
  mutate(sum = rowSums(maxdrinks2, na.rm = TRUE))


maxdrinks4 = maxdrinks3 |> 
  group_by(Categories, sum) |>
  summarise(n = n()) |>
  arrange(desc(sum)) |>
  filter(n >= 25)




alc1 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " A ") | Categories == "A") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Alcohol Misuse in HH") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



alc2 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " A ") | Categories == "A") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(alc1, alc2, nrow = 1)




mi1 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, "MI ") | Categories == "MI") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Mental Illness in HH") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



mi2 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, "MI ") | Categories == "MI") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(mi1, mi2, nrow = 1)



i1 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " I ") | Categories == "I") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Incarcerated HH Member") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



i2 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " I ") | Categories == "I") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(i1, i2, nrow = 1)


sd1 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " S/D ") | Categories == "S/D") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Parental Separation/Divorce") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



sd2 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " S/D ") | Categories == "S/D") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(sd1, sd2, nrow = 1)


dv1 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " DV ") | Categories == "DV") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Domestic Violence in HH") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



dv2 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " DV ") | Categories == "DV") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(dv1, dv2, nrow = 1)



pa1 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " PA ") | Categories == "PA") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Physical Abuse") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



pa2 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " PA ") | Categories == "PA") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(pa1, pa2, nrow = 1)


ea1 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " EA ") | Categories == "EA") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Emotional Abuse") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



ea2 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " EA ") | Categories == "EA") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(ea1, ea2, nrow = 1)


sa11 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " SA1 ") | Categories == "SA1") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Sexual Abuse (Molestation)") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



sa12 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " SA1 ") | Categories == "SA1") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(sa11, sa12, nrow = 1)


sa21 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " SA2 ") | Categories == "SA2") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Sexual Abuse (Forced Sexual Touch to Abuser)") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



sa22 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " SA2 ") | Categories == "SA2") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(sa21, sa22, nrow = 1)


sa31 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " SA3 ") | Categories == "SA3") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Sexual Abuse (Rape)") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



sa32 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " SA3 ") | Categories == "SA3") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(sa31, sa32, nrow = 1)


n11 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " N1 ") | Categories == "N1") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Neglect (N1)") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



n12 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " N1 ") | Categories == "N1") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(n11, n12, nrow = 1)


n21 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " N2 ") | Categories == "N2") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1, show.legend = FALSE) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum), show.legend = FALSE) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "History of Neglect (N2)") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
    
  )



n22 = left_join(maxdrinks4, maxdrinks3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(str_detect(Categories, " N2 ") | Categories == "N2") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmin = mean-se, xmax = mean+se, col = sum)) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 1, end = 0.30) +
  theme_minimal() +
  labs(x = "Average Occasions for Binge Drinking (per month)", title = " ") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "white", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 15, family = "Avenir"),
    legend.text = element_text(color = "black", size = 11, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

grid.arrange(n21, n22, nrow = 1)





maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max)) |>
  filter(n >= 25) |>
  filter(sum != 0) |>
filter(str_detect(Categories, " A ") | Categories == "A")




maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(sum == 1) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  ggplot(aes(x = mean, y = Categories)) + 
  geom_point(size = 4, stroke = 1, aes(x = mean, y = Categories, shape = sex, col = age)) +
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = age, alpha = se), width = 0.3) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "One ACE") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )


maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(sum == 2) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  ggplot(aes(x = mean, y = Categories)) + 
  geom_point(size = 4, stroke = 1, aes(x = mean, y = Categories, shape = sex, col = age)) +
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = age, alpha = se), width = 0.4) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "Two ACEs") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )


maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(sum == 3) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  ggplot(aes(x = mean, y = Categories)) + 
  geom_point(size = 4, stroke = 1, aes(x = mean, y = Categories, shape = sex, col = age)) +
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = age, alpha = se), width = 0.5) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "Three ACEs") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )


maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(sum == 4) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  ggplot(aes(x = mean, y = Categories)) + 
  geom_point(size = 4, stroke = 1, aes(x = mean, y = Categories, shape = sex, col = age)) +
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = age, alpha = se), width = 0.3) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "Four ACEs") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )


maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(sum == 5) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  ggplot(aes(x = mean, y = Categories)) + 
  geom_point(size = 4, stroke = 1, aes(x = mean, y = Categories, shape = sex, col = age)) +
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = age, alpha = se), width = 0.3) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "Five ACEs") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )

maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(sum == 6) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  ggplot(aes(x = mean, y = Categories)) + 
  geom_point(size = 4, stroke = 1, aes(x = mean, y = Categories, shape = sex, col = age)) +
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = age, alpha = se), width = 0.3) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "Six ACEs") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )


maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(sum == 7) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  ggplot(aes(x = mean, y = Categories)) + 
  geom_point(size = 4, stroke = 1, aes(x = mean, y = Categories, shape = sex, col = age)) +
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = age, alpha = se), width = 0.3) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "Seven ACEs") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )



maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(sum == 8) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  ggplot(aes(x = mean, y = Categories)) + 
  geom_point(size = 4, stroke = 1, aes(x = mean, y = Categories, shape = sex, col = age)) +
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = age, alpha = se), width = 0.3) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "Eight ACEs") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )


maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(sum == 9) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  ggplot(aes(x = mean, y = Categories)) + 
  geom_point(size = 4, stroke = 1, aes(x = mean, y = Categories, shape = sex, col = age)) +
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = age, alpha = se), width = 0.3) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "Nine ACEs") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )


maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(sum == 10) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  ggplot(aes(x = mean, y = Categories)) + 
  geom_point(size = 4, stroke = 1, aes(x = mean, y = Categories, shape = sex, col = age)) +
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = age, alpha = se), width = 0.3) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "Ten ACEs") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )


maxdrinks3 |>
  group_by(Categories, sum, age, sex) |>
  summarise(mean = mean(max),
            n = n(),
            sd = sd(max),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(sum == 11 | sum == 13) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, mean)) |>
  ggplot(aes(x = mean, y = Categories)) + 
  geom_point(size = 4, stroke = 1, aes(x = mean, y = Categories, shape = sex, col = age)) +
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = age, alpha = se), width = 0.1) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Maximum Drinks On An Occasion (past month)", title = "Eleven and Thirteen ACEs") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "right", 
    legend.direction = "vertical", 
    legend.box = "vertical", 
    legend.title = element_blank()
  )




