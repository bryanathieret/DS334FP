


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










## During the past 30 days, how many days per month did you have at least one drink of any alcoholic beverage such as beer, wine, a malt beverage or liquor?


permonth = everything |> select(MentalIllness, Alcoholic, Incarcerated, SeparationDivorce, DomesticViolence, PhysicalAbuse, EmotionalAbuse, SexualAbuse1, SexualAbuse2, SexualAbuse3, Neglect1, Neglect2, SubstanceAddiction, ALCDAY4, ALCDAY5, sex, age, race, number) |>
  mutate(permonth = case_when(
    ALCDAY5 == 201 ~ 1, 
    ALCDAY5 == 202 ~ 2,
    ALCDAY5 == 203 ~ 3,
    ALCDAY5 == 204 ~ 4,
    ALCDAY5 == 205 ~ 5,
    ALCDAY5 == 206 ~ 6,
    ALCDAY5 == 207 ~ 7,
    ALCDAY5 == 208 ~ 8,
    ALCDAY5 == 209 ~ 9,
    ALCDAY5 == 210 ~ 10, 
    ALCDAY5 == 211 ~ 11,
    ALCDAY5 == 212 ~ 12,
    ALCDAY5 == 213 ~ 13,
    ALCDAY5 == 214 ~ 14,
    ALCDAY5 == 215 ~ 15,
    ALCDAY5 == 216 ~ 16,
    ALCDAY5 == 217 ~ 17,
    ALCDAY5 == 218 ~ 18,
    ALCDAY5 == 219 ~ 19,
    ALCDAY5 == 220 ~ 20,
    ALCDAY5 == 221 ~ 21,
    ALCDAY5 == 222 ~ 22,
    ALCDAY5 == 223 ~ 23,
    ALCDAY5 == 224 ~ 24,
    ALCDAY5 == 225 ~ 25,
    ALCDAY5 == 226 ~ 26,
    ALCDAY5 == 227 ~ 27,
    ALCDAY5 == 228 ~ 28,
    ALCDAY5 == 229 ~ 29,
    ALCDAY5 == 230 ~ 30, 
    
    ALCDAY4 == 201 ~ 1, 
    ALCDAY4 == 202 ~ 2,
    ALCDAY4 == 203 ~ 3,
    ALCDAY4 == 204 ~ 4,
    ALCDAY4 == 205 ~ 5,
    ALCDAY4 == 206 ~ 6,
    ALCDAY4 == 207 ~ 7,
    ALCDAY4 == 208 ~ 8,
    ALCDAY4 == 209 ~ 9,
    ALCDAY4 == 210 ~ 10, 
    ALCDAY4 == 211 ~ 11,
    ALCDAY4 == 212 ~ 12,
    ALCDAY4 == 213 ~ 13,
    ALCDAY4 == 214 ~ 14,
    ALCDAY4 == 215 ~ 15,
    ALCDAY4 == 216 ~ 16,
    ALCDAY4 == 217 ~ 17,
    ALCDAY4 == 218 ~ 18,
    ALCDAY4 == 219 ~ 19,
    ALCDAY4 == 220 ~ 20,
    ALCDAY4 == 221 ~ 21,
    ALCDAY4 == 222 ~ 22,
    ALCDAY4 == 223 ~ 23,
    ALCDAY4 == 224 ~ 24,
    ALCDAY4 == 225 ~ 25,
    ALCDAY4 == 226 ~ 26,
    ALCDAY4 == 227 ~ 27,
    ALCDAY4 == 228 ~ 28,
    ALCDAY4 == 229 ~ 29,
    ALCDAY4 == 230 ~ 30)) |>
  filter(!is.na(permonth))

permonth |> ggplot(aes(x = permonth)) + geom_density()

############################################





permonth = everything |> select(MentalIllness, Alcoholic, Incarcerated, SeparationDivorce, DomesticViolence, PhysicalAbuse, EmotionalAbuse, SexualAbuse1, SexualAbuse2, SexualAbuse3, Neglect1, Neglect2, SubstanceAddiction, ALCDAY4, ALCDAY5, sex, age, race, number) |>
  mutate(permonth = case_when(
    ALCDAY5 == 201 ~ 1, 
    ALCDAY5 == 202 ~ 2,
    ALCDAY5 == 203 ~ 3,
    ALCDAY5 == 204 ~ 4,
    ALCDAY5 == 205 ~ 5,
    ALCDAY5 == 206 ~ 6,
    ALCDAY5 == 207 ~ 7,
    ALCDAY5 == 208 ~ 8,
    ALCDAY5 == 209 ~ 9,
    ALCDAY5 == 210 ~ 10, 
    ALCDAY5 == 211 ~ 11,
    ALCDAY5 == 212 ~ 12,
    ALCDAY5 == 213 ~ 13,
    ALCDAY5 == 214 ~ 14,
    ALCDAY5 == 215 ~ 15,
    ALCDAY5 == 216 ~ 16,
    ALCDAY5 == 217 ~ 17,
    ALCDAY5 == 218 ~ 18,
    ALCDAY5 == 219 ~ 19,
    ALCDAY5 == 220 ~ 20,
    ALCDAY5 == 221 ~ 21,
    ALCDAY5 == 222 ~ 22,
    ALCDAY5 == 223 ~ 23,
    ALCDAY5 == 224 ~ 24,
    ALCDAY5 == 225 ~ 25,
    ALCDAY5 == 226 ~ 26,
    ALCDAY5 == 227 ~ 27,
    ALCDAY5 == 228 ~ 28,
    ALCDAY5 == 229 ~ 29,
    ALCDAY5 == 230 ~ 30, 
    
    ALCDAY4 == 201 ~ 1, 
    ALCDAY4 == 202 ~ 2,
    ALCDAY4 == 203 ~ 3,
    ALCDAY4 == 204 ~ 4,
    ALCDAY4 == 205 ~ 5,
    ALCDAY4 == 206 ~ 6,
    ALCDAY4 == 207 ~ 7,
    ALCDAY4 == 208 ~ 8,
    ALCDAY4 == 209 ~ 9,
    ALCDAY4 == 210 ~ 10, 
    ALCDAY4 == 211 ~ 11,
    ALCDAY4 == 212 ~ 12,
    ALCDAY4 == 213 ~ 13,
    ALCDAY4 == 214 ~ 14,
    ALCDAY4 == 215 ~ 15,
    ALCDAY4 == 216 ~ 16,
    ALCDAY4 == 217 ~ 17,
    ALCDAY4 == 218 ~ 18,
    ALCDAY4 == 219 ~ 19,
    ALCDAY4 == 220 ~ 20,
    ALCDAY4 == 221 ~ 21,
    ALCDAY4 == 222 ~ 22,
    ALCDAY4 == 223 ~ 23,
    ALCDAY4 == 224 ~ 24,
    ALCDAY4 == 225 ~ 25,
    ALCDAY4 == 226 ~ 26,
    ALCDAY4 == 227 ~ 27,
    ALCDAY4 == 228 ~ 28,
    ALCDAY4 == 229 ~ 29,
    ALCDAY4 == 230 ~ 30)) |>
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
  filter(!is.na(permonth)) |>
  mutate(age = case_when(
    age == 1 ~ "18 to 24",
    age == 2 ~ "25 to 34",
    age == 3 ~ "35 to 44",
    age == 4 ~ "45 to 54",
    age == 5 ~ "55 to 64",
    age == 6 ~ "65 or > 65"))





# Models for permonth: 
## response variable is number of days is a count. Going to use a quasipoisson distribution. 

permonthall = glm(permonth ~ 
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
                 family = "quasipoisson", data = permonth)


permonthall1 = glm(permonth ~ age + sex + race +
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
                  family = "quasipoisson", data = permonth)


permonthall2 = glm(permonth ~ 
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
                  family = "quasipoisson", data = permonth)


permonthall3 = glm(permonth ~ 
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
                  family = "quasipoisson", data = permonth)



permonthall4 = glm(permonth ~ 
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
                  family = "quasipoisson", data = permonth)

permonthall5 = glm(permonth ~
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
                  family = "quasipoisson", data = permonth)


anova( permonthall1, permonthall, permonthall2, permonthall3, permonthall4, permonthall5, test = "Chisq") # permonthall4 is the best model. lowest deviance and df.
anova(permonthall2, permonthall4, test = "Chisq")


summary(permonthall4)

tidy(permonthall4) |> 
  mutate(estimate = round(exp(estimate), 2)) |>
  #filter(p.value <= 0.01) |>
  mutate(std.error = round(std.error, 2)) |>
  mutate(statistic = round(statistic, 2)) |>
  kable(align = c('r', 'c', 'c', 'c', 'c'))




permonth2 = everything |> select(MentalIllness, Alcoholic, Incarcerated, SeparationDivorce, DomesticViolence, PhysicalAbuse, EmotionalAbuse, SexualAbuse1, SexualAbuse2, SexualAbuse3, Neglect1, Neglect2, SubstanceAddiction, ALCDAY4, ALCDAY5, sex, age, race, number) |>
  mutate(permonth = case_when(
    ALCDAY5 == 201 ~ 1, 
    ALCDAY5 == 202 ~ 2,
    ALCDAY5 == 203 ~ 3,
    ALCDAY5 == 204 ~ 4,
    ALCDAY5 == 205 ~ 5,
    ALCDAY5 == 206 ~ 6,
    ALCDAY5 == 207 ~ 7,
    ALCDAY5 == 208 ~ 8,
    ALCDAY5 == 209 ~ 9,
    ALCDAY5 == 210 ~ 10, 
    ALCDAY5 == 211 ~ 11,
    ALCDAY5 == 212 ~ 12,
    ALCDAY5 == 213 ~ 13,
    ALCDAY5 == 214 ~ 14,
    ALCDAY5 == 215 ~ 15,
    ALCDAY5 == 216 ~ 16,
    ALCDAY5 == 217 ~ 17,
    ALCDAY5 == 218 ~ 18,
    ALCDAY5 == 219 ~ 19,
    ALCDAY5 == 220 ~ 20,
    ALCDAY5 == 221 ~ 21,
    ALCDAY5 == 222 ~ 22,
    ALCDAY5 == 223 ~ 23,
    ALCDAY5 == 224 ~ 24,
    ALCDAY5 == 225 ~ 25,
    ALCDAY5 == 226 ~ 26,
    ALCDAY5 == 227 ~ 27,
    ALCDAY5 == 228 ~ 28,
    ALCDAY5 == 229 ~ 29,
    ALCDAY5 == 230 ~ 30, 
    
    ALCDAY4 == 201 ~ 1, 
    ALCDAY4 == 202 ~ 2,
    ALCDAY4 == 203 ~ 3,
    ALCDAY4 == 204 ~ 4,
    ALCDAY4 == 205 ~ 5,
    ALCDAY4 == 206 ~ 6,
    ALCDAY4 == 207 ~ 7,
    ALCDAY4 == 208 ~ 8,
    ALCDAY4 == 209 ~ 9,
    ALCDAY4 == 210 ~ 10, 
    ALCDAY4 == 211 ~ 11,
    ALCDAY4 == 212 ~ 12,
    ALCDAY4 == 213 ~ 13,
    ALCDAY4 == 214 ~ 14,
    ALCDAY4 == 215 ~ 15,
    ALCDAY4 == 216 ~ 16,
    ALCDAY4 == 217 ~ 17,
    ALCDAY4 == 218 ~ 18,
    ALCDAY4 == 219 ~ 19,
    ALCDAY4 == 220 ~ 20,
    ALCDAY4 == 221 ~ 21,
    ALCDAY4 == 222 ~ 22,
    ALCDAY4 == 223 ~ 23,
    ALCDAY4 == 224 ~ 24,
    ALCDAY4 == 225 ~ 25,
    ALCDAY4 == 226 ~ 26,
    ALCDAY4 == 227 ~ 27,
    ALCDAY4 == 228 ~ 28,
    ALCDAY4 == 229 ~ 29,
    ALCDAY4 == 230 ~ 30)) |>
  mutate(sex = if_else(sex == 1, "Male", "Female")) |>
  filter(!is.na(permonth))
  


permonth2 = permonth2 |> 
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

sumsACEpermonth= rowSums(permonth2, na.rm = TRUE)


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

for (x in 1:121229) {
  part = permonth2 |> slice(x)
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



permonth3 = permonth |>
  mutate(Categories = Categories) |>
  mutate(Categories = str_squish(Categories)) |>
  mutate(Categories = as.factor(Categories)) |>
  mutate(sum = rowSums(permonth2, na.rm = TRUE)) |>
  select(-ALCDAY4, -ALCDAY5, -race, -number)


permonth4 = permonth3 |> 
  group_by(Categories, sum) |>
  summarise(n = n()) |>
  arrange(desc(sum)) |>
  filter(n >= 25) 


left_join(permonth4, permonth3) |>
  group_by(Categories, sum) |>
  summarise(mean = mean(permonth),
            n = n(),
            sd = sd(permonth),
            se = sd/sqrt(n)) |>
  filter(n >= 15) |>
  filter(str_detect(Categories, " SA3 ") | Categories == "SA3") |>
  arrange(sum) |>
  ungroup() |>
  mutate(Categories = fct_reorder(Categories, sum)) |>
  mutate(sum = as.factor(sum)) |>
  ggplot(aes(x = mean, y = Categories, col = sum)) + 
  geom_point(shape = 21, stroke = 1) + 
  geom_errorbar(aes(xmax = mean+se, xmin = mean-se, col = sum), width = 0.3) + 
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(x = "Average Alcoholic Drinks Per Month") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_text(color = "black", size = 9, family = "Avenir"),
    axis.title.x = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 12, family = "Avenir"),
    plot.title = element_text(color = "black", size = 24, family = "Avenir"),
    legend.text = element_text(color = "black", size = 10, family = "Avenir"),
    legend.position = "top", 
    legend.direction = "horizontal", 
    legend.box = "horizontal", 
    legend.title = element_blank()
  )













































grid2 <- permonth |>
  data_grid(
    perweek = c(1:30),
    sex = c("Female", "Male"),
    age = c("18 to 24", "25 to 34", "35 to 44", "45 to 54", "55 to 64", "65 or > 65"),
    MentalIllness = c("0", "1"),
    Alcoholic = c("0", "1"),
    SubstanceAddiction = c("0", "1"),
    Incarcerated = c("0", "1"),
    SeparationDivorce = c("0", "1"),
    DomesticViolence = c("0", "1"),
    PhysicalAbuse = c("0", "1"),
    EmotionalAbuse = c("0", "1"),
    SexualAbuse1 = c("0", "1"),
    SexualAbuse2 = c("0", "1"),
    SexualAbuse3 = c("0", "1"),
    Neglect1 = c("0", "1"),
    Neglect2 = c("0", "1"),
  ) 
grid2

aug_permonth <- augment(permonthall4, newdata = grid2, se_fit = TRUE) |>
  mutate(.fittedprob = exp(.fitted) / (1 + exp(.fitted)))


one = aug_permonth |> filter(
  MentalIllness == "1" &
    Alcoholic == "0" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "1" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "0" & 
    SubstanceAddiction == "1" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "0" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "1" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "0" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "1" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "0" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "1" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "0" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "1" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "0" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "1" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "0" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "1" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "1" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "1" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "1" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "1" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "0" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "1" &
    Neglect1 == "0" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "0" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "0" &
    Neglect1 == "1" &
    Neglect2 == "0" |
    MentalIllness == "0" &
    Alcoholic == "0" & 
    SubstanceAddiction == "0" & 
    Incarcerated == "0" & 
    SeparationDivorce == "0" &
    DomesticViolence == "0" & 
    PhysicalAbuse == "0" & 
    EmotionalAbuse == "0" & 
    SexualAbuse1 == "0" &
    SexualAbuse2 == "0" & 
    SexualAbuse3 == "0" &
    Neglect1 == "0" &
    Neglect2 == "1"
) |>
  pivot_longer(c(4:16), names_to = "Category", values_to = "yesno") |>
  filter(yesno != "0") 

two = aug_permonth |>
  filter(
    MentalIllness == "0" &
      Alcoholic == "0" & 
      SubstanceAddiction == "0" & 
      Incarcerated == "0" & 
      SeparationDivorce == "0" &
      DomesticViolence == "0" & 
      PhysicalAbuse == "0" & 
      EmotionalAbuse == "0" & 
      SexualAbuse1 == "0" &
      SexualAbuse2 == "0" & 
      SexualAbuse3 == "0" &
      Neglect1 == "0" &
      Neglect2 == "0") |>
  pivot_longer(c(4:16), names_to = "Category", values_to = "yesno") |>
  mutate(Category = "None")



full_join(one, two) |> 
  mutate(Category = fct_reorder(Category, .fittedprob)) |>
  ggplot(aes(y = Category, x = .fittedprob)) +
  geom_boxplot(outliers = FALSE) +
  geom_point(aes(col = age, shape = sex, stroke = 0.8)) +
  facet_wrap(~perweek) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(title = "Alcohol consumption in days per month", x = "Probability to Consume Alcohol") +
  theme(
    strip.text = element_text(color = "black", size = 16, family = "Avenir"),
    plot.title = element_text(color = "black", size = 20, family = "Avenir"),
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.y = element_text(color = "black", size = 13, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 16, family = "Avenir"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 20, family = "Avenir"),
    legend.position = c(0.70, 0.2),
    legend.direction = "horizontal",
    #legend.direction = "vertical",
    legend.box = "horizontal"
  ) 







permonth2 = everything |> select(MentalIllness, Alcoholic, Incarcerated, SeparationDivorce, DomesticViolence, PhysicalAbuse, EmotionalAbuse, SexualAbuse1, SexualAbuse2, SexualAbuse3, Neglect1, Neglect2, SubstanceAddiction, ALCDAY4, ALCDAY5, sex, age, race, number) |>
  mutate(permonth = case_when(
    ALCDAY5 == 201 ~ 1, 
    ALCDAY5 == 202 ~ 2,
    ALCDAY5 == 203 ~ 3,
    ALCDAY5 == 204 ~ 4,
    ALCDAY5 == 205 ~ 5,
    ALCDAY5 == 206 ~ 6,
    ALCDAY5 == 207 ~ 7,
    ALCDAY5 == 208 ~ 8,
    ALCDAY5 == 209 ~ 9,
    ALCDAY5 == 210 ~ 10, 
    ALCDAY5 == 211 ~ 11,
    ALCDAY5 == 212 ~ 12,
    ALCDAY5 == 213 ~ 13,
    ALCDAY5 == 214 ~ 14,
    ALCDAY5 == 215 ~ 15,
    ALCDAY5 == 216 ~ 16,
    ALCDAY5 == 217 ~ 17,
    ALCDAY5 == 218 ~ 18,
    ALCDAY5 == 219 ~ 19,
    ALCDAY5 == 220 ~ 20,
    ALCDAY5 == 221 ~ 21,
    ALCDAY5 == 222 ~ 22,
    ALCDAY5 == 223 ~ 23,
    ALCDAY5 == 224 ~ 24,
    ALCDAY5 == 225 ~ 25,
    ALCDAY5 == 226 ~ 26,
    ALCDAY5 == 227 ~ 27,
    ALCDAY5 == 228 ~ 28,
    ALCDAY5 == 229 ~ 29,
    ALCDAY5 == 230 ~ 30, 
    
    ALCDAY4 == 201 ~ 1, 
    ALCDAY4 == 202 ~ 2,
    ALCDAY4 == 203 ~ 3,
    ALCDAY4 == 204 ~ 4,
    ALCDAY4 == 205 ~ 5,
    ALCDAY4 == 206 ~ 6,
    ALCDAY4 == 207 ~ 7,
    ALCDAY4 == 208 ~ 8,
    ALCDAY4 == 209 ~ 9,
    ALCDAY4 == 210 ~ 10, 
    ALCDAY4 == 211 ~ 11,
    ALCDAY4 == 212 ~ 12,
    ALCDAY4 == 213 ~ 13,
    ALCDAY4 == 214 ~ 14,
    ALCDAY4 == 215 ~ 15,
    ALCDAY4 == 216 ~ 16,
    ALCDAY4 == 217 ~ 17,
    ALCDAY4 == 218 ~ 18,
    ALCDAY4 == 219 ~ 19,
    ALCDAY4 == 220 ~ 20,
    ALCDAY4 == 221 ~ 21,
    ALCDAY4 == 222 ~ 22,
    ALCDAY4 == 223 ~ 23,
    ALCDAY4 == 224 ~ 24,
    ALCDAY4 == 225 ~ 25,
    ALCDAY4 == 226 ~ 26,
    ALCDAY4 == 227 ~ 27,
    ALCDAY4 == 228 ~ 28,
    ALCDAY4 == 229 ~ 29,
    ALCDAY4 == 230 ~ 30)) |>
  mutate(sex = if_else(sex == 1, "Male", "Female")) |>
  mutate(sex = as.factor(sex)) |>
  filter(!is.na(permonth))


permonthSA3 = 
  permonth2 |> 
  select(c(1:13)) |>
  filter(SexualAbuse3 == 1) |>
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


sumsSA3 = rowSums(permonthSA3, na.rm = TRUE)


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

for (x in 1:5333) {
  part = permonthSA3 |> slice(x)
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

Categories

permonthSA32 = permonth2 |> 
  filter(SexualAbuse3 == 1) |>
  mutate(Categories = Categories) |>
  mutate(Categories = as.factor(Categories))



permonthSA33 = 
  permonthSA32 |>
  group_by(Categories) |>
  summarise(n = n()) |>
  arrange(desc(n)) |>
  filter(n >= 25)


permonthSA34 = left_join(permonthSA33, permonthSA32, by = "Categories") |>
  select(-n)






permonthSA34 |>
  ggplot(aes(x = permonth, col = Categories)) +
  geom_density()


permonthSA32 |>
  mutate(sum = rowSums(permonthSA3, na.rm = TRUE)) |>
  group_by(sum) |>
  summarise(n = n())
  

aug_permonthSA3 = aug_permonth |>
  mutate(sex = as.factor(sex),
         age = as.numeric(age),
         MentalIllness = as.numeric(MentalIllness),
         Alcoholic = as.numeric(Alcoholic),
         Incarcerated = as.numeric(Incarcerated),
         SeparationDivorce = as.numeric(SeparationDivorce),
         DomesticViolence = as.numeric(DomesticViolence),
         PhysicalAbuse = as.numeric(PhysicalAbuse),
         EmotionalAbuse = as.numeric(EmotionalAbuse),
         SexualAbuse1 = as.numeric(SexualAbuse1),
         SexualAbuse2 = as.numeric(SexualAbuse2),
         SexualAbuse3 = as.numeric(SexualAbuse3),
         Neglect1 = as.numeric(Neglect1),
         Neglect2 = as.numeric(Neglect2),
         SubstanceAddiction = as.numeric(SubstanceAddiction)) |>
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
  

permonthSA3predictions = left_join(permonthSA32, aug_permonthSA3)


permonthSA3predictions |>
  select(Categories, permonth, .fittedprob)









permonthSA32 |> 
  mutate(sum = rowSums(permonthSA3, na.rm = TRUE)) |>
  mutate(sum = as.factor(sum)) |>
  filter(sum == 1) |>
ggplot(aes(x = permonth, col = Categories, group = Categories)) +
  geom_density(show.legend = FALSE) +
  facet_wrap(~sum) +
  theme(
    legend.text = element_text(color = "black", size = 3),
    legend.position = c(0.99, 0.99),
    legend.direction = "vertical",
    legend.box = "vertical"
  )

permonthSA32 |> 
  mutate(sum = rowSums(permonthSA3, na.rm = TRUE)) |>
  mutate(sum = as.factor(sum)) |>
  filter(sum == 2) |>
  ggplot(aes(x = permonth, col = Categories, group = Categories)) +
  geom_density() +
  facet_wrap(~sum) +
  theme(
    legend.text = element_text(color = "black", size = 7),
    legend.direction = "vertical",
    legend.box = "vertical"
  )


permonthSA32 |> 
  mutate(sum = rowSums(permonthSA3, na.rm = TRUE)) |>
  mutate(sum = as.factor(sum)) |>
  filter(sum == 3) |>
  ggplot(aes(x = permonth, col = Categories, group = Categories)) +
  geom_density() +
  facet_wrap(~sum) +
  theme(
    legend.text = element_text(color = "black", size = 7),
    legend.direction = "vertical",
    legend.box = "vertical"
  )

permonthSA32 |> 
  mutate(sum = rowSums(permonthSA3, na.rm = TRUE)) |>
  mutate(sum = as.factor(sum)) |>
  filter(sum == 4) |>
  ggplot(aes(x = permonth, col = Categories, group = Categories)) +
  geom_density() +
  facet_wrap(~sum) +
  theme(
    legend.text = element_text(color = "black", size = 7),
    legend.direction = "vertical",
    legend.box = "vertical"
  )


permonthSA32 |> 
  mutate(sum = rowSums(permonthSA3, na.rm = TRUE)) |>
  mutate(sum = as.factor(sum)) |>
  filter(sum == 5) |>
  ggplot(aes(x = permonth, col = Categories, group = Categories)) +
  geom_density() +
  facet_wrap(~sum) +
  theme(
    legend.text = element_text(color = "black", size = 7),
    legend.direction = "vertical",
    legend.box = "vertical"
  )










full_join(one, two) |> 
  filter(Category == "SexualAbuse3" | Category == "None") |>
  mutate(Category = fct_reorder(Category, .fittedprob)) |>
ggplot(aes(x = perweek, y = .fittedprob)) +
  geom_point(aes(col = Category))












  geom_line(aes(group = age, col = Category))



  geom_point(aes(col = age, shape = sex, stroke = 0.8)) +
  facet_wrap(~perweek) +
  scale_color_viridis_d(option = "plasma", direction = 1, begin = 0.85, end = 0.35) +
  scale_shape_manual(values = c(1, 6)) +
  theme_minimal() +
  labs(title = "Alcohol consumption in days per month", x = "Probability to Consume Alcohol") +
  theme(
    strip.text = element_text(color = "black", size = 16, family = "Avenir"),
    plot.title = element_text(color = "black", size = 20, family = "Avenir"),
    legend.title = element_blank(),
    legend.text = element_text(color = "black", size = 15, family = "Avenir"),
    axis.text.y = element_text(color = "black", size = 13, family = "Avenir"),
    axis.text.x = element_text(color = "black", size = 16, family = "Avenir"),
    axis.title.y = element_blank(),
    axis.title.x = element_text(color = "black", size = 20, family = "Avenir"),
    legend.position = c(0.70, 0.2),
    legend.direction = "horizontal",
    #legend.direction = "vertical",
    legend.box = "horizontal"
  ) 







