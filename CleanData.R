

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


write.csv(everything, "everything.csv")


