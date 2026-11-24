poemAnalysis_v3_1 <- function(spreadSheetName = "POEM_v3.1_April 11, 2024_12.18.csv") {
  # Processes, classifies, and summarizes POEM v3.1 (SV_cub6KY8wuvPSBUi) data.
  #
  # Args:
  #   spreadSheetName: Path to the Qualtrics POEM v3.1 CSV file.
  #
  # Returns:
  #   A named list with three data.frames:
  #     Results - Summary table of demographics, diagnoses, and scores.
  #     T       - Full pre-processed data table.
  #     Dx      - ICHD-3 diagnostic classification table.

  ## 1. Load, Rename, and Filter Table
  # Skip header (row 1) and Qualtrics metadata (rows 2-3) so numeric columns import cleanly
  df <- read.csv(
    spreadSheetName,
    skip = 3,
    header = FALSE,
    col.names = get_poem_v31_column_names(),
    na.strings = c("", "NA"),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  # Drop completely empty rows and columns
  df <- df[rowSums(!is.na(df)) > 0, , drop = FALSE]
  df <- df[, colSums(!is.na(df)) > 0, drop = FALSE]

  # Keep finished, valid, non-preview responses
  is_valid <- (is.na(df$Finished) | toupper(trimws(as.character(df$Finished))) != "FALSE") &
              !grepl("No", as.character(df$AnsCorrect), fixed = TRUE) &
              !grepl("Survey Preview", as.character(df$Status), fixed = TRUE)
  df <- df[is_valid, , drop = FALSE]
  rownames(df) <- NULL

  # Convert demographic fields to factors
  cat_cols <- c("UserLanguage", "RaceEth", "GenderID", "SAAB", "SO")
  df[cat_cols] <- lapply(df[cat_cols], factor)

  # Matching helpers (return numeric 0/1; treat NA as 0)
  is_yes <- function(x) as.numeric(!is.na(x) & as.character(x) == "Yes")
  has_text <- function(x, patterns) {
    s <- ifelse(is.na(x), "", as.character(x))
    matched <- Reduce(`|`, lapply(patterns, function(p) grepl(p, s, fixed = TRUE)))
    as.numeric(matched)
  }

  ## 2. ICHD-3 Migraine & Aura Criteria
  # Combine >4 hr (MigCritC/D) and <4 hr (MigCritC2/D2) symptom columns
  migCritC_all <- paste(ifelse(is.na(df$MigCritC), "", df$MigCritC),
                        ifelse(is.na(df$MigCritC2), "", df$MigCritC2))
  migCritD_all <- paste(ifelse(is.na(df$MigCritD), "", df$MigCritD),
                        ifelse(is.na(df$MigCritD2), "", df$MigCritD2))

  df$migICHD_A   <- is_yes(df$MigCritA)
  df$migICHD_B   <- is_yes(df$MigCritB)
  df$migICHD_C1  <- has_text(migCritC_all, "worse on one side")
  df$migICHD_C2  <- has_text(migCritC_all, "pounding, pulsating, or throbbing")
  df$migICHD_C3  <- has_text(migCritC_all, "moderate or severe in intensity")
  df$migICHD_C4  <- has_text(migCritC_all, "made worse by routine activities")
  df$migICHD_C   <- df$migICHD_C1 + df$migICHD_C2 + df$migICHD_C3 + df$migICHD_C4
  df$migICHD_Dp  <- as.numeric(has_text(migCritD_all, "Sensitivity to light") &
                               has_text(migCritD_all, "Sensitivity to sound"))
  df$migICHD_Dnv <- has_text(migCritD_all, "Nausea and/or vomiting")
  df$migICHD_D   <- as.numeric(df$migICHD_Dp | df$migICHD_Dnv)

  # Aura criteria
  aura_ha <- c("Before the headache", "During the headache")
  df$visAuraICHD_A    <- is_yes(df$VisAura2orMore)
  df$sensAuraICHD_A   <- is_yes(df$SensAura2orMore)
  df$speechAuraICHD_A <- as.numeric(is_yes(df$SpeechAura2orMore) &
                                    has_text(df$SpeechAura, c("Trouble saying words", "Unable to speak")))

  df$visAuraICHD_C1    <- is_yes(df$VisAuraSpread)
  df$sensAuraICHD_C1   <- is_yes(df$SensAuraSpread)
  df$AuraICHD_C2       <- has_text(df$MultiSxAura_sametime, "No")
  df$visAuraICHD_C3    <- as.numeric(has_text(df$VisAuraDur, "5 min to 1 hour")    | is_yes(df$VisAuraDur_yn))
  df$sensAuraICHD_C3   <- as.numeric(has_text(df$SensAuraDur, "5 min to 1 hour")   | is_yes(df$SensAuraDur_yn))
  df$speechAuraICHD_C3 <- as.numeric(has_text(df$SpeechAuraDur, "5 min to 1 hour") | is_yes(df$SpeechAuraDur_yn))
  df$visAuraICHD_C4    <- is_yes(df$VisAuraUni)
  df$sensAuraICHD_C4   <- is_yes(df$SensAuraUni)
  df$speechAuraICHD_C4 <- has_text(df$SpeechAura, "Unable to speak")
  df$visAuraICHD_C5    <- has_text(df$VisAura, c("Zigzag lines", "Spots", "Stars", "Flashing lights", "Lines", "Heat waves"))
  df$sensAuraICHD_C5   <- has_text(df$SensAura, "Tingling")
  df$visAuraICHD_C6    <- has_text(df$VisAuraHArelation, aura_ha)
  df$sensAuraICHD_C6   <- has_text(df$SensAuraHArelation, aura_ha)
  df$speechAuraICHD_C6 <- has_text(df$SpeechAuraHArelation, aura_ha)

  df$visAuraICHD_C    <- df$visAuraICHD_C1 + df$AuraICHD_C2 + df$visAuraICHD_C3 + df$visAuraICHD_C4 + df$visAuraICHD_C5 + df$visAuraICHD_C6
  df$sensAuraICHD_C   <- df$sensAuraICHD_C1 + df$AuraICHD_C2 + df$sensAuraICHD_C3 + df$sensAuraICHD_C4 + df$sensAuraICHD_C5 + df$sensAuraICHD_C6
  df$speechAuraICHD_C <- df$AuraICHD_C2 + df$speechAuraICHD_C3 + df$speechAuraICHD_C4 + df$speechAuraICHD_C6

  ## 3. Symptom, Burden, Allodynia, TheraSpecs, and Pseudo-VLSQ-8 Scores
  # Pseudo-MIDAS
  midas_cols <- c("HAmissedWork3mo", "HAimpactWork3mo", "HAmissedHousework3mo", "HAimpactHousework3mo", "HAmissedSocial3mo")
  midas_keys <- c("1 - 3 days", "4 - 11 days", "12 - 24 days", "25 - 48 days", "49 - 72 days", "73 - 90 days")
  midas_vals <- c(2, 7, 18, 36, 60, 81)
  for (i in seq_along(midas_cols)) {
    df[[paste0("MIDAS", i)]] <- map_contains(df[[midas_cols[i]]], midas_keys, midas_vals, default_val = 0)
  }
  df$MIDAS_score <- rowSums(df[paste0("MIDAS", 1:5)])

  # Frequency & Continuous HA
  freq_keys <- c("0 - 4 days", "5 - 9 days", "10 - 15 days", "16 - 20 days", "More than 20 days")
  df$MigraineFrequency <- map_contains(df$MigFreq, freq_keys, 1:5, default_val = NA_real_)
  df$ContinuousHA      <- has_text(df$contHAsomeBreaks, "no breaks")

  # Allodynia (ASC-12, combining HA and headache-free blocks)
  allo_stems <- c("Comb", "Ponytail", "Shave", "EyeGlasses", "Contacts", "Earrings",
                  "Necklace", "TightClothes", "Shower", "Pillow", "Heat", "Cold")
  for (i in seq_along(allo_stems)) {
    s1 <- ifelse(is.na(df[[paste0("Allodynia", allo_stems[i])]]), "", df[[paste0("Allodynia", allo_stems[i])]])
    s2 <- ifelse(is.na(df[[paste0("Allodynia", allo_stems[i], "_noHA")]]), "", df[[paste0("Allodynia", allo_stems[i], "_noHA")]])
    score <- as.numeric(grepl("Less than half the time", s1, fixed = TRUE) | grepl("Less than half the time", s2, fixed = TRUE))
    score[grepl("Half the time or more", s1, fixed = TRUE) | grepl("Half the time or more", s2, fixed = TRUE)] <- 2
    df[[paste0("allodynia", i)]] <- score
  }
  df$allodynia_score <- rowSums(df[paste0("allodynia", 1:12)])

  # Associated & Visual Aura symptom indicators
  df$photophobia  <- has_text(migCritD_all, "Sensitivity to light")
  df$phonophobia  <- has_text(migCritD_all, "Sensitivity to sound")
  df$nausea_vomit <- has_text(migCritD_all, "Nausea")

  sx_spec <- list(
    c("osmophobia",   "CAMS",    "Smell sensitivity"),
    c("lightheaded",  "CAMS",    "Light headedness"),
    c("blurryVision", "CAMS",    "Blurry vision"),
    c("thinking",     "CAMS",    "Difficulty thinking"),
    c("neckPain",     "CAMS",    "Neck pain"),
    c("balance",      "CAMS",    "Balance problems"),
    c("spinning",     "CAMS",    "Room spinning"),
    c("ringing",      "CAMS",    "Ear ringing"),
    c("doubleVision", "CAMS",    "Double vision"),
    c("visZigzag",    "VisAura", "Zigzag lines"),
    c("visFlash",     "VisAura", "Flashing lights"),
    c("visHeatWaves", "VisAura", "Heat waves"),
    c("visStars",     "VisAura", "Stars"),
    c("visSpots",     "VisAura", "Spots"),
    c("visLines",     "VisAura", "Lines"),
    c("visLoss",      "VisAura", "Vision loss")
  )
  for (spec in sx_spec) {
    df[[spec[1]]] <- has_text(df[[spec[2]]], spec[3])
  }

  # TheraSpecs Photophobia Inventory (0 = Never ... 4 = Always)
  ts_map <- c(
    Light_HA = "lightHA", Light_Dizzy = "lightDizzy", Light_EyeStrain = "lightEyeStrain",
    Light_BlurryVision = "lightBlurryVision", Light_LightIntolerant = "lightIntolerant",
    Light_Anxiety = "lightAnxiety", Fluorescent = "lightFluorescent", Flicker = "lightFlicker",
    Headlight = "lightHeadlight", OutdoorGlare = "lightOutdoorGlare", Screen = "lightScreen",
    Sunlight = "lightSunlight", FlickeringTrees = "lightTrees", SunglassesSunlight = "lightGlassesSun",
    SunglassesIndoors = "lightGlassesInd", SeekDarkness = "lightSeekDark", LightTvLimit = "lightLimTV",
    LightDeviceLimit = "lightLimDevice", LightShopsLimit = "lightLimShops", LightDriveLimit = "lightLimDrive",
    LightWorkLimit = "lightLimWork", LightOutdoorLimit = "lightLimOutdoor"
  )
  for (src_col in names(ts_map)) {
    df[[ts_map[[src_col]]]] <- map_contains(df[[src_col]], c("Rarely", "Sometimes", "Often", "Always"), 1:4, default_val = 0)
  }

  df$lightSx_score        <- rowSums(df[c("lightHA", "lightDizzy", "lightEyeStrain", "lightBlurryVision", "lightIntolerant", "lightAnxiety")])
  df$lightBother_score    <- rowSums(df[c("lightFluorescent", "lightFlicker", "lightOutdoorGlare", "lightTrees", "lightSunlight", "lightHeadlight", "lightScreen")])
  df$lightAvoidance_score <- rowSums(df[c("lightGlassesSun", "lightGlassesInd", "lightSeekDark", "lightLimTV", "lightLimDevice", "lightLimShops", "lightLimDrive", "lightLimWork", "lightLimOutdoor")])
  df$lightFlicker_score   <- rowSums(df[c("lightFluorescent", "lightFlicker", "lightTrees", "lightScreen")])
  df$lightStatic_score    <- rowSums(df[c("lightOutdoorGlare", "lightSunlight", "lightHeadlight")])

  # Pseudo VLSQ-8 (Verriotto et al., 2017; each item coded 1 = Never ... 5 = Always, total 8-40)
  df$vlsq1 <- df$lightSunlight + 1                            # Q1: Outdoors during daylight
  df$vlsq2 <- df$lightOutdoorGlare + 1                        # Q2: Sense of glare
  df$vlsq3 <- df$lightFlicker + 1                             # Q3: Flickering lights
  df$vlsq4 <- as.numeric(df$LightSensitivity)                 # Q4: Worst severity in past 30 days (1-5)
  df$vlsq5 <- df$lightHA + 1                                  # Q5: Light-induced headache
  df$vlsq6 <- df$lightBlurryVision + 1                        # Q6: Light-induced blurry vision
  df$vlsq7 <- pmax(df$lightLimTV, df$lightLimDevice) + 1      # Q7: Limits TV or computer/device use
  df$vlsq8 <- df$lightGlassesInd + 1                          # Q8: Dark glasses indoors
  df$vlsq8_score <- rowSums(df[paste0("vlsq", 1:8)])

  ## 4. ICHD-3 Diagnostic Classification
  Dx <- data.frame(
    ZipCode = df$ZipCode,
    MigraineFrequency = df$MigFreq,
    stringsAsFactors = FALSE
  )

  crit_count          <- df$migICHD_A + df$migICHD_B + as.numeric(df$migICHD_C >= 2) + df$migICHD_D
  Dx$Migraine         <- as.numeric(crit_count == 4)
  Dx$ProbableMigraine <- as.numeric(crit_count == 3)
  Dx$Chronic          <- has_text(df$MigFreq, c("16 - 20 days", "More than 20 days"))

  Dx$VisualAura <- as.numeric(df$visAuraICHD_A == 1 & df$visAuraICHD_C >= 3)
  Dx$SensAura   <- as.numeric(df$sensAuraICHD_A == 1 & df$sensAuraICHD_C >= 3)
  Dx$SpeechAura <- as.numeric(df$speechAuraICHD_A == 1 & df$speechAuraICHD_C >= 3)
  Dx$MultiAura  <- as.numeric((Dx$VisualAura + Dx$SensAura + Dx$SpeechAura) >= 2)

  has_aura <- (Dx$VisualAura == 1) | (Dx$SensAura == 1) | (Dx$SpeechAura == 1)
  no_ha    <- (has_text(df$unprovokedHA_yn, "No") == 1) |
              (has_text(df$HA_yn, "No") == 1 & has_text(df$everHA, "No") == 1 & has_text(df$everDiscomfort, "No") == 1)

  dx_code <- rep(NA_real_, nrow(df))
  dx_code[Dx$Migraine == 1 & !has_aura]         <- 1
  dx_code[Dx$Migraine == 1 &  has_aura]         <- 2
  dx_code[Dx$ProbableMigraine == 1 & !has_aura] <- 3
  dx_code[Dx$ProbableMigraine == 1 &  has_aura] <- 4
  dx_code[has_text(df$unprovokedHA_yn, "Yes") == 1 & Dx$Migraine == 0 & Dx$ProbableMigraine == 0] <- 5
  dx_code[no_ha] <- 6

  dx_labels <- c(
    "migraine w/out aura", "migraine w/aura", "probable migraine w/out aura",
    "probable migraine w/aura", "non-migraine headache", "no headache"
  )
  Dx$Dx <- factor(dx_code, levels = 1:6, labels = dx_labels)

  Dx$Dx2 <- Dx$Dx
  levels(Dx$Dx2) <- list(
    "migraine"              = dx_labels[1:4],
    "non-migraine headache" = "non-migraine headache",
    "no headache"           = "no headache"
  )

  df$Dx <- Dx$Dx
  df$DxSimple <- Dx$Dx2
  levels(df$DxSimple) <- list(
    "not migraine" = c("non-migraine headache", "no headache"),
    "migraine"     = "migraine"
  )

  ## 5. Compile Summary Results Table
  Results <- data.frame(
    ResponseID        = factor(df$ResponseID),
    Age               = df$Age,
    ZipCode           = df$ZipCode,
    RaceEthnicity     = df$RaceEth,
    GenderID          = df$GenderID,
    SAAB              = df$SAAB,
    SO                = df$SO,
    HAageOnset        = factor(df$HAageOnset),
    MigraineFamHx     = factor(df$MigFamHx),
    DxFull            = df$Dx,
    DxSimple          = df$DxSimple,
    MIDAS             = df$MIDAS_score,
    MigraineFrequency = df$MigraineFrequency,
    HAseverity        = df$SeverityHA,
    ContinuousHA      = df$ContinuousHA,
    LightSensScore    = df$lightSx_score + df$lightBother_score + df$lightAvoidance_score - df$lightGlassesSun,
    PseudoVLSQ8       = df$vlsq8_score,
    AllodyniaScore    = df$allodynia_score,
    interAllodynia    = factor(df$AllodyniaNoHA),
    PhoticSneeze      = factor(as.character(df$LightSneeze)),
    MotionSick        = factor(df$MotionSick),
    stringsAsFactors  = FALSE
  )

  list(Results = Results, T = df, Dx = Dx)
}

## Helper Functions
map_contains <- function(col, keys, vals, default_val = 0) {
  s <- ifelse(is.na(col), "", as.character(col))
  out <- rep(default_val, length(s))
  for (k in seq_along(keys)) {
    out[grepl(keys[k], s, fixed = TRUE)] <- vals[k]
  }
  out
}

get_poem_v31_column_names <- function() {
  c(
    "StartDate","EndDate","Status","IPAddress","Progress","DurationToCompleteSec",
    "Finished","RecordedDate","ResponseID","LastName","FirstName","Email","ExternalReference","LocationLatitude",
    "LocationLongitude","DistributionChannel","UserLanguage","Q_RecaptchaScore","Age","ZipCode","RaceEth","AIAN","AIANtxt",
    "AIANtribe","AIANtribe_txt","Asian","Asian_txt","Black","Black_txt","Hispanic","Hispanic_txt","MENA","MENA_txt",
    "NHPI","NHPI_txt","White","White_txt","RaceEth_txt","GenderID","GenderID2","GenderID_txt","SAAB","SAAB_txt","SO",
    "SO2","SO_txt","HA_yn","unprovokedHA_yn","MigCritB","MigCritC","MigCritD","MigCritC2","MigCritD2","MigCritA",
    "MigFreq","MigRecent","VisAura","SensAura","SpeechAura","MultiSxAura_sametime","VisAura2orMore","VisAuraHArelation",
    "VisAuraRecent","VisAuraDur","VisAuraUni","VisAuraSpread","VisAuraDur_yn","SensAura2orMore","SensAuraHArelation",
    "SensAuraRecent","SensAuraDur","SensAuraUni","SensAuraSpread","SensAuraDur_yn","SpeechAura2orMore","SpeechAuraHArelation",
    "SpeechAuraHArecent","SpeechAuraDur","SpeechAuraDur_yn","menstrualMig","CAMS","contHA","contHAsomeBreaks","HAmissedWork3mo",
    "HAimpactWork3mo","HAmissedHousework3mo","HAimpactHousework3mo","HAmissedSocial3mo","HAdays3mo","SeverityHA",
    "HAageOnset","MotionSick","MigFamHx","LightSneeze","Light_HA","Light_Dizzy","Light_EyeStrain","Light_BlurryVision",
    "Light_LightIntolerant","Light_Anxiety","Fluorescent","Flicker","Headlight","OutdoorGlare","Screen","Sunlight",
    "FlickeringTrees","LightSensitivity","SunglassesSunlight","SunglassesIndoors","SeekDarkness","LightTvLimit","LightDeviceLimit",
    "LightShopsLimit","LightDriveLimit","LightWorkLimit","LightOutdoorLimit","AllodyniaComb","AllodyniaPonytail","AllodyniaShave",
    "AllodyniaEyeGlasses","AllodyniaContacts","AllodyniaEarrings","AllodyniaNecklace","AllodyniaTightClothes","AllodyniaShower",
    "AllodyniaPillow","AllodyniaHeat","AllodyniaCold","AllodyniaNoHA","AnsCorrect","Research","AllodyniaComb_noHA","AllodyniaPonytail_noHA",
    "AllodyniaShave_noHA","AllodyniaEyeGlasses_noHA","AllodyniaContacts_noHA","AllodyniaEarrings_noHA","AllodyniaNecklace_noHA",
    "AllodyniaTightClothes_noHA","AllodyniaShower_noHA","AllodyniaPillow_noHA","AllodyniaHeat_noHA","AllodyniaCold_noHA",
    "everHA","everDiscomfort"
  )
}