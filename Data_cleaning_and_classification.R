# Mobile App Data Analysis and Visualization
# Author: Owais Raza (took help with Git's copilot)
# Date: May 29 2025
# Listening to: Sohrab Pournazeri - Introduction Vocal Improvisation
# Description: Data_cleaning_and_classification


# ---- 1. Package Setup ----
required_packages <- c(
  "readxl", "dplyr", "stringr", "tidyr", "tools"
)
missing <- setdiff(required_packages, rownames(installed.packages()))
if (length(missing)) install.packages(missing)
lapply(required_packages, function(pkg) suppressPackageStartupMessages(library(pkg, character.only = TRUE)))

# ---- 2. Data Loading ----
df <- readxl::read_excel("MobileAppData.xlsx")

# ---- 3. Unified Category Mapping ----
category_map <- list(
  "Navigation" = c("Maps & Navigation", "Auto & Vehicles", "GPS Navigation"),
  "Business" = c("Business", "Finance"),
  "Travel" = c("Travel & Local"),
  "Utilities" = c("Productivity", "Tools", "Utilities"),
  "Lifestyle & Entertainment" = c("Lifestyle", "Personalization", "Shopping", "Health & Fitness", 
                                  "Medical", "Entertainment", "Social", "Communication", "News & Magazines"),
  "Education" = c("Education", "Books & Reference", "Libraries & Demo", "Reference")
)
normalize_category <- function(x) tolower(trimws(as.character(x)))
get_unified_category <- function(app_cat, play_cat) {
  app_cat <- normalize_category(app_cat)
  play_cat <- normalize_category(play_cat)
  for (unified in names(category_map)) {
    mapped <- tolower(category_map[[unified]])
    if (!is.na(app_cat) && app_cat %in% mapped) return(unified)
    if (!is.na(play_cat) && play_cat %in% mapped) return(unified)
  }
  return("Other")
}

# ---- 4. Haddon Factor Determination ----
human_keywords <- c(
  "driver", "driving", "learn", "training", "education", "skill", "practice",
  "behavior", "behaviour", "habit", "coaching", "instructor",
  "alert", "attention", "focus", "concentration", "awareness", "vigilant",
  "fatigue", "drowsy", "tired", "sleep", "rest", "break",
  "alcohol", "drunk", "breathalyzer", "sobriety", "impair", "substance",
  "drug", "medication", "blood alcohol",
  "health", "medical", "fitness", "wellness", "stress", "anxiety",
  "vision", "hearing", "reaction time", "cognitive",
  "distraction", "phone", "text", "call", "hands-free", "do not disturb",
  "silent", "block", "disable", "focus mode"
)
vehicle_keywords <- c(
  "maintenance", "service", "repair", "diagnostic", "engine", "brake",
  "tire", "oil", "battery", "fluid", "inspection", "recall",
  "airbag", "seatbelt", "abs", "traction", "stability", "cruise control",
  "collision", "automatic", "sensor", "camera", "radar", "lidar",
  "fuel", "mileage", "efficiency", "consumption", "performance",
  "dashboard", "warning", "indicator", "malfunction", "error code",
  "theft", "alarm", "immobilizer", "tracking", "stolen", "security",
  "remote start", "lock", "unlock", "keyless"
)
physical_env_keywords <- c(
  "navigation", "gps", "map", "route", "direction", "turn-by-turn",
  "shortest", "fastest", "alternative route", "waypoint",
  "traffic", "congestion", "jam", "road", "highway", "construction",
  "accident", "incident", "closure", "detour", "real-time",
  "weather", "rain", "snow", "ice", "fog", "wind", "temperature",
  "visibility", "condition", "forecast", "storm",
  "parking", "gas station", "fuel station", "rest area", "toll",
  "bridge", "tunnel", "intersection", "roundabout", "speed limit",
  "camera",
  "location", "nearby", "find", "search", "poi", "landmark",
  "address", "coordinates", "geofence"
)
social_env_keywords <- c(
  "police", "enforcement", "ticket", "fine", "violation", "citation",
  "law", "legal", "regulation", "compliance", "court",
  "speed limit", "speed camera", "radar", "speeding", "violation",
  "traffic light", "stop sign", "yield", "right of way",
  "insurance", "claim", "report", "accident report", "incident report",
  "damage", "liability", "coverage", "policy",
  "community", "social", "share", "report", "crowd", "user report",
  "hazard report", "social network", "friends", "family",
  "emergency", "911", "first aid", "roadside assistance", "tow",
  "help", "sos", "rescue", "ambulance", "fire department"
)
count_keyword_matches <- function(text, keywords) {
  sum(sapply(keywords, function(kw) stringr::str_detect(text, stringr::fixed(kw, ignore_case = TRUE))))
}
# ---- 5. Data Cleaning and Feature Engineering ----
df <- df %>%
  dplyr::mutate(
    UnifiedCategory = mapply(get_unified_category, `App store category`, `Google Play Store category`),
    Pre_Event_Clean = ifelse(stringr::str_detect(`Pre-Event / Pre-Driving`, "[a-zA-Z0-9]"), 1, 0),
    Event_Clean = ifelse(stringr::str_detect(`Event / Driving`, "[a-zA-Z0-9]"), 1, 0),
    Post_Event_Clean = ifelse(stringr::str_detect(`Post-Event / Post - Driving`, "[a-zA-Z0-9]"), 1, 0),
    Stage = dplyr::case_when(
      Pre_Event_Clean == 1 & Event_Clean == 0 & Post_Event_Clean == 0 ~ "Pre-event",
      Pre_Event_Clean == 0 & Event_Clean == 1 & Post_Event_Clean == 0 ~ "Event",
      Pre_Event_Clean == 0 & Event_Clean == 0 & Post_Event_Clean == 1 ~ "Post-event",
      Pre_Event_Clean == 1 & Event_Clean == 1 & Post_Event_Clean == 0 ~ "Pre-event & Event",
      Pre_Event_Clean == 1 & Event_Clean == 0 & Post_Event_Clean == 1 ~ "Pre-event & Post-event",
      Pre_Event_Clean == 0 & Event_Clean == 1 & Post_Event_Clean == 1 ~ "Event & Post-event",
      Pre_Event_Clean == 1 & Event_Clean == 1 & Post_Event_Clean == 1 ~ "All stages",
      TRUE ~ "Unspecified"
    )
  ) %>%
  dplyr::rowwise() %>%
  dplyr::mutate(
    combined_text = paste(
      `App name`, UnifiedCategory, Categories, Subcategories,
      `Pre-Event / Pre-Driving`, `Event / Driving`, `Post-Event / Post - Driving`,
      `Other Features`, Comments,
      sep = " ", collapse = " "
    ) %>% tolower(),
    score_human = count_keyword_matches(combined_text, human_keywords),
    score_vehicle = count_keyword_matches(combined_text, vehicle_keywords),
    score_physical_environment = count_keyword_matches(combined_text, physical_env_keywords),
    score_social_environment = count_keyword_matches(combined_text, social_env_keywords),
    Haddon_top_score = max(score_human, score_vehicle, score_physical_environment, score_social_environment),
    Haddon_top_factor = paste(
      c("Human", "Vehicle", "Physical Environment", "Social Environment")[
        c(score_human, score_vehicle, score_physical_environment, score_social_environment) == Haddon_top_score & Haddon_top_score > 0
      ],
      collapse = " & "
    )
  ) %>%
  dplyr::ungroup()

# ---- 6. Secondary Haddon Stage Classification for Unspecified ----
pre_event_keywords <- c(
  "driver training", "road safety education", "defensive driving course", "hazard perception test", "pre-trip inspection",
  "route planning", "trip planning", "risk assessment", "journey management plan", "vehicle maintenance reminder",
  "pre-drive checklist", "pre-drive health check", "fatigue management", "sobriety check", "navigate", "navigator", "construction alerts",
  "license verification", "traffic forecast",  "insurance verification", "weather forecast", "instructions", "destinations", "planner",   "plan",
  "tracking", "journey",  "planning", "route", "navigation", "map", "GPS", "directions", "traffic condition", "road condition", "alert",  "warning",
  "weather", "pre-trip", "education", "training", "awareness", "prevention", "preparation", "safety check", "vehicle inspection", "impairment", "fitness",
  "maintenance", "schedule", "fatigue", "drowsy", "reminder", "notification",  "alcohol", "health check"
)
event_keywords <- c(
  "turn-by-turn navigation", "real-time", "live traffic updates", "lane departure warning", "collision warning", "driving", "real-time",
  "emergency braking", "speed alert", "drowsiness detection", "live", "current", "hands-free communication", "voice command driving", "active", "monitor",
  "emergency call", "automatic crash detection", "incident notification", "blind spot warning", "active safety system", "traffic", "alarm",
  "adaptive cruise control", "autopilot engaged", "parking", "gps", "parking assistance active", "route", "road", "speed", "tracking", "speed",
  "acceleration", "braking", "collision", "detection", "emergency", "crash", "incident", "hazard", "congestion", "traffic jam", "roadwork", "construction",
  "hands-free", "voice", "bluetooth", "automatic", "autonomous", "assistance", "adaptive", "lane", "proximity", "distance"
)
post_event_keywords <- c(
  "accident report", "incident report", "insurance claim", "medical follow-up", "claim status", "damage", "vehicle repair status", "service", "cost", "challan",
  "towing service", "replacement vehicle", "post-crash analysis", "investigation", "long-term monitoring", "post-incident review", "crash", "accident", "injury",
  "destination", "police", "recovery", "towing", "repair", "crash report",  "insurance", "weather", "police", "offences", "tickets", "emergency", "911",
  "SOS", "rescue", "ambulance", "medical", "hospital", "first aid", "injury", "damage", "insurance","claim", "report", "incident report"
)
count_keyword_matches_exact <- function(text, keywords) {
  sum(sapply(keywords, function(k) grepl(paste0("\\b", k, "\\b"), text, ignore.case = TRUE)))
}
classify_app_haddon_stage <- function(app_description) {
  pre_count <- count_keyword_matches_exact(app_description, pre_event_keywords)
  event_count <- count_keyword_matches_exact(app_description, event_keywords)
  post_count <- count_keyword_matches_exact(app_description, post_event_keywords)
  max_count <- max(pre_count, event_count, post_count)
  if (max_count == 0) {
    return(list(stages = "Unspecified", pre_count = pre_count, event_count = event_count, post_count = post_count, max_count = max_count))
  }
  assigned_stages <- c()
  if (pre_count == max_count) assigned_stages <- c(assigned_stages, "Pre-event")
  if (event_count == max_count) assigned_stages <- c(assigned_stages, "Event")
  if (post_count == max_count) assigned_stages <- c(assigned_stages, "Post-event")
  assigned_stages <- unique(assigned_stages)
  return(list(stages = paste(assigned_stages, collapse = " & "), pre_count = pre_count, event_count = event_count, post_count = post_count, max_count = max_count))
}
apply_secondary_classification <- function(df, text_columns) {
  unspecified_indices <- which(df$Stage == "Unspecified")
  if (length(unspecified_indices) == 0) {
    df$Final_Stage <- df$Stage
    return(df)
  }
  df$Secondary_Classification <- NA
  df$Keyword_Pre_Matches <- NA
  df$Keyword_Event_Matches <- NA
  df$Keyword_Post_Matches <- NA
  df$Final_Stage <- df$Stage
  for (i in unspecified_indices) {
    combined_text <- ""
    for (col in text_columns) {
      if (col %in% names(df) && !is.na(df[[col]][i])) {
        combined_text <- paste(combined_text, as.character(df[[col]][i]), sep = " ")
      }
    }
    classification <- classify_app_haddon_stage(combined_text)
    df$Secondary_Classification[i] <- classification$stages
    df$Keyword_Pre_Matches[i] <- classification$pre_count
    df$Keyword_Event_Matches[i] <- classification$event_count
    df$Keyword_Post_Matches[i] <- classification$post_count
    if (classification$stages != "Unclassified") {
      df$Final_Stage[i] <- classification$stages
    }
    df$Final_Stage <- gsub(", ", " & ", df$Final_Stage)
  }
  return(df)
}
text_columns_to_search <- c("combined_text")
df <- apply_secondary_classification(df, text_columns_to_search)

saveRDS(df, "cleaned_appdata_with_haddon.rds")
message("Saved cleaned and classified data as 'cleaned_appdata_with_haddon.rds'")