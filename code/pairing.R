library(pacman)
p_load('readxl','dplyr','caret','stringr','proxy','tidyr','tibble','reshape2','DT')

# 1. Load data
file_path <- "data/womentoring_pairing_sheet.xlsx"
mentors_raw <- read_excel(file_path, sheet = "womentors")
mentees_raw <- read_excel(file_path, sheet = "womentees")

mentors_raw <- mentors_raw %>% mutate(name = paste0(trimws(`First name`), "_", trimws(ID)))
mentees_raw <- mentees_raw %>% mutate(name = paste0(trimws(`First name`), "_", trimws(ID)))

# 2. Filter mentees based on WIOMSA membership
mentees_raw <- mentees_raw %>%
  filter(`Are you a WIOMSA member with a current, valid membership ? If not, please sign up here: https://www.wiomsa.org/membership-options/` != "No")

# 3. Prepare mentors
mentors <- mentors_raw %>%
  transmute(
    name = paste0(trimws(`First name`), "_", trimws(ID)),
    preferred_languages = paste(`Preferred language`, `Other language`, sep=','),
    career_level=`Do you have a preference for the womentee’s level of experience?`,
    skills = `What areas of womentoring are you most interested in? (Select all that apply)`,
    language = paste(`Preferred language`, `Other language`, sep=','),
    geography = paste(`Country of residence`,`Country`,sep=','),
    format = `What is your preferred format for womentoring sessions? (Select all that apply)`,
    frequency = `How frequently would you be available to meet with your womentee?`,
    sector=`Sector`,
    goals=`Goals`,
    professional_experience = `Years of experience`
  )

# 4. Prepare mentees
mentees <- mentees_raw %>%
  transmute(
    name = paste0(trimws(`First name`), "_", trimws(ID)),
    preferred_languages = paste(`Preferred language`, `Other language`, sep=','),
    career_level = `Current Level of Study/Professional Stage`,
    skills = `What are your main goals for joining the mentoring program? (Select all that apply)`,
    language = paste(`Preferred language`, `Other language`, sep=','),
    geography = `Country of residence`,
    format = `What is your preferred format for mentoring sessions? (Select all that apply)`,
    frequency = `How often would you like to meet with your mentor?`,
    sector = `Sector preference`,
    goals = `Goals`,
    challenges = `Challenges`
  )

# Clean text values (same function as before)
clean_text <- function(x) {
  na_idx <- is.na(x)
  x_clean <- tolower(x[!na_idx])
  x_clean <- gsub("[^,[:alnum:]\\s]", "", x_clean)
  x_clean <- gsub("\\s+", " ", x_clean)
  x_clean <- trimws(x_clean)
  x_clean <- sapply(strsplit(x_clean, ","), function(parts) {
    cleaned <- unique(trimws(parts))
    cleaned <- cleaned[cleaned != "" & cleaned != "na"]
    paste(cleaned, collapse = ", ")
  })
  result <- rep(NA_character_, length(x))
  result[!na_idx] <- x_clean
  return(result)
}

mentors <- mentors %>% mutate(across(everything(), ~ clean_text(as.character(.))))
mentors_clean<-mentors
mentees <- mentees %>% mutate(across(everything(), ~ clean_text(as.character(.))))
mentees_clean<-mentees

# One-hot encoding and Jaccard similarity (unchanged)
expand_multivalue <- function(df, colname, prefix = NULL) {
  if (is.null(prefix)) prefix <- colname
  cleaned <- tolower(df[[colname]])
  cleaned <- gsub("\\s*,\\s*", ",", cleaned)
  cleaned <- trimws(cleaned)
  values <- strsplit(cleaned, ",")
  values <- lapply(values, function(x) unique(trimws(x[x != ""])))
  unique_terms <- sort(unique(unlist(values)))
  for (term in unique_terms) {
    safe_name <- paste0(prefix, "_", gsub("\\s+", "_", term))
    df[[safe_name]] <- sapply(values, function(x) term %in% x) * 1
  }
  df[[colname]] <- NULL
  return(df)
}

multi_fields <- c("language", "skills", "format", "sector", "goals", "career_level", "geography", "field", "frequency")
for (field in multi_fields) {
  if (field == "career_level") {
    # Special handling for career_level 'no preference'
    if (field %in% colnames(mentors)) {
      # Create base one-hot encoding
      mentors <- expand_multivalue(mentors, field)
      # Find columns related to career_level
      career_cols <- grep("^career_level_", names(mentors), value = TRUE)
      # Set all to 1 where "no_preference" is present
      if ("career_level_no_preference" %in% names(mentors)) {
        mentors[mentors$career_level_no_preference == 1, career_cols] <- 1
      }
    }
    if (field %in% colnames(mentees)) {
      mentees <- expand_multivalue(mentees, field)
    }
  } else {
    if (field %in% colnames(mentors)) mentors <- expand_multivalue(mentors, field)
    if (field %in% colnames(mentees)) mentees <- expand_multivalue(mentees, field)
  }
}

mentor_bin <- as.data.frame(mentors %>% select(where(is.numeric)))
mentee_bin <- as.data.frame(mentees %>% select(where(is.numeric)))
rownames(mentor_bin) <- mentors$name
rownames(mentee_bin) <- mentees$name

# Define field weights (default = 1)
weights <- rep(1, ncol(mentor_bin))
names(weights) <- colnames(mentor_bin)

# Prioritize language (3x) and career level (2x)
weights[grep("^language_", names(weights))] <- 3
weights[grep("^career_level_", names(weights))] <- 2

# Apply weights
mentor_bin <- sweep(mentor_bin, 2, weights, FUN = "*")
mentee_bin <- sweep(mentee_bin, 2, weights, FUN = "*")



jaccard_matrix <- matrix(0, nrow = nrow(mentee_bin), ncol = nrow(mentor_bin))
rownames(jaccard_matrix) <- rownames(mentee_bin)
colnames(jaccard_matrix) <- rownames(mentor_bin)

for (i in 1:nrow(mentee_bin)) {
  for (j in 1:nrow(mentor_bin)) {
    inter <- sum(as.numeric(mentee_bin[i, ]) & as.numeric(mentor_bin[j, ]))
    union <- sum(as.numeric(mentee_bin[i, ]) | as.numeric(mentor_bin[j, ]))
    jaccard_matrix[i, j] <- ifelse(union == 0, 0, inter / union)
  }
}

sim_df <- melt(jaccard_matrix, varnames = c("mentee", "mentor"), value.name = "score") %>%
  arrange(desc(score))

full_scores <- sim_df %>% arrange(mentor, desc(score))
top3_by_mentor <- full_scores %>% group_by(mentor) %>% slice_max(order_by = score, n = 3)
top3_by_mentor <- top3_by_mentor %>% mutate(
  mentor = trimws(mentor),
  mentee = trimws(mentee)
)

# New summary table: match detail infomatch_details <- top3_by_mentor %>%
match_details <- top3_by_mentor %>%
  left_join(mentors_clean %>% select(
    name,
    mentor_language = preferred_languages,
    mentor_career = career_level,
    professional_experience,
    mentor_country = geography
  ), by = c("mentor" = "name")) %>%
  left_join(mentees_clean %>% select(
    name,
    mentee_language = preferred_languages,
    mentee_career = career_level,
    mentee_country = geography
  ), by = c("mentee" = "name")) %>%
  rowwise() %>%
mutate(
  shared_language = paste(intersect(
    strsplit(mentor_language, ",\\s*")[[1]],
    strsplit(mentee_language, ",\\s*")[[1]]
  ), collapse = ", "),
  shared_skills = paste(intersect(
    strsplit(mentors_clean$skills[mentors_clean$name == mentor], ",\\s*")[[1]],
    strsplit(mentees_clean$skills[mentees_clean$name == mentee], ",\\s*")[[1]]
  ), collapse = ", "),
  shared_format = paste(intersect(
    strsplit(mentors_clean$format[mentors_clean$name == mentor], ",\\s*")[[1]],
    strsplit(mentees_clean$format[mentees_clean$name == mentee], ",\\s*")[[1]]
  ), collapse = ", "),
  shared_country = paste(intersect(
    strsplit(mentor_country, ",\\s*")[[1]],
    strsplit(mentee_country, ",\\s*")[[1]]
  ), collapse = ", "),
  shared_frequency = paste(intersect(
    strsplit(mentors_clean$frequency[mentors_clean$name == mentor], ",\\s*")[[1]],
    strsplit(mentees_clean$frequency[mentees_clean$name == mentee], ",\\s*")[[1]]
  ), collapse = ", "),
  shared_sector = paste(intersect(
    strsplit(mentors_clean$sector[mentors_clean$name == mentor], ",\\s*")[[1]],
    strsplit(mentees_clean$sector[mentees_clean$name == mentee], ",\\s*")[[1]]
  ), collapse = ", "),
  shared_goals = paste(intersect(
    strsplit(mentors_clean$goals[mentors_clean$name == mentor], ",\\s*")[[1]],
    strsplit(mentees_clean$goals[mentees_clean$name == mentee], ",\\s*")[[1]]
  ), collapse = ", "),
  mentee_challenges = mentees_clean$challenges[mentees_clean$name == mentee]
)

match_details$score<-round(match_details$score,2)

match_details_table <- match_details %>%
  select(
    mentor, mentee, score,
    shared_language,
    mentor_career_preference = mentor_career, 
    mentee_career_level = mentee_career,
    shared_skills,
    shared_format,
    shared_frequency,
    shared_sector,
    # shared_goals,
    shared_country,
    prof_years = professional_experience,
    mentee_challenges
  )


# match_details_table<-match_details %>% select(mentor,mentee,score,shared_language,mentor_country,mentee_country,professional_experience)

# Export
write.csv(full_scores, "data/full_womentoring_scoring_matrix.csv", row.names = FALSE)
write.csv(top3_by_mentor, "data/womentoring_top3_per_mentor.csv", row.names = FALSE)
write.csv(match_details_table, "data/womentoring_top3_summary_table.csv", row.names = FALSE)

max(full_scores$score)
