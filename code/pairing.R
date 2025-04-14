library(pacman)
p_load('readxl','dplyr','FNN','caret','stringr','proxy','tidyr','tibble')

# 1. Load data
file_path <- "data/womentoring_pairing_sheet.xlsx"
mentors_raw <- read_excel(file_path, sheet = "womentors_anon")
mentees_raw <- read_excel(file_path, sheet = "womentees_anon")

# 2. Filter mentees based on WIOMSA membership
mentees_raw <- mentees_raw %>%
  filter(`Are you a WIOMSA member with a current, valid membership ? If not, please sign up here: https://www.wiomsa.org/membership-options/` != "No")

# 3. Prepare mentors
mentors <- mentors_raw %>%
  transmute(
    name = paste0(`First name`, "_", ID),
    professional_field = `Professional field of interest/experience`,
    career_stage = `Role/Position/Job title`,
    skills = `What areas of womentoring are you most interested in? (Select all that apply)`,
    language = paste(`Preferred language`, `Are you comfortable mentoring in languages other than your preferred language? (Specify which ones)`),
    geography = `Where in the Western Indian Ocean do you work?`,
    format = `What is your preferred format for womentoring sessions? (Select all that apply)`,
    frequency = `How frequently would you be available to meet with your womentee?`,
    sector = `Organization/Institution`
  )

# 4. Prepare mentees
mentees <- mentees_raw %>%
  transmute(
    name = paste0(`First name`, "_", ID),
    professional_field = `Field of Interest within Marine Science`,
    career_stage = `Current Level of Study/Professional Stage`,
    skills = `What are your main goals for joining the mentoring program? (Select all that apply)`,
    language = paste(`Preferred language`, `Are you comfortable being mentored in languages other than your preferred language? (Specify which ones)`),
    geography = `Country of residence`,
    format = `What is your preferred format for mentoring sessions? (Select all that apply)`,
    frequency = `How often would you like to meet with your mentor?`,
    sector = `Do you have a preference for the mentor’s background? (e.g., academia, industry, government, NGO)`
  )

# 5. Clean text values
clean_text <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:]]", "", x)
  x <- gsub("\\s+", " ", x)
  trimws(x)
}

mentors <- mentors %>% mutate(across(everything(), ~ clean_text(as.character(.))))
mentees <- mentees %>% mutate(across(everything(), ~ clean_text(as.character(.))))

# 6. Combine for encoding
combined <- bind_rows(
  mentors %>% mutate(role = "mentor"),
  mentees %>% mutate(role = "mentee")
)

# 7. One-hot encode all matching criteria
matching_vars <- c("professional_field", "career_stage", "skills", "language",
                   "geography", "format", "frequency", "sector")
dummies <- dummyVars(~ ., data = combined[, matching_vars], sep = "_")
encoded <- predict(dummies, newdata = combined[, matching_vars])

# 8. Apply weights to prioritize Language (3x), Career Stage (2x)
colnames_encoded <- colnames(encoded)
weights <- rep(1, length(colnames_encoded))
weights[grep("^language_", colnames_encoded)] <- 3
weights[grep("^career_stage_", colnames_encoded)] <- 2
weighted_encoded <- sweep(encoded, 2, weights, FUN = "*")

# 9. Separate mentor and mentee feature sets
mentor_features <- weighted_encoded[combined$role == "mentor", ]
mentee_features <- weighted_encoded[combined$role == "mentee", ]

# 10. Compute all pairwise distances
dist_matrix <- proxy::dist(mentee_features, mentor_features, method = "euclidean")
rownames(dist_matrix) <- mentees$name
colnames(dist_matrix) <- mentors$name

# 11. Convert to long format (this works!)
dist_df <- as.data.frame(as.table(dist_matrix)) %>%
  rename(mentee = Var1, mentor = Var2, distance = Freq) %>%
  mutate(score = round(1 / (1 + distance), 3)) %>%
  arrange(distance)

# 12. assignment
# FULL SCORING MATRIX: All mentor–mentee combinations
full_scores <- dist_df %>%
  arrange(mentor, distance) %>%
  select(mentor, mentee, score, distance)

# OPTIONAL: Get top 3 mentees per mentor
top3_by_mentor <- full_scores %>%
  group_by(mentor) %>%
  slice_min(order_by = distance, n = 3)

# View or export
print(top3_by_mentor)
write.csv(full_scores, "data/full_womentoring_scoring_matrix.csv", row.names = FALSE)
write.csv(top3_by_mentor, "data/womentoring_top3_per_mentor.csv", row.names = FALSE)



# 13. Final pairings
final_pairings <- assigned %>%
  arrange(mentor, distance) %>%
  select(mentee, mentor, score)

print(final_pairings)

max(final_pairings$score)

# Optionally save:
# write.csv(final_pairings, "womentoring_pairings_top3.csv", row.names = FALSE)
