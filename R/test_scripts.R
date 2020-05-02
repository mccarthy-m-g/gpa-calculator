test_df <- read.csv("Data/transcript.csv")

test_df <- test_df %>%
  group_by(Course)  %>%
  summarise(sum(Credits))

test_df <- test_df %>%
  mutate("grade_point_values" = case_when(
    Grade == "A+" ~ gpa_4.0[1],
    Grade == "A"  ~ gpa_4.0[2],
    Grade == "A-" ~ gpa_4.0[3],
    Grade == "AB" ~ gpa_4.0[4],
    Grade == "B+" ~ gpa_4.0[5],
    Grade == "B"  ~ gpa_4.0[6],
    Grade == "B-" ~ gpa_4.0[7],
    TRUE ~ as.numeric(as.factor("grade_point_values")))
  ) %>%
  # calculate grade points
  mutate("grade_points" = (Credits * grade_point_values)) %>%
  mutate("Date" = as.Date(as.factor(semester_to_date(Term, Year)))) %>%
  mutate("Semester" = paste(Term, Year, sep = " ")) %>%
  group_by(Date, Semester)
  
test_credits <- test_df %>% 
  summarise(sum(Credits))

test_points <- test_df %>%
  summarise(sum(grade_points))

test_join <- full_join(test_credits, test_points, by = c("Date", "Semester"))

test_join <- test_join %>%
  select(Date, Semester, "grade_points" = `sum(grade_points)`, "Credits" = `sum(Credits)`)

test_join <- test_join %>%
  mutate("GPA" = grade_points/Credits)

test_join <- test_join %>%
  ungroup() %>%
  mutate(cumcredits = cumsum(Credits)) %>%
  mutate(cumpoints = cumsum(grade_points)) %>%
  mutate(cumgpa = cumpoints/cumcredits)

  
  
