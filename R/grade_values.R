# create function that sums the total of all observations in a column ---------
sum_col_variable <- function(df, x) {
  x <- enquo(x)

  df %>%
  summarise_at(vars(!!x), ~sum(.)) %>%
    as.numeric(.)
}

### this function may be unecessary now ###
# create function that gives filter options for sum_col_variable --------------
reactive_total <- function(df,
                           x,
                           input_subject,
                           date_min,
                           date_max,
                           input_date_min,
                           input_date_max,
                           group_by_date = FALSE) {
  x <- enquo(x)
  
  if (input_subject != "" & input_date_min != "" & input_date_max != "") {
    # subject and date range filter
    df %>%
      filter(Course == input_subject) %>%
      filter(Date >= date_min & Date <= date_max) %>%
      sum_col_variable(!!x)
  } else if (input_date_min != "" | input_date_max != "") {
    # date range filter
    df %>%
      filter(Date >= date_min & Date <= date_max) %>%
      sum_col_variable(!!x)
  } else if (input_subject != "") {
    # subject filter
    df %>%
      filter(Course == input_subject) %>%
      sum_col_variable(!!x)
  } else {
    # no filter
    df %>%
      {if (group_by_date == TRUE) group_by(Date) else .} %>%
      sum_col_variable(!!x)
  }
}

# create function that extracts a string based on a pattern -------------------
extract_string <- function(input, pattern) {
  as.character(input) %>%
    str_trim() %>%
    str_to_title() %>%
    str_extract(pattern = pattern) %>%
    na.omit()
}

# create function that transforms a semester input (e.g., "Fall 2015") --------
# into a date object 
semester_to_date <- function(term, year) {
  case_when(
    term == "Winter" ~ paste(year, "01", "01", sep = "-"),
    term == "Spring" ~ paste(year, "03", "01", sep = "-"),
    term == "Summer" ~ paste(year, "06", "01", sep = "-"),
    term == "Fall" ~ paste(year, "09", "01", sep = "-"))
}

# create function that calculates gpa -----------------------------------------
calc_gpa <- function(total_grade_points, total_credits) {
  round(as.numeric(total_grade_points / total_credits), digits = 2)
}
  
# create tibble for blank transcript ------------------------------------------
blank_transcript <- tibble("Course" = as.character(""),
                           "Grade" = as.character(""),
                           "Credits" = as.numeric(),
                           "Year" = as.numeric(),
                           "Term" = as.character(""))

# create tibble for example transcript
example_transcript <- tibble("Course"  = c("PSYC", "STAT", "CHEM", "PHIL",
                                           "ARTS"),
                             "Grade"   = c("A+", "B-", "B", "C+", "F"),
                             "Credits" = c(3, 3, 4, 1.5, 3),
                             "Year"    = c(2015, 2015, 2015, 2016, 2016),
                             "Term"    = c("Winter", "Fall", "Fall", "Winter",
                                           "Winter"))

# create vectors to be used in reactive calculations --------------------------
courses <- c("PSYC", "STAT", "CHEM", "PHIL", "ARTS")

season <- c("Fall", "Winter", "Spring", "Summer")

years <- "([0-9][0-9][0-9][0-9])"

grades <- c("A+", "A", "A-", "AB", "B+", "B", "B-", "BC",
            "C+", "C", "C-", "CD", "D+", "D", "D-", "DF", "F")

gpa_4.0 <- c(4.0, 4.0, 3.7, 3.5,
             3.3, 3.0, 2.7, 2.5,
             2.3, 2.0, 1.7, 1.5,
             1.3, 1.0, 0.7, 0.5,
             0)

gpa_4.33 <- c(4.33, 4.00, 3.67, NA_real_,
              3.33, 3.00, 2.67, NA_real_,
              2.33, 2.00, 1.67, NA_real_,
              1.33, 1.00, 0.67, NA_real_,
              0.00)

Custom <- 0

# create styling for the result output text -----------------------------------
result_gpa <- function(input) {
  p(paste(input),
    style = "margin-top: -24px; margin-bottom: 0px; font-size: 96px; font-weight: bold;")
}

italic <- function(input) {
  em(input)
}

developed_by <- function() {
  url <- a("Michael McCarthy", href="https://github.com/mccarthy-m-g")
  p(tagList(("Developed by"), url),
    style = "font-size: 12px; padding-top: 24px;
             margin-bottom: -3px; text-align: right;")
  # https://twitter.com/mccarthy_m_g or maybe use twitter url
}
  
# create styling for plots ----------------------------------------------------
plot_padding_line <- "padding-left: 10px; padding-right: 10px;"
plot_height_line <- "350px"
plot_padding_bar <- "padding-left: 16px; padding-right: 14px;"
plot_height_bar <- "331px"

