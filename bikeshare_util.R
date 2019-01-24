
# https://cran.r-project.org/web/packages/dplyr/vignettes/programming.html

# quote() is a base function and quotes its input and returns it as an expression
# Using " also quotes its input but the result is a string
# quo() is a "quosure" and is a dplyr function that does the same as quote() but also includes
#   the environment

# Put grouping field last so that we can extend for multiple grouping fields
# Do I want to be able to call this dplyr style (unquoted field names) or just use field name strings?
#   Let's start by designing to be used dplyr style

counts_by_date_group <- function(df, start, end, date_var, group_var){
  
  # Use enquo to enable dplyr style calling
  date_var <- enquo(date_var)
  group_var <- enquo(group_var)
  
  # Need to use quo_name() to convert group_var quosure to string to use as new column name
  group_var_name <- quo_name(group_var)
  date_var_name <- quo_name(date_var)
  
  dates <- seq(start, end, by='days')
  
  # The following results in a one column tibble. Yes, even with as.vector().
  # grp <- as.vector(unique(df[, grpfld]))
  # To get as actual vector, need to do this using dplyr::pull
  grp <- df %>% distinct(!!group_var) %>% pull()
  # Or this using unlist
  # grp <- unlist(unique(df[, group_var]), use.names = FALSE)
  
  # https://tidyr.tidyverse.org/reference/expand.html
  # Use expand.grid() to create the fully seeded dataframe
  date_grp <- expand.grid(grp, dates)
  names(date_grp) <- c(group_var_name, date_var_name)
  
  # Get sorted by station and date
  date_grp <- date_grp %>% 
    arrange(!!group_var, !!date_var)
  
  # Count trips out
  counts_group_date <- trip %>% 
    filter(between(!!date_var, startdate, enddate)) %>% 
    group_by(!!group_var, !!date_var) %>% 
    count()
  
  # Join the fully seeded dataframe to the count dataframe 
  counts_group_date <- date_grp %>% 
    left_join(counts_group_date, by = c(group_var_name, date_var_name))
  
  # Replace the NAs with 0's.
  counts_group_date$n <- counts_group_date$n %>% replace_na(0)
  
  counts_group_date
  
}

# Extend to handle multiple grouping fields
counts_by_date_groups <- function(df, start, end, date_var, ...){
  
  # Use enquo to enable dplyr style calling
  date_var <- enquo(date_var)
  group_var <- group_var <- quos(...)

  
  # Need to use quo_name() to convert group_var quosure to string to use as new column name
  group_var_name <- quo_name(group_var)
  date_var_name <- quo_name(date_var)
  
  dates <- seq(start, end, by='days')
  
  # Find distinct combinations of the grouping variables
  grps <- df %>% distinct(!!group_var) 
  
  # Append the dates sequenc
  
  # Use expand.grid() to create the fully seeded dataframe
  date_grp <- expand.grid(grp, dates)
  names(date_grp) <- c(group_var_name, date_var_name)
  
  # Get sorted by station and date
  date_grp <- date_grp %>% 
    arrange(!!group_var, !!date_var)
  
  # Count trips out
  counts_group_date <- trip %>% 
    filter(between(!!date_var, startdate, enddate)) %>% 
    group_by(!!group_var, !!date_var) %>% 
    count()
  
  # Join the fully seeded dataframe to the count dataframe 
  counts_group_date <- date_grp %>% 
    left_join(counts_group_date, by = c(group_var_name, date_var_name))
  
  # Replace the NAs with 0's.
  counts_group_date$n <- counts_group_date$n %>% replace_na(0)
  
  counts_group_date
  
}