
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
  counts_group_date <- df %>% 
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
counts_by_date_groups <- function(df, start, end, date_var, ..., dow=1, daytype=TRUE){
  
  # Use enquo to enable dplyr style calling
  date_var <- enquo(date_var)
  group_vars <- quos(...)
  num_group_vars <- length(group_vars)

  # Need to use quo_name() to convert group_var quosure to string to 
  # use as new column name
  group_var_names <- lapply(group_vars, quo_name)
  date_var_name <- quo_name(date_var)
  all_names <- c(group_var_names, date_var_name)
  
  # Create vector of dates over specified range
  dates <- seq(start, end, by='days')

  # There may be some clever lapply approach to this, but I'm just going to
  # use a loop to construct a list of vectors containing the unique values
  # for each of the grouping fields (not including the date)
  group_vals_unique <- list()
  for (gvn in group_var_names){
    unique_vals <- unique_vals_vector(df, gvn)
    group_vals_unique <- c(group_vals_unique, list(unique_vals))
  }
  group_vals_unique <- c(group_vals_unique, list(dates))
  
  # Use expand.grid() to create the fully seeded dataframe
  date_grps <- expand.grid(group_vals_unique)
  names(date_grps) <- all_names

  # Count events
  counts_group_date <- df %>%
    filter(between(!!date_var, startdate, enddate)) %>%
    group_by(!!!group_vars, !!date_var) %>%
    count()
  
  # Join the fully seeded dataframe to the count dataframe 
  counts_group_date <- date_grps %>%
    left_join(counts_group_date)

  # Get sorted by group vars and date
  counts_group_date <- counts_group_date %>%
    arrange(!!!group_vars, !!date_var)
  
  # Replace the NAs with 0's.
  counts_group_date$n <- counts_group_date$n %>% replace_na(0)
  
  # Add day of week fields 
  if(dow == 1){
    counts_group_date$dow <- wday(counts_group_date[, date_var_name])
  } else if(dow == 2){
    counts_group_date$dow <- wday(counts_group_date[, date_var_name], 
                                  label = TRUE)
  } else if(dow == 3){
    counts_group_date$dow <- wday(counts_group_date[, date_var_name], 
                                  label = TRUE, abbr = FALSE)
  }
  
  if(daytype){
    counts_group_date$daytype <- 
      ifelse(wday(counts_group_date[, date_var_name]) %in% 2:6,
             "weekday",
             "weekend")
  }
 
  counts_group_date
  
}

# Extend to handle multiple grouping fields and datetimes
counts_by_datetime_groups <- function(df, start, end, datetime_var, by, ..., dow=1, daytype=TRUE){
  
  # Use enquo to enable dplyr style calling
  datetime_var <- enquo(datetime_var)
  group_vars <- quos(...)
  num_group_vars <- length(group_vars)
  
  # Need to use quo_name() to convert group_var quosure to string to 
  # use as new column name
  group_var_names <- lapply(group_vars, quo_name)
  datetime_var_name <- quo_name(datetime_var)
  all_names <- c(group_var_names, datetime_var_name)
  
  # Create vector of dates over specified range
  datetimes <- seq(start, end, by=by)
  
  # There may be some clever lapply approach to this, but I'm just going to
  # use a loop to construct a list of vectors containing the unique values
  # for each of the grouping fields (not including the date)
  group_vals_unique <- list()
  for (gvn in group_var_names){
    unique_vals <- unique_vals_vector(df, gvn)
    group_vals_unique <- c(group_vals_unique, list(unique_vals))
  }
  group_vals_unique <- c(group_vals_unique, list(datetimes))
  
  # Use expand.grid() to create the fully seeded dataframe
  datetime_grps <- expand.grid(group_vals_unique)
  names(datetime_grps) <- all_names
  
  # Count events
  counts_groups_datetime <- df %>%
    filter(between(!!datetime_var, startdate, enddate)) %>%
    group_by(!!!group_vars, !!datetime_var) %>%
    count()
  
  # Join the fully seeded dataframe to the count dataframe 
  counts_groups_datetime <- datetime_grps %>%
    left_join(counts_groups_datetime)
  
  # Get sorted by group vars and date
  counts_groups_datetime <- counts_groups_datetime %>%
    arrange(!!!group_vars, !!datetime_var)
  
  # Replace the NAs with 0's.
  counts_groups_datetime$n <- counts_groups_datetime$n %>% replace_na(0)
  
  # Add day of week fields 
  if(dow == 1){
    counts_groups_datetime$dow <- wday(counts_groups_datetime[, datetime_var_name])
  } else if(dow == 2){
    counts_groups_datetime$dow <- wday(counts_groups_datetime[, datetime_var_name], 
                                  label = TRUE)
  } else if(dow == 3){
    counts_groups_datetime$dow <- wday(counts_groups_datetime[, datetime_var_name], 
                                  label = TRUE, abbr = FALSE)
  }
  
  if(daytype){
    counts_groups_datetime$daytype <- 
      ifelse(wday(counts_groups_datetime[, datetime_var_name]) %in% 2:6,
             "weekday",
             "weekend")
  }
  
  counts_groups_datetime
  
}

unique_vals_vector <- function(df, col){
  
  unlist(unique(df[, col]), use.names = FALSE)
  
}

stats_by_datetime_groups <- function(df, suffix = "_n") {
  
}