library(RMariaDB)
library(httr)
library(dplyr)
library(rStrava)

save_data <- function(db,table,data) {
    # Append to table input data
    dbAppendTable(db,table,data,overwrite=T)
}

# update_field <- function(db,table,field,value,pk_field,pk_value){
#     query <- paste0("UPDATE ",table," ",field,"=",value,"  WHERE ",pk_field,"=",pk_value)
#     dbSendQuery(db, query)
#     #dbDisconnect(db)
# }

load_data <- function(db,table) {
    # Connect to the database
    # Construct the fetching query
    query <- sprintf("SELECT * FROM %s", table)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    data
}


update_activities <- function(db,users,curr_activities){
    date_origin <- as.Date('2022-05-01')
    all_strava_activities <- lapply(1:nrow(users),function(i){
                                print(users$name[i])
                                stoken <- httr::config(token = readRDS(paste0('auth/httr-oauth_',athletes_mapping[users$name[i]]))[[1]])
                                activities_list <- get_activity_list(stoken,after=date_origin-1)
                                activity_list_to_table(activities_list,users$athlete_id[i])
                              }) %>% bind_rows()
    curr_activities$date <- as.character(curr_activities$date)
    new_activities <- anti_join(all_strava_activities,curr_activities,by=c('date','athlete_id'))
    all_activities <- bind_rows(curr_activities,all_strava_activities) %>%
                      distinct()
    save_data(db,'Activity',new_activities)
    return(all_activities)
}

activity_list_to_table <- function(activity_list,athlete_id){
    lapply(1:length(activity_list), function(i){
        tibble('date'=gsub('T',' ',gsub('Z$','',activity_list[[i]]$start_date)),
                          'name'=activity_list[[i]]$name,
                          'athlete_id'=athlete_id,
                          'type'=activity_list[[i]]$type,
                          'time'=activity_list[[i]]$moving_time,
                          'distance'=activity_list[[i]]$distance,
                          'avg_heartrate'=if(activity_list[[i]]$has_heartrate) activity_list[[i]]$average_heartrate else as.numeric(NA),
                          'description'='')
    }) %>% bind_rows()  
}
