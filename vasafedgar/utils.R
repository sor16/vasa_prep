library(RMariaDB)
library(httr)
library(dplyr)
library(tidyr)
library(rStrava)

roll_avg <- function(x,k){
    roll_avg_vec <- vector('numeric',length(x))
    for(i in 1:length(x)){
        lower_idx <- max(i-k+1,1)
        roll_avg_vec[i] <- mean(x[lower_idx:i])
    }
    return(roll_avg_vec)
}

save_data <- function(db,table,data) {
    # Append to table input data
    dbAppendTable(db,table,data,overwrite=T)
}
# 
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


update_activities <- function(db,users){
    date_origin <- as.Date('2022-05-01')
    curr_activities <- load_data(db,'Activity')
    new_activities <- lapply(1:nrow(users),function(i){
                                stoken <- httr::config(token = readRDS(paste0('auth/httr-oauth_',athletes_mapping[users$name[i]]))[[1]])
                                if(is.na(users$latest_activity[i])){
                                    date_start <- date_origin
                                }else{
                                    date_start <- as.Date(users$latest_activity[i])
                                }
                                activities_list <- get_activity_list(stoken,after=date_start-1)
                                activity_list_to_table(activities_list,users$athlete_id[i])
                              }) %>% bind_rows()
    users_updated <- group_by(new_activities,athlete_id) %>%
                     summarise(latest_activity=max(date)) %>%
                     left_join(select(users,-latest_activity),.,by='athlete_id')
    curr_activities$date <- as.character(curr_activities$date)
    new_activities <- anti_join(new_activities,curr_activities,by=c('date','athlete_id'))
    if(nrow(new_activities)!=0){
        dbAppendTable(db,'Activity',new_activities,overwrite=T)
    }
    if(!isTRUE(all_equal(users,users_updated))){
        dbWriteTable(db,'User',users_updated,overwrite=T)
    }
    all_activities <- bind_rows(curr_activities,new_activities) %>%
                      as_tibble() %>%
                      arrange(athlete_id,date)
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
