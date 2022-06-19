library(RMySQL)
library(httr)

save_data <- function(table,data) {
    # Connect to the database
    db <- dbConnect(MySQL(), dbname = database_name, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    # Construct the update query by looping over the data fields
    query <- sprintf(
        "INSERT INTO %s (%s) VALUES ('%s') AS NEW ON DUPLICATE KEY UPDATE %s",
        table,
        paste(names(data), collapse = ", "),
        paste(data, collapse = "', '"),
        paste(paste0(names(data),' = NEW.',names(data)),collapse=', ')
    )
    # Submit the update query and disconnect
    dbGetQuery(db, query)
    dbDisconnect(db)
}

load_data <- function(table) {
    # Connect to the database
    db <- dbConnect(MySQL(), dbname = database_name, host = options()$mysql$host, 
                    port = options()$mysql$port, user = options()$mysql$user, 
                    password = options()$mysql$password)
    # Construct the fetching query
    query <- sprintf("SELECT * FROM %s", table)
    # Submit the fetch query and disconnect
    data <- dbGetQuery(db, query)
    dbDisconnect(db)
    data
}

get_stoken_from_binary <- function(t){
    t_bin_vec <- unlist(strsplit(t,split=''))
    t_bin_pairs <- paste0(t_bin_vec[c(T,F)],t_bin_vec[c(F,T)])
    f = file('.httr-oauth',"wb")
    writeBin(object=as.raw(as.hexmode(t_bin_pairs)),con=f) 
    close(f)
    stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
    file.remove('.httr-oauth')
    stoken
    
}

update_activities <- function(users){
    stokens <- list()
    for(i in 1:nrow(users)){
        stokens[[i]] <- get_stoken_from_binary(users$auth[i])
        date_origin <- if(is.na(users$last_updated[i])) as.Date('2022-05-01') else as.Date(users$last_updated[i],format='%Y-%m-%d')
        new_activities <- get_activity_list(stokens[[i]],after=date_origin-1)
        save_data('User',data=c('name'=users$name[i],
                                'athlete_id'=users$athlete_id[i],
                                'auth'=users$auth[i],
                                'photo'=users$photo[i],
                                'last_updated'=gsub(' GMT','',.POSIXct(Sys.time(), tz="GMT"))))
        save_activities(new_activities,users$athlete_id[i])
    }
}

save_activities <- function(activity_list,athlete_id){
    for(i in 1:length(activity_list)){
        dat <- c('athlete_id'=athlete_id,
                 'date'=gsub('T',' ',gsub('Z$','',activity_list[[i]]$start_date)),
                 'name'=activity_list[[i]]$name,
                 'type'=activity_list[[i]]$type,
                 'time'=activity_list[[i]]$moving_time,
                 'distance'=activity_list[[i]]$distance,
                 'avg_heartrate'=if(activity_list[[i]]$has_heartrate) activity_list[[i]]$average_heartrate else 'NA',
                 'description'='')
        save_data('Activity',dat)
    }
}
