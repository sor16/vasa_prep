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

save_activity_prop <- function(db,id,activity_prop_athlete,activity_prop){
    activity_prop_athlete_old <- filter(activity_prop,athlete_id==id)
    if(!isTRUE(all_equal(activity_prop_athlete_old,activity_prop_athlete))){
        if(nrow(activity_prop_athlete_old)==0){
            activity_prop <- bind_rows(activity_prop,activity_prop_athlete)
        }
        activity_prop$p[activity_prop$athlete_id==id] <- activity_prop_athlete$p
        activity_prop <- select(activity_prop,activity_type,athlete_id,p)
        dbWriteTable(db,'ActivityProp',activity_prop,overwrite=T)
    }
    return(activity_prop)
}

save_target_dist <- function(db,users,athlete_id,target_dist){
    target_dist_old <- users$target_dist[users$athlete_id==athlete_id]
    if(is.na(target_dist_old) | target_dist_old!=target_dist){
        users$target_dist[users$athlete_id==athlete_id] <- target_dist
        dbWriteTable(db,'User',users,overwrite=T)
    }
    return(users)
}

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

### Summary plots

plot_summary_var <- function(summary_dat,var,title){
    summary_dat_time <- filter(summary_dat,name==var)
    colors <- c("Hlaup"="#BC3C29FF","Hjól"="#0072B5FF", "Gönguskíði"="#E18727FF","Ganga"="#20854EFF","Sund"="#7876B1FF")
    colors <- colors[summary_dat_time$name_ice]
    
    p <- ggplot(data=summary_dat_time) +
        geom_col(aes(x=name_ice,y=p,fill=name_ice)) +
        geom_text(aes(x=name_ice,y=p,label=paste0(format(round(100*p,1),nsmall=1),'%')),nudge_y=0.06,nudge_x=0.2) +
        geom_text(aes(x=name_ice,y=p,label=paste0(format(round(value,1),nsmall=1),ifelse(var=='tot_time','h','km'))),nudge_y=ifelse(var=='tot_time',0.06,0.08),nudge_x=-0.2) +
        coord_flip() +
        scale_y_continuous(limits=c(0,1)) +
        scale_fill_manual(values=colors,breaks=names(colors)) +
        ggtitle(title) +
        theme_classic() +
        theme(axis.line = element_blank(),
              axis.ticks = element_blank(),
              axis.text = element_blank(),
              axis.title = element_blank(),
              legend.title = element_blank())
    
    if(var=='tot_eff_distance'){
        p <- p + theme(legend.position='none')
    }
    return(p)
}

plot_summary_athlete <- function(activities_mapped,athlete_name){
    summary_dat <- filter(activities_mapped,name==athlete_name) %>% 
                    group_by(name_ice) %>% 
                    summarise(tot_time=sum(time)/(60*60),
                              tot_distance=sum(distance)/1e3,
                              tot_eff_distance=sum(weight*distance)/1e3) %>%
                    pivot_longer(cols=c('tot_time','tot_eff_distance')) %>%
                    group_by(name) %>%
                    mutate(p=value/sum(value))
    plot_summary_var(summary_dat,var='tot_time',title='Tímalengd æfinga') /
    plot_summary_var(summary_dat,var='tot_eff_distance',title='Umreiknuð vegalengd')
}
