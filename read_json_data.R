library(rjson)
library(lubridate)
library(ggplot2)
library(dplyr)

json_data <- fromJSON(paste(readLines('./vasa_analytics/vasa_results_2023.json'), collapse=""))
json_df <- lapply(json_data, function(x) {
    lapply(x$splits, function(y) {
        no_nulls <- lapply(y, function(z) {
            if(is.null(z)) return(as.character(NA))
            return(z)
        })
        as_tibble(no_nulls) 
    }) %>% 
        bind_rows() %>%
        mutate(
            name = x$name, 
            start_group = x$start_group,
        ) %>%
        select(name, start_group, everything()) 
}) %>% 
    bind_rows() %>%
    mutate(
        desc = factor(trimws(desc), levels = unique(trimws(desc))),
        time = hms(time, quiet=T), 
        finish_time = rep(hms(time[desc== "Time Total (Brutto)"], quiet = T), each=length(unique(desc))),
        diff = if_else(is.na(diff) | nchar(diff) == 5, hms(paste0("00:",diff), quiet=T), hms(diff)), 
        min_km = ms(min_km, quiet=T), 
        kmh = as.numeric(kmh),
        place = as.numeric(place),
        id = rep(1:length(json_data), each = length(unique(desc))),
        finish_label = cut(
            as.numeric(finish_time),
            breaks = as.numeric(hms(
                strftime(
                    seq(
                        as.POSIXct("2023-03-05 03:30:00"), 
                        as.POSIXct("2023-03-05 12:30:00"), 
                        by = '30 min'
                    ), 
                    format="%H:%M:%S"
                )
            )),
            labels = c(as.character(strftime(
                seq(
                    as.POSIXct("2023-03-05 04:00:00"), 
                    as.POSIXct("2023-03-05 12:30:00"), 
                    by = '30 min'
                ), 
                format="%H:%M:%S"
            )))  
        ),
        time_numeric = as.numeric(as.duration(time))/3600,
        diff_numeric = as.numeric(as.duration(diff))/3600,
    )

fedgar <- c("Rögnvaldsson, Kari","Rögnvaldsson, Sölvi","Saemundsson, Rögnvaldur")
fedgar_dat <- json_df %>% filter(grepl(paste0(fedgar,collapse="|"), name))

json_df %>% 
    filter(!is.na(finish_label)) %>%
    group_by(finish_label, desc) %>%
    summarise(average_time = mean(time_numeric, na.rm=T)) %>%
    ungroup() %>%
    ggplot() + 
    geom_line(aes(x=desc, y=average_time, group=finish_label, col=finish_label), alpha=0.5) +
    geom_line(data=fedgar_dat, aes(x=desc, y=time_numeric, group=name), alpha=1)

json_df %>% 
    filter(!is.na(finish_label)) %>%
    group_by(finish_label, desc) %>%
    summarise(average_speed = mean(kmh, na.rm=T)) %>%
    ungroup() %>%
    ggplot() + 
    geom_line(aes(x=desc, y=average_speed, group=finish_label, col=finish_label), alpha=0.5) +
    geom_line(data=fedgar_dat, aes(x=desc, y=kmh, group=name), alpha=1)

json_df %>% 
    filter(!is.na(finish_label)) %>%
    group_by(finish_label, desc) %>%
    summarise(average_diff = mean(diff_numeric, na.rm=T)) %>%
    ungroup() %>%
    ggplot() + 
    geom_line(aes(x=desc, y=average_diff, group=finish_label, col=finish_label), alpha=0.5) +
    geom_line(data=fedgar_dat, aes(x=desc, y=diff_numeric, group=name), alpha=1)


json_df %>% 
    filter(!is.na(finish_label)) %>%
    group_by(finish_label, desc) %>%
    summarise(average_place = mean(place, na.rm=T)) %>%
    ungroup() %>%
    ggplot() + 
    geom_line(aes(x=desc, y=average_place, group=finish_label, col=finish_label), alpha=0.5) +
    geom_line(data=fedgar_dat, aes(x=desc, y=place, group=name), alpha=1)

