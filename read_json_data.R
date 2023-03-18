library(rjson)

json_data <- fromJSON(paste(readLines('./vasa_analytics/first_ten2.json'), collapse=""))
json_df <- lapply(json_data, function(x) {
    lapply(x$splits, function(y) {
        no_nulls <- lapply(y, function(z) {
            if(is.null(z)) return(as.character(NA))
            return(z)
        })
        as_tibble(no_nulls)
    }) %>% 
        bind_rows() %>%
        mutate(name = x$name, start_group = x$start_group) %>%
        select(name, start_group, everything())
}) %>% bind_rows()
