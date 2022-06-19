library(shiny)
library(rStrava)
library(dplyr)
library(ggplot2)
source('account_info.R')
source('utils.R')
ui <- fluidPage(
        # Application title
        titlePanel("Vasafeðgar"),
        fluidRow(
            column(3,
               wellPanel(
                   h4("Stillingar"),
                   checkboxGroupInput(inputId='athlete',label='Veldu íþróttamenn',choices = athletes,selected = athletes[1]),
                   actionButton(inputId = "update",label = "Uppfæra")
               ),
               wellPanel(
                   h4("Bæta við íþróttamanni"),
                   textInput(inputId='name',label='Nafn'),
                   textInput(inputId='athlete_id',label='Athlete ID'),
                   actionButton(inputId = "authenticate",label = "Auðkenna")
               )

            ),
            column(9,
                plotOutput("active_minutes")
            )
        )
    )

server <- function(input, output) {
    observeEvent(input$authenticate,{
        stoken <- httr::config(token = strava_oauth(app_name, 
                                                    app_client_id, 
                                                    app_secret, 
                                                    app_scope="activity:read_all",
                                                    cache=T))
        stoken_binary = paste(readBin('.httr-oauth',what = "raw",n = 1e4),collapse='')
        dat <- c('name'=input$name,
                 'athlete_id'=input$athlete_id,
                 'auth'=stoken_binary)
        save_data('User',dat)
    })
    
    observeEvent(input$update,{
        output$active_minutes <- renderPlot({
            users <- load_data('User')
            update_activities(users)
            activities <- load_data('Activity')
            if(nrow(activities)==0){
                return(NULL)
            }
            cols <- c('#BC3C29FF','#0072B5FF','#E18727FF','#20854EFF')
            activities %>%
            mutate(cum_time=cumsum(time)/(60*60),
                   date=as.Date(date)) %>%
            ggplot(aes(date,cum_time,fill=factor(athlete_id),col=factor(athlete_id))) +
            geom_line(size=2) +
            geom_point(size=4) +
            scale_color_manual(values=head(cols,nrow(users)),labels=users$name,name='Íþróttamaður') +
            scale_fill_manual(values=head(cols,nrow(users)),labels=users$name,name='Íþróttamaður') +
            xlab('Dagsetning') +
            ylab('Uppsafnaður æfingatími') +
            theme_classic()+
            theme(
                plot.title=element_text(size=18,face="bold"),
                axis.title.x=element_text(size=18),
                axis.title.y=element_text(size=18),
                axis.text.x=element_text(size=18),
                axis.text.y=element_text(size=18),
                legend.title=element_text(size=18),
                legend.text = element_text(size=18),
                plot.margin=unit(c(0,1,0,0),'cm')
            )
        })
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
