library(shiny)
library(rStrava)
library(dplyr)
library(ggplot2)
library(shinyjs)
library(jsonlite)
source('account_info.R')
source('utils.R')
endpoint <- 'https://www.strava.com/oauth/authorize?'
url <- paste0(endpoint,'client_id=',app_client_id,'&scope=activity%3Aread_all&redirect_uri=http%3A%2F%2Flocalhost%3A1410%2F&response_type=code&state=8WoYVJbwef')
jscode <- paste0("Shiny.addCustomMessageHandler('mymessage', function(message) {window.location = '",url,"';});")

ui <- fluidPage(
            # Application title
            useShinyjs(),
            titlePanel("Vasafeðgar"),
            fluidRow(
                column(3,
                   wellPanel(
                       h4("Stillingar"),
                       checkboxGroupInput(inputId='athlete',label='Veldu íþróttamenn',choices = athletes,selected = athletes[1]),
                       actionButton(inputId = "update",label = "Uppfæra")
                   ),
                   wellPanel(
                       h4("Handskrá æfingu")
                   )
            
                ),
                column(9,
                    tabsetPanel(
                        tabPanel('Æfingar',
                            textOutput('stoken'),
                            plotOutput("active_minutes")
                        ),
                        tabPanel('Users',
                            dataTableOutput('user')
                        ),
                        tabPanel('Activities',
                                 dataTableOutput('activity')
                        )
                    )
                )
            )
        )

server <- function(input, output, session) {
    get_con <- reactive({
        dbConnect(MariaDB(), dbname = database_name, host = mysql$host,
                  port = mysql$port, user = mysql$user,
                  password = mysql$password,ssl.ca='cert.pem')
    })
    
    get_tables <- reactive({
        list(curr_activities=load_data(get_con(),'Activity'),
             users=load_data(get_con(),'User'))
    })
    
    output$active_minutes <- renderPlot({
        users <- get_tables()$users
        activities <- get_tables()$curr_activities
        # update_activities(users)
        # activities <- load_data('Activity')
        if(nrow(activities)==0){
            return(NULL)
        }
        cols <- c('#BC3C29FF','#0072B5FF','#E18727FF','#20854EFF')
        group_by(activities,athlete_id) %>%
        mutate(cum_time=cumsum(time)/(60*60)) %>%
        ungroup() %>%
        mutate(date=as.Date(date)) %>%
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
        
    output$user <- renderDataTable({
        get_tables()$users
    })

    output$activity <- renderDataTable({
        get_tables()$curr_activities
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
