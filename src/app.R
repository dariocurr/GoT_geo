# loading required libraries
library(shiny)
library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(scales)

# loading csv data
appearances = read_csv("res/appearances.csv", show_col_types = FALSE)
characters = read_csv("res/characters.csv", show_col_types = FALSE)
episodes = read_csv("res/episodes.csv", show_col_types = FALSE)
scenes = read_csv("res/scenes.csv", show_col_types = FALSE)
deaths = read_csv("res/deaths.csv", show_col_types = FALSE)
populations = read_csv("res/populations.csv", show_col_types = FALSE)

# loading geographical data
locations = st_read("res/geo/Locations.shp", crs = 4326)
lakes = st_read("res/geo/Lakes.shp", crs = 4326)
conts = st_read("res/geo/Continents.shp", crs = 4326)
land = st_read("res/geo/Land.shp", crs = 4326)
wall = st_read("res/geo/Wall.shp", crs = 4326)
islands = st_read("res/geo/Islands.shp", crs = 4326)
kingdoms = st_read("res/geo/Political.shp", crs = 4326)
landscapes = st_read("res/geo/Landscape.shp", crs = 4326)
roads = st_read("res/geo/Roads.shp", crs = 4326)
rivers = st_read("res/geo/Rivers.shp", crs = 4326)
scenes_locations = st_read("res/geo/ScenesLocations.shp", crs = 4326)

# the 10 most important characters
mostImportantCharacters = characters %>%
    left_join(appearances) %>%
    left_join(scenes) %>%
    group_by(name) %>%
    summarize(presence_time = sum(duration)) %>%
    arrange(desc(presence_time)) %>%
    head(10)

# the 10 most important killers
mostImportantKillers = deaths %>%
    filter(Killer %in% characters$name) %>%
    group_by(Killer) %>%
    summarize(killed = n()) %>%
    arrange(desc(killed)) %>%
    head(10)

# time per location for each season for each character
timePerSeason = appearances %>%
    left_join(scenes) %>%
    left_join(episodes) %>%
    filter(name %in% mostImportantCharacters$name) %>%
    group_by(name, location, seasonNum) %>%
    summarise(total_time = sum(duration)) %>%
    left_join(scenes_locations) %>%
    st_as_sf()

# all the displacements performed by the most important characters
movingPerSeason = scenes %>%
    left_join(episodes) %>%
    left_join(appearances) %>%
    filter(name %in% mostImportantCharacters$name) %>%
    left_join(scenes_locations) %>%
    st_as_sf() %>%
    group_by(name, seasonNum) %>%
    summarise(do_union = FALSE) %>%
    sf::st_cast("LINESTRING")

# all the season numbers
seasons = episodes %>%
    distinct(seasonNum)

# deaths per killer per location for each season
deathsPerSeason = deaths %>%
    filter(Killer %in% c(mostImportantCharacters$name[mostImportantCharacters$name != "Sansa Stark"], "Bronn")) %>%
    group_by(Killer, Location, Season) %>%
    summarise(killed = n()) %>%
    left_join(locations, by = c("Location" = "name")) %>%
    st_as_sf()

# deaths per killer per method for each season
deathsPerMethod = deaths %>%
    select(Killer, Season, Method) %>%
    filter(Killer %in% mostImportantKillers$Killer) %>%
    group_by(Killer, Season, Method) %>%
    summarise(killed = n())

# scenes stats regarding time, scenes and characters
scenes_stats = scenes %>%
    left_join(episodes) %>%
    group_by(episodeTitle, seasonNum) %>%
    summarize(
        nb_scenes = n(),
        duration_max = max(duration),
        nbcharacters = sum(nbc)
    )


# define UI for application
ui <- fluidPage(

    # add fontawesome
    tags$head(tags$script(src="https://use.fontawesome.com/releases/v5.15.2/js/all.js")),

    br(),
    fluidRow(
        column(
            width = 6,
            # application logo
            img(
                src = "logo.jpg",
                width = 280,
                height = 140
            ),
        ),
        column(
            width = 6,
            # add link to repo
            includeHTML("www/repo.html"),
            align = "center",
            style = "padding-top: 35px",
        )
    ),
    br(),
    br(),    

    # adding a tabset panel to ui
    tabsetPanel(
        
        # adding a tab
        tabPanel(
            "Presence Time",
            icon = icon("history"),
            
            # panel title
            titlePanel(
                "Discover where the most important characters have been during seasons"
            ),
            br(),
            
            # adding a sidebar layout
            sidebarLayout(
                
                # adding to the sidebar panel a selection for characters and a checkbox group for the seasons
                sidebarPanel(
                    selectInput(
                        "characters1",
                        "Characters: ",
                        mostImportantCharacters$name,
                        multiple = TRUE
                    ),
                    checkboxGroupInput(
                        "seasons1",
                        "Seasons: ",
                        seasons$seasonNum,
                        selected = seasons$seasonNum
                    )
                ),
                
                # show the plot in the main panel
                mainPanel(plotOutput("timePlot"))
                
            )
        ),
        
        # adding a tab
        tabPanel(
            "Deaths",
            icon = icon("skull-crossbones"),
            
            # panel title
            titlePanel(
                "Discover how much and where the most important characters killed during seasons"
            ),
            
            # adding a sidebar layout
            sidebarLayout(
                
                # adding to the sidebar panel a selection for characters and a checkbox group for the seasons
                sidebarPanel(
                    selectInput(
                        "characters2",
                        "Characters: ",
                        c(mostImportantCharacters$name[mostImportantCharacters$name != "Sansa Stark"], "Bronn"),
                        multiple = TRUE
                    ),
                    checkboxGroupInput(
                        "seasons2",
                        "Seasons: ",
                        seasons$seasonNum,
                        selected = seasons$seasonNum
                    )
                ),
                
                # show the plot in the main panel
                mainPanel(plotOutput("deathsPlot"))
                
            )
            
        ),
        
        # adding a tab
        tabPanel(
            "Character trips",
            icon = icon("map-marked-alt"),
            
            # panel title
            titlePanel("Discover how the most important character moved during seasons"),
            
            # adding a sidebar layout
            sidebarLayout(
                
                # adding to the sidebar panel a selection for characters and a checkbox group for the seasons
                sidebarPanel(
                    selectInput(
                        "characters3",
                        "Characters: ",
                        mostImportantCharacters$name,
                        multiple = TRUE
                    ),
                    radioButtons(
                        "seasons3",
                        "Seasons: ",
                        seasons$seasonNum,
                        selected = seasons$seasonNum[1]
                    )
                ),
                
                # show the plot in the main panel
                mainPanel(plotOutput("movingsPlot"))
                
            )
            
        ),
        
        # adding a tab
        tabPanel(
            "Statistics",
            icon = icon("chart-bar"),
            
            # panel title
            titlePanel("Discover some general statistics during the seasons"),
            
            # adding a sidebar layout
            sidebarLayout(
                
                # adding to the sidebar panel a checkbox group for the seasons
                sidebarPanel(
                    checkboxGroupInput(
                        "seasons4",
                        "Seasons: ",
                        seasons$seasonNum,
                        selected = seasons$seasonNum
                    )
                ),
                
                # show the plots in the main panel
                mainPanel(
                    plotOutput("deathsBarPlot"),
                    HTML("<br><br>"),
                    plotOutput("timeBarPlot"),
                    HTML("<br><br>"),
                    plotOutput("scenesPlot"),
                    HTML("<br><br>"),
                    plotOutput("populationsPlot")
                )
            )
        )
        
    )
    
)


# define server logic required to draw plots
server <- function(session, input, output) {
    
    # render the plot regarding characters presence time
    output$timePlot <- renderPlot({
        selected = timePerSeason %>%
            filter(name %in% input$characters1) %>%
            filter(seasonNum %in% input$seasons1) %>%
            group_by(name, location) %>%
            summarize(total_time = sum(total_time))
        selected = st_jitter(selected, factor = 0.01)
        treeColor = "#cadfaa"
        waterColor = "#b8dbff"
        landColor = "#f3f1ed"
        borderColor = "grey"
        ggplot() +
            geom_sf(data = land, fill = landColor, col = borderColor) +
            geom_sf(data = islands, fill = landColor, col = borderColor) +
            geom_sf(data = landscapes %>% filter(type == "forest"), fill = treeColor, col = borderColor) +
            geom_sf(data = rivers, col = waterColor) +
            geom_sf(data = lakes, col = waterColor, fill = waterColor) +
            geom_sf(data = wall, col = "black", size = 1) +
            geom_sf_text(data = locations %>% filter(size > 4, name != 'Tolos'), aes(label = name), size = 4, family = "Palatino", fontface = "italic") +
            geom_sf(data = selected, aes(size = total_time / 60, color = factor(name)), alpha = 0.5) +
            scale_size_continuous(range = c(3, 10)) +
            scale_colour_brewer(palette = "Set1") +
            theme_minimal() +
            coord_sf(expand = 0, ndiscr = 0) +
            theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
            labs(x = "", y = "", color = "Character", size = "Time (min)")
    },  width = 900, height = 600)
    
    # render the plot regarding deaths and killers
    output$deathsPlot <- renderPlot({
        selected = deathsPerSeason %>% 
            filter(Killer %in% input$characters2) %>% 
            filter(Season %in% input$seasons2) %>%
            group_by(Killer, Location) %>%
            summarize(killed = sum(killed))
        selected = st_jitter(selected, factor = 0.01)
        treeColor = "#cadfaa"
        waterColor = "#b8dbff"
        landColor = "#f3f1ed"
        borderColor = "grey"
        ggplot() +
            geom_sf(data = land, fill = landColor, col = borderColor) + geom_sf(data = islands, fill = landColor, col = borderColor) +
            geom_sf(data = landscapes %>% filter(type == "forest"), fill = treeColor, col = borderColor) +
            geom_sf(data = rivers, col = waterColor) +
            geom_sf(data = lakes, col = waterColor, fill = waterColor) +
            geom_sf(data = wall, col = "black", size = 1) +
            geom_sf_text(data = locations %>% filter(size > 4, name != 'Tolos'), aes(label = name), size = 4, family = "Palatino", fontface = "italic") +
            geom_sf(data = selected, aes(size = killed, color = factor(Killer)), alpha = 0.5) +
            scale_size(breaks = c(5, 10, 50, 100, 300, 700, 1000), range = c(0, 30)) +
            scale_colour_brewer(palette = "Set1") +
            theme_minimal() +
            coord_sf(expand = 0, ndiscr = 0) +
            theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
            labs(x = "", y = "", color = "Character", size = "People Killed")
    },  width = 900, height = 600)
    
    # render the plot regarding characters displacements
    output$movingsPlot <- renderPlot({
        selected = movingPerSeason %>% 
            filter(name %in% input$characters3) %>% 
            filter(seasonNum == as.integer(input$seasons3))
        selected = st_jitter(selected, factor = 0.05)
        treeColor = "#cadfaa"
        waterColor = "#b8dbff"
        landColor = "#f3f1ed"
        borderColor = "grey"
        ggplot() +
            geom_sf(data = land, fill = landColor, col = borderColor) + 
            geom_sf(data = islands, fill = landColor, col = borderColor) +
            geom_sf(data = landscapes %>% filter(type == "forest"), fill = treeColor, col = borderColor) +
            geom_sf(data = rivers, col = waterColor) +
            geom_sf(data = lakes, col = waterColor, fill = waterColor) +
            geom_sf(data = wall, col = "black", size = 1) +
            geom_sf_text(data = locations %>% filter(size > 4, name != 'Tolos'), aes(label = name), size = 4, family = "Palatino", fontface = "italic") +
            geom_sf(data = selected, aes(color = factor(name))) +
            scale_size(breaks = c(5, 50, 100, 300, 700, 1000), range = c(0, 30)) +
            scale_colour_brewer(palette = "Set1") +
            theme_minimal() +
            coord_sf(expand = 0, ndiscr = 0) +
            theme(legend.title = element_text(size = 14), legend.text = element_text(size = 12)) +
            labs(x = "", y = "", color = "Character")
    },  width = 900, height = 600)
    
    # render the plot regarding killers
    output$deathsBarPlot <- renderPlot({
        validate(need(!is.null(input$seasons4), "Please select at least one season to visualize this plot!"))
        selected = deathsPerMethod %>% 
            filter(Season %in% input$seasons4)
        ggplot(data = selected) +
            geom_bar(aes(x = Killer, y = killed, fill = Method), stat = 'identity') +
            scale_x_discrete("Killer", guide = guide_axis(n.dodge = 2)) +
            scale_y_continuous("People killed") +
            ggtitle("People killed by Characters") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    
    # render the plot regarding characters presence time
    output$timeBarPlot <- renderPlot({
        validate(need(!is.null(input$seasons4), "Please select at least one season to visualize this plot!"))
        selected = timePerSeason %>% 
            filter(seasonNum %in% input$seasons4)
        ggplot(data = selected) +
            geom_bar(aes(x = name, y = total_time / 60, fill = factor(seasonNum)), stat = "identity", width = .5) +
            theme_bw() +
            scale_x_discrete("Characters") +
            scale_y_continuous("Total Time (min)", breaks = seq(0, 750, by = 120), limits = c(0, 780), expand = c(0, 1)) +
            ggtitle("Total Screen Time per Character") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
            guides(fill = guide_legend(title = "Seasons")) +
            scale_fill_brewer(palette = "Spectral")
    })
    
    # render the plot regarding scenes stats
    output$scenesPlot <- renderPlot({
        validate(need(!is.null(input$seasons4), "Please select at least one season to visualize this plot!"))
        labels = scenes_stats %>% 
            filter(duration_max > 400 | nb_scenes > 100) %>% 
            filter(seasonNum %in% input$seasons4)
        selected = scenes_stats %>% 
            filter(seasonNum %in% input$seasons4)
        ggplot(selected, aes(x = nb_scenes, y = duration_max, col = factor(seasonNum))) +
            geom_point(aes(size = nbcharacters)) +
            geom_text(data = labels, aes(label = episodeTitle), vjust = -0.6) +
            scale_x_continuous("Number of scenes", limits = c(0, 280)) +
            scale_y_continuous("Duration of the longest scene", limits = c(100, 800)) +
            guides(colour = "legend", size = "legend") +
            theme_bw() +
            ggtitle("Episodes Informations") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold")) +
            scale_color_discrete(name = "Seasons") +
            scale_colour_brewer(palette = "Set1") +
            scale_size_continuous(name = "Characters") +
            guides(col = guide_legend(title = "Seasons"))
    })
    
    # render the plot regarding populations
    output$populationsPlot <- renderPlot({
        ggplot(populations) +
            geom_bar(aes(x = name, y = population), stat = "identity", width = .5, fill = "tomato3") +
            scale_x_discrete("Locations") +
            scale_y_continuous("Population", labels = comma, seq(500000, 5500000, 500000)) +
            ggtitle("Number of People in Westeros") +
            theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
            theme(plot.title = element_text(hjust = 0.5, face = "bold"))
    })
    
}


# run the application
shinyApp(ui = ui, server = server)
