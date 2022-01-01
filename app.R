#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library("shiny")
library("RCurl")
library("rjson")
library("dplyr")
library("tidyr")
library("ggplot2")
library("patchwork")
library("gapminder")
library("zoo")

breaks <- tibble("breaks" = c(1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 10000000000),
                 "label" = c("1", "10", "100", "1 000", "10 000", "100 000", "1 000 000", "10  000 000", "100 000 000", "1 000 000 000", "10 000 000 000"))

l <- fromJSON(getURI("https://api.covid19api.com/countries"))
countries <- tibble("name" = sapply(l, "[[", "Country"),
                    "slug" = sapply(l, "[[", "Slug"),
                    "iso" = sapply(l, "[[", "ISO2")) %>% arrange(name)

country <- "germany"
generation_time <- 4

get_data_from_api <- function(country){
    types <- c("confirmed", "recovered", "deaths")
    type <- types[1]
    data <- tibble()
    for (type in types) {
        call <- paste("https://api.covid19api.com/dayone/country/",
                      country,
                      "/status/",
                      type,
                      "/live", sep = "")
        t <- fromJSON(getURI(call))
        tmp <- tibble("value" = sapply(t, "[[", "Cases"),
                      "date" = as.POSIXct(sapply(t, "[[", "Date")  , "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
                      "name" = type)
        # to merge provinces
        tmp <- tmp %>% group_by(date) %>% summarise("value" = sum(value)) %>% mutate("name" = type)
        data <- rbind(data, tmp) %>% arrange(date) %>% select(date, name, value)
    }
    
    data <- data %>% distinct(date, name, .keep_all = TRUE)
    wide <- data %>% pivot_wider() 
    
    rec_time <- 14
    wide
    wide$removed <- c(rep(NA, rec_time), wide$confirmed[1:(nrow(wide) - rec_time)])
    wide$recovered <- c(rep(NA, rec_time), wide$confirmed[1:(nrow(wide) - rec_time)] - wide$deaths[1:(nrow(wide) - rec_time)])
    
    wide <- wide %>% 
        mutate("infected" = .data$confirmed - (.data$recovered + .data$deaths),
               "immune" = .data$recovered,
               "daily_cases" = c(NA, diff(.data$confirmed)),
               "daily_recovered" = c(NA, diff(.data$recovered)),
               "daily_deaths" = c(NA, diff(.data$deaths)),
               "Letality total" = .data$deaths / .data$confirmed,
               "Letality finished" = .data$deaths / (.data$recovered + .data$deaths))
    
    data <- wide %>% 
        pivot_longer(cols = -date) %>% 
        filter(!is.na(value) & !is.infinite(value)) %>% 
        filter(value > 0)
    
    # replace todays value with NA if == 0
    filt_today <- data$date == as.POSIXct(Sys.Date())
    filt_confirmed <- data$name == "confirmed"
    max_conf <- data$value[filt_confirmed] %>% max()
    if (sum(filt_today & filt_confirmed) > 0) {
        if (data$value[filt_today & filt_confirmed] < max_conf) {
            data$value[filt_today] <- NA
        }
    }
    data
}
keep_infections = TRUE
immunity <- 2
immunity <- Inf
days2fc = 444

forecast <- function(infected, ifac = .33, sfac = 1, pop = 82000000, let = .4,
                     days2remove = 12, removed = 50000, days2fc = 60,
                     start = as.POSIXct(Sys.Date(), tz = "UTC"),
                     immunity = Inf, keep_infections = TRUE){
    fc <- tibble("date" = start + c(0:days2fc) * 3600 * 24,
                 "infected" = NA,
                 "removed" = NA,
                 "deaths" = NA,
                 "recovered" = NA)
    fc$infected[1] <- infected
    fc$removed[1] <- removed
    fc$deaths[1] <- round(removed * let)
    fc$recovered[1] <- removed - round(removed * let)
    
    if (!is.infinite(immunity)) {
        immun <- floor(rep(fc$recovered[1] / (immunity * 30), (immunity * 30)))
        immun[length(immun)] <- immun[length(immun)] + (fc$recovered[1] - sum(immun))
    }
    
    fc
    
    (1 + ifac)^days2remove
    (1 + (sfac * ifac))^c(1:days2remove)
    
    inf <- round((1 + (sfac * ifac))^c(1:days2remove) / sum((1 + (sfac * ifac))^c(1:days2remove)) * infected)
    i <- 2
    for (i in 2:days2fc) {
        if (keep_infections & !is.infinite(immunity) & sum(inf, na.rm = T) < 10) {
            inf[length(inf)] <- 10
        }
        # if (i == 26) {stop()}
        ddeaths <- round(inf[1] * let)
        fc$deaths[i] <- fc$deaths[i - 1] + ddeaths
        if (!is.infinite(immunity)) {
            immun <- c(immun[2:length(immun)], inf[1] - ddeaths)
            fc$recovered[i] <- sum(immun)
        }else{
            fc$recovered[i] <- fc$recovered[i - 1] + inf[1] - ddeaths
        }
        
        fc$removed[i] <- fc$deaths[i] + fc$recovered[i]
        susceptible <- pop - (fc$removed[i] + sum(inf))
        susceptible_limitation <- max(c(0, susceptible / pop))
        inf <- c(inf[2:days2remove], round(sum(inf) * ifac * sfac * susceptible_limitation))
        fc$infected[i] <- sum(inf)
    }
    fc %>% as.data.frame()
    fc$immune <- fc$recovered
    fc %>% pivot_longer(cols = -date)
}

start_id <- "Germany"
if (FALSE) {
    
    dat <- get_data_from_api(country)
    
    dat$name %>% unique()
    dat %>% filter(name %in% c("infected", "daily_cases")) %>% pivot_wider() %>% 
        ggplot(aes(x = infected, y = daily_cases, col = date)) +
        geom_point()
    dat %>% filter(name %in% c("infected", "daily_cases")) %>%
        pivot_wider() %>% 
        filter(infected > 100) %>% 
        mutate("ifac" = daily_cases / infected) %>% 
        ggplot(aes(x = date, y = ifac)) +
        geom_point() +
        stat_smooth(method = "loess") +
        ggtitle("Infektionsfaktor")
    
    dat %>% filter(name %in% c("infected", "daily_cases")) %>%
        pivot_wider() %>% 
        filter(infected > 100) %>% 
        mutate("ifac" = daily_cases / infected) %>% 
        ggplot(aes(x = date, y = ifac / 0.33)) +
        geom_point() +
        stat_smooth(method = "loess") +
        ggtitle("Maßnahmenfaktor")
    
    dat %>% filter(name %in% c("infected", "daily_cases")) %>%
        pivot_wider() %>% 
        filter(infected > 100) %>% 
        mutate("ifac" = daily_cases / infected) %>% 
        mutate("sfac" = ifac / .33) %>% pull(sfac) %>% tail(5) %>% median(na.rm = T)
    
    # natural infection factor 0.33 reasonable
    # dividing ifac by .33 can estimate social/behaviour factor
    # these factors can be extrapolated for future estimations
    # TODO population
    # TODO infected to removed modelling
    # TODO removed to recovered/deaths
    dat$name %>% unique()
    dat %>%
        filter(name %in% c("infected", "recovered", "deaths", "removed")) %>% 
        filter(date > (Sys.time() - (3600 * 24 * 40))) %>% 
        ggplot(aes(x = date, y = value, col = name)) +
        geom_line()+
        geom_point() +
        scale_y_log10()
    
    removed <- dat %>% filter(name == "removed") %>% arrange(desc(value)) %>% head(1)
    removed$value
    initial <- dat %>% filter(name == "confirmed") %>% filter(value >= removed$value) %>% 
        arrange(value) %>% head(1)
    
    days2remove <- (as.numeric(removed$date) - as.numeric(initial$date)) / (3600 * 24)
    days2remove
    # DECISION forecast period 2 month
    # DECISION letality as mean of both procedures
    infected <- 100000
    ifac = .33
    sfac = 1
    pop = 82000000
    let = .04
    days2remove <- 12
    removed <- 50000
    days2fc = 60
    immunity <- 1
    start = as.POSIXct(Sys.Date(), tz = "UTC")
    
    test <- forecast(infected = 100000, ifac = .33, sfac = 1, pop = 82000000, let = .4,
                     days2remove = 12, removed = 50000, days2fc = 60,
                     start = as.POSIXct(Sys.Date(), tz = "UTC"))
    test %>%
        ggplot(aes(x = date, y = value, col = name)) +
        geom_line() +
        geom_point()
    
    dat %>%
        filter(name %in% c("confirmed", "infected", "recovered", "deaths")) %>% 
        pivot_wider() %>%
        mutate("N" = 82000000,
               "R" = recovered + deaths,
               "I" = infected) %>% 
        mutate("S" = N - confirmed,
               "dS" = c(NA, diff(S)),
               "dR" = c(NA, diff(R)),
               "dt" = c(NA, as.numeric(diff(date)))) %>% 
        select(c(date, N, S, I, dS, dt, dR)) %>% 
        mutate("beta" = -1 * (dS * N) / (dt * S * I),
               "gamma" = dR / (dt * I)) %>% 
        mutate("Reff" = beta / gamma) %>% 
        filter(!is.na(Reff) & !is.infinite(Reff)) %>% 
        ggplot(aes(x = date, y = Reff)) +
        geom_point() +
        xlab("") +
        ylab("R effective") +
        ylim(0, 5) + 
        stat_smooth() +
        ggtitle(paste("R value for", country))
    
}


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("Covid 19 country wise cases and forward projections"),
    h3("Data courtesy: covid19api.com; Source: Johns Hopkins CSSE"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("sel_country", "Select country", countries$name,
                        selected = start_id, multiple = FALSE,
                        selectize = TRUE, width = NULL, size = NULL),
            numericInput("days2fc", "Specify forecast period [days]", 90, min = 5, max = 365 * 3, step = 1,
                         width = NULL),
            sliderInput("startdate", "First date to be shown", as.Date("2020-01-01"), Sys.Date() - 7, as.Date(paste(substr(Sys.Date() - 31, 1,7), 1, sep = "-")), step = 1),
            sliderInput("immunity", "Specify immunity period [month, 0 = infinite]", 0, 36, 0, step = 1),
            checkboxInput("keep_virus_alive", "Keep infections incoming", value = TRUE)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("caseplots"),
            plotOutput("daily_cases"),
            plotOutput("letality"),
            plotOutput("r_value"),
            plotOutput("forecast")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data <- reactive({
        country <- countries %>% filter(name == input$sel_country) %>% pull(slug)
        dat <- get_data_from_api(country) %>% 
            dplyr::filter(!is.na(.data$value))
        dat
    })
    
    output$caseplots <- renderPlot({
        # generate bins based on input$bins from ui.R
        print(input$sel_country)
        dat <- data()
        
        first <- max(c(as.POSIXct(input$startdate)), min(dat$date))
        
        plt_cases <- dat %>% 
            filter(date >= first) %>% 
            filter(name %in% c("confirmed", "deaths", "infected", "recovered")) %>% 
            ggplot(aes(x = date, y = value, col = name)) +
            geom_line(lwd = 1) +
            geom_point() +
            xlab("") +
            ylab("Cases (logarithmic)") +
            ggtitle(input$sel_country) +
            scale_y_log10(breaks = breaks$breaks,
                          labels = breaks$label) +
            theme_light() +
            theme(legend.position = "bottom", legend.title = element_blank())
        
        plt_cases
    })
    
    output$daily_cases <- renderPlot({
        # generate bins based on input$bins from ui.R
        print(input$sel_country)
        dat <- data()
        
        first <- max(c(as.POSIXct(input$startdate)), min(dat$date))
        
        plt_daily <- dat %>% 
            filter(name %in% c("daily_cases", "daily_recovered", "daily_deaths")) %>% 
            pivot_wider() %>% 
            mutate("rollingmean_cases" = rollmean(daily_cases, 7, na.pad = T, na.rm = T, align = "right"),
                   "rollingmean_recovered" = rollmean(daily_recovered, 7, na.pad = T, na.rm = T, align = "right"),
                   "rollingmean_deaths" = rollmean(daily_deaths, 7, na.pad = T, na.rm = T, align = "right")) %>% 
            pivot_longer(cols = -date) %>% 
            mutate("calc" = sapply(strsplit(name, "_"), "[[", 1),
                   "type" = sapply(strsplit(name, "_"), "[[", 2)) %>% 
            select(-name) %>% 
            pivot_wider(names_from = calc) %>% 
            filter(date >= first) %>% 
            ggplot(aes(x = date, y = daily, col = type)) +
            geom_point() +
            geom_line(aes(x = date, y = rollingmean, col = type)) +
            xlab("") +
            ylab("Cases: - = 7d avg.; * = daily") +
            ggtitle("Daily new cases and 7-day averages") +
            theme_light() +
            theme(legend.position = "bottom", legend.title = element_blank())
        
        plt_daily
    })
    
    output$letality <- renderPlot({
        # generate bins based on input$bins from ui.R
        print(input$sel_country)
        dat <- data()
        
        lag <- dat %>%
            filter(name %in% c("confirmed", "deaths")) %>% 
            select(date, name, value) %>% 
            pivot_wider(names_from = name, values_from = value) %>% 
            mutate("date" = date + 3600 * 24 * 7)
        
        let_avg <- dat %>% 
            filter(name %in% c("confirmed", "deaths")) %>% 
            select(date, name, value) %>% 
            pivot_wider(names_from = name, values_from = value) %>% 
            left_join(lag, by = "date") %>% 
            mutate(confirmed = confirmed.x - confirmed.y,
                   deaths = deaths.x - deaths.y) %>% 
            select(date, confirmed, deaths) %>%
            mutate("Letality weekly" = deaths / confirmed) %>% 
            pivot_longer(cols = -date) %>% 
            filter(!is.na(value)) %>% 
            filter(name == "Letality weekly")
        
        dat <- rbind(dat, let_avg)
        
        first <- max(c(as.POSIXct(input$startdate)), min(dat$date))
        
        plt_letality <- dat %>% 
            filter(date >= first) %>% 
            filter(name %in% c("Letality total", "Letality finished", "Letality weekly")) %>% 
            ggplot(aes(x = date, y = 100 * value, col = name)) +
            geom_line(lwd = 1) +
            geom_point() +
            xlab("") +
            ylim(0, 15) +
            ylab("Letality [%]") +
            ggtitle("Letality estimation") +
            theme_light() +
            theme(legend.position = "bottom", legend.title = element_blank())
        
        plt_letality
    })
    
    output$r_value <- renderPlot({
        dat <- data()
        first <- max(c(as.POSIXct(input$startdate)), min(dat$date))
        
        if ("immune" %in% dat$name) {
            tmp <- dat %>%
                filter(date >= first) %>% 
                filter(name %in% c("confirmed", "infected", "immune", "deaths")) %>% 
                pivot_wider() %>%
                mutate("N" = 82000000,
                       "R" = .data$immune + .data$deaths,
                       "I" = .data$infected) %>% 
                mutate("S" = N - confirmed,
                       "dS" = c(NA, diff(S)),
                       "dR" = c(NA, diff(R)),
                       "dt" = c(NA, as.numeric(diff(date)))) %>% 
                select(c(date, N, S, I, dS, dt, dR)) %>% 
                mutate("beta" = -1 * (dS * N) / (dt * S * I),
                       "gamma" = dR / (dt * I)) %>% 
                mutate("Reff" = beta / gamma) 
            
            dtmp <- tmp %>% pivot_longer(cols = -date) %>% 
                filter(name == "Reff")
            dat <- rbind(dat, dtmp) %>% arrange(date)
        }
        
        
        plt_R <- dat %>% 
            filter(name %in% c("daily_cases", "Reff")) %>% 
            pivot_wider() %>% 
            mutate("prev_gen" = daily_cases + c(diff(daily_cases, lag = generation_time), rep(NA, generation_time))) %>% 
            mutate("ReffRKI" = prev_gen / daily_cases) %>% 
            pivot_longer(cols = -date) %>% 
            filter(!is.na(value)) %>% 
            filter(name %in% c("Reff", "ReffRKI")) %>% 
            pivot_wider() %>% 
            mutate("rollmean_Reff" = rollmean(Reff, 7, na.pad = TRUE, na.rm = F, align = "right"),
                   "rollmean_ReffRKI" = rollmean(ReffRKI, 7, na.pad = TRUE, na.rm = F, align = "right")) %>% 
            pivot_longer(cols = -date) %>% 
            filter(!is.na(value)) %>% 
            mutate("type" = substr(name, 1, 4),
                   "R" = sapply(lapply(strsplit(name, "_"), rev), "[[", 1)) %>% 
            select(-name) %>% 
            pivot_wider(names_from = type) %>% 
            filter(date >= first) %>% 
            ggplot(aes(x = date, y = Reff, col = R)) +
            geom_hline(yintercept = 1, col = "red") + 
            geom_point() +
            ylim(0,4) +
            geom_line(aes(x = date, y = roll, col = R)) +
            xlab("") +
            ylab("R: - = 7d avg.; * = daily") +
            ggtitle(paste("R value for", input$sel_country)) +
            theme_light() +
            theme(legend.position = "bottom", legend.title = element_blank())
        
        plt_R
        
    })
    
    output$forecast <- renderPlot({
        dat <- data()
        first <- max(c(as.POSIXct(input$startdate)), min(dat$date))
        yesterday <- dat %>% filter(date == as.POSIXct(Sys.Date() - 1)) %>%
            pivot_wider()
        yesterday
        
        removed <- dat %>% filter(name == "removed") %>% arrange(desc(value)) %>% head(1)
        removed$value
        initial <- dat %>% filter(name == "confirmed") %>% filter(value >= removed$value) %>% 
            arrange(value) %>% head(1)
        
        days2remove <- (as.numeric(removed$date) - as.numeric(initial$date)) / (3600 * 24)
        
        sfac = dat %>% filter(name %in% c("infected", "daily_cases")) %>%
            pivot_wider() %>% 
            filter(infected > 100) %>% 
            mutate("ifac" = daily_cases / infected) %>% 
            mutate("sfac" = ifac / .33) %>% pull(sfac) %>% tail(7) %>% median(na.rm = T)
        sfac <- max(c(0, sfac))
        
        let <- dat %>% filter(name %in% c("Letality total")) %>%
            filter(!is.na(value)) %>% 
            arrange(desc(date)) %>% pull(value) %>% head(2) %>% mean(na.rm = T)
        
        cname <- tolower(input$sel_country)
        
        gapminder_pop <- gapminder %>%
            group_by(country) %>%
            arrange(year) %>%
            summarize("pop" = last(pop))
        
        gapminder_pop$country <- gapminder_pop$country %>% as.character()
        gapminder_pop$country[gapminder_pop$country == "United States"] <- "United States of America"
        
        population <- gapminder_pop %>% 
            filter(tolower(country) == tolower(cname)) %>% pull(pop) %>% head(1)
        
        immunity <- Inf
        if (input$immunity > 0) {
            immunity <- input$immunity
        }
        
        fct <- forecast(infected = yesterday$infected,
                        ifac = .33,
                        sfac = sfac,
                        pop = population,
                        let = let,
                        days2remove = days2remove,
                        removed = yesterday$removed,
                        days2fc = input$days2fc,
                        start = yesterday$date,
                        immunity = immunity,
                        keep_infections = input$keep_virus_alive)
        
        fct2 <- forecast(infected = yesterday$infected,
                         ifac = .33,
                         sfac = 1,
                         pop = population,
                         let = let,
                         days2remove = days2remove,
                         removed = yesterday$removed,
                         days2fc = input$days2fc,
                         start = yesterday$date,
                         immunity = immunity,
                         keep_infections = input$keep_virus_alive)
        tmp1 <- dat
        tmp2 <- fct %>%
            pivot_wider() %>%
            mutate(daily_deaths = c(NA, diff(deaths))) %>%
            pivot_longer(cols = -c(date))
        tmp3 <- fct2 %>%
            pivot_wider() %>%
            mutate(daily_deaths = c(NA, diff(deaths))) %>%
            pivot_longer(cols = -c(date))
        tmp1$source <- "data"
        tmp2$source <- "model current actions"
        tmp3$source <- "model no actions"
        cmbnd <- rbind(tmp1, tmp2, tmp3) %>% filter
        
        plt_fc <- cmbnd %>%
            filter(name %in% c("infected", "daily_deaths")) %>% 
            filter(value > 0) %>% 
            filter(date >= first) %>% 
            ggplot(aes(x = date, y = value, col = name, lty = source, pch = source)) +
            geom_line(lwd = 1) +
            xlab("") +
            ylab("Cases (logarithmic)") +
            scale_y_log10(breaks = breaks$breaks,
                          labels = breaks$label) +
            ggtitle(paste("Forward projection", input$sel_country)) +
            theme_light() +
            theme(legend.position = "bottom", legend.title = element_blank())
        print("plotting forecast")
        plt_fc
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
