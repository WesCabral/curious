library(shiny)
library(tidyverse)
library(lubridate)
library(chron)
library(remoji)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WhatsApp Dashboard v1.0"),

    # Sidebar with a slider input for number of bins 
    fluidRow(
        column(1),
        
        column(4, 
               fileInput(inputId = "file", "Submeta um arquivo de conversa (apenas android)", accept = ".txt",8)),
        column(3,
               plotOutput("distPlot")),
        column(3,
               plotOutput("plot3")),
        
        plotOutput("plot2")

        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    cleandata <- reactive({
        
        file = input$file
        file = read_tsv(file$datapath, skip = 1, col_names = "column")
        
        wrongLines = sort(
            which(
                !str_detect(string = file$column, pattern = "[0-9]{2}/[0-9]{2}/[0-9]{4} [0-9]{2}:[0-9]{2} - ")
            ), 
            decreasing =  T)
        
        for (i in wrongLines){
            file[i-1,1] = paste(file[i-1,1], "\n", file[i,1])
            file[i,1] = NA
        }
        
        file = drop_na(file)
        
        file = separate(file, column, into = c("data", "autor"), sep = " - ", extra = "merge") %>%
            separate(autor, into = c("autor", "mensagem"), sep = ":", extra = "merge", fill = "right") %>%
            mutate(diasemana = str_replace(wday(as.Date(data), label = T), "^\\w{1}", toupper))  
        
        file$data = dmy_hm(file$data)
        
        file = mutate(file, horadec = floor(hour(data) + minute(data)/60))
        
    })
    
    output$distPlot <- renderPlot({
        
        #Gráfico1
        
        drop_na(cleandata()) %>%
            group_by(autor) %>%
            summarise(n = n()) %>%
            arrange(desc(n)) %>%
            top_n(wt = n, 10) %>%
            ggplot(aes(x=reorder(autor,n), y = n, fill = autor)) +
                geom_bar(stat = 'identity') +
                geom_label(aes(label=n), hjust=1.5) +
                labs(title = "Mensagens por usuário", x= NULL, y = NULL) +
                coord_flip() +
                theme_minimal() +
                theme(legend.position = "none", axis.text=element_text(size=12),
                      title=element_text(size=14),axis.ticks=element_blank(), 
                      strip.background = element_rect(colour="white"))
            
    })
    
    output$plot2 <- renderPlot({
        
        #Gráfico2
        
        cleandata() %>%
            count(diasemana, horadec, name = 'count')  %>%
            drop_na() %>%
            complete(diasemana, horadec = seq(min(horadec), max(horadec)), fill = list(count = 0)) %>%
            ggplot(aes(diasemana, horadec, fill=count)) +
            geom_tile() + 
            scale_fill_continuous(name = "Qtde de \nMensagens", type = "viridis") +
            facet_wrap(~factor(diasemana, levels = c("Dom", "Seg", "Ter", "Qua", "Qui", "Sex", "Sáb"), ordered = T), 
                       nrow = 1, scales = "free") +
            scale_y_continuous(minor_breaks = NULL, limits = c(-0.5,24), breaks = seq(-0.5,23.5),
                               labels = c(paste0(seq(00,23),"h"),"23h59min")) + theme_minimal() +
            theme(axis.ticks=element_blank(), strip.background = element_rect(colour="white")) +
            labs(title = "Distribuição das mensagens durante a semana", x="", y="Horário")
        
        
    })
    
    output$plot3 <- renderPlot({
        
        #Gráfico3
        
        emjname = tibble(Emoji = emoji(list_emoji()), Nome = list_emoji())
        emj = emoji(list_emoji())
        emjcount = tibble()
        
        for (i in 1:length(emj)){
            emjcount[i,1] = emj[i]
            emjcount[i,2] = length(unlist(str_extract_all(cleandata()$mensagem, emj[i])))
        }
        
        names(emjcount) = c("Emoji", "Contagem")
        
        emjcount %>%
            filter(Contagem != 0) %>%
            group_by(Emoji) %>%
            summarise(Contagem = sum(Contagem)) %>%
            slice_max(Contagem, n = 5) %>%
            left_join(emjname, by = "Emoji") %>%
            group_by(Emoji, Contagem) %>%
            summarise(Nome = first(Nome)) %>%
            ggplot(aes(x=reorder(Emoji,Contagem), y=Contagem)) +
            geom_bar(aes(fill=Emoji),stat="identity") +
            geom_label(aes(label=Contagem, fill=Emoji), hjust=1.5) +
            labs(title = "Top 5 emojis mais usados", x= NULL, y = NULL) +
            coord_flip() +
            theme_minimal() +
            theme(legend.position = "none", axis.text=element_text(size=12),
                  title=element_text(size=14), axis.text.y = element_text(size=25))
        
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
