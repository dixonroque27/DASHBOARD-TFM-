library(shiny)
library(ggplot2)
library(dplyr)
library(tibble)
library(magrittr)

# Cargar los datos desde un archivo local
stroke_data <- read.csv("stroke (1).csv")  # Asegúrate de que el archivo stroke (1).csv está en el mismo directorio

# Convertir las variables categóricas a factores
stroke_data$heart_disease <- as.factor(stroke_data$heart_disease)
stroke_data$stroke <- as.factor(stroke_data$stroke)
stroke_data$work_type <- as.factor(stroke_data$work_type)
stroke_data$gender <- as.factor(stroke_data$gender)
stroke_data$smoking_status <- as.factor(stroke_data$smoking_status)
stroke_data$hypertension <- as.factor(stroke_data$hypertension)
stroke_data$ever_married <- as.factor(stroke_data$ever_married)
stroke_data$Residence_type <- as.factor(stroke_data$Residence_type)

# Función para crear gráficos de barras con porcentajes
create_bar_plot <- function(variable, var_name) {
  var_stroke_counts <- as.data.frame(table(stroke_data[[variable]], stroke_data$stroke))
  colnames(var_stroke_counts) <- c(var_name, "Stroke_Status", "Count")
  var_stroke_counts <- var_stroke_counts %>%
    group_by(!!sym(var_name)) %>%
    mutate(Proportion = Count / sum(Count)) %>%
    ungroup() %>%
    as_tibble(.name_repair = "unique")
  
  plot <- ggplot(var_stroke_counts, aes(x = .data[[var_name]], y = Proportion, fill = Stroke_Status)) +
    geom_col(position = "dodge", alpha = 0.7) +
    geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)), 
              position = position_dodge(width = 0.9), vjust = -0.25) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = paste("Proportion of Stroke by", var_name), x = var_name, y = "Proportion", fill = "Stroke Status") +
    theme_minimal()
  
  return(plot)
}

# Función específica para la proporción de ACV
create_stroke_status_plot <- function() {
  stroke_counts <- as.data.frame(table(stroke_data$stroke))
  colnames(stroke_counts) <- c("Stroke_Status", "Count")
  stroke_counts <- stroke_counts %>%
    mutate(Proportion = ifelse(Stroke_Status == 0, 0.951, 0.049)) %>%
    as_tibble(.name_repair = "unique")
  
  plot <- ggplot(stroke_counts, aes(x = Stroke_Status, y = Proportion, fill = as.factor(Stroke_Status))) +
    geom_col(position = "dodge", alpha = 0.7) +
    geom_text(aes(label = scales::percent(Proportion, accuracy = 0.1)), 
              position = position_dodge(width = 0.9), vjust = -0.25) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(title = "Proportion of Stroke Status", x = "Stroke Status (0 = No, 1 = Yes)", y = "Proportion", fill = "Stroke Status") +
    theme_minimal()
  
  return(plot)
}

# Crear gráficos
plot_heart_disease <- create_bar_plot("heart_disease", "Heart_Disease")
plot_stroke <- create_stroke_status_plot()
plot_work <- create_bar_plot("work_type", "Work_Type")
plot_gender <- create_bar_plot("gender", "Gender")
plot_smoking <- create_bar_plot("smoking_status", "Smoking_Status")
plot_hypertension <- create_bar_plot("hypertension", "Hypertension")
plot_ever_married <- create_bar_plot("ever_married", "Ever_Married")
plot_residence_type <- create_bar_plot("Residence_type", "Residence_Type")
plot_age <- ggplot(stroke_data, aes(x = age, fill = stroke)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Age by Stroke Occurrence", x = "Age", y = "Density", fill = "Stroke") +
  theme_minimal()
plot_glucose <- ggplot(stroke_data, aes(x = avg_glucose_level, fill = stroke)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of Average Glucose Level by Stroke Occurrence", x = "Average Glucose Level", y = "Density", fill = "Stroke") +
  theme_minimal()
plot_bmi <- ggplot(stroke_data, aes(x = bmi, fill = stroke)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of BMI by Stroke Occurrence", x = "BMI", y = "Density", fill = "Stroke") +
  theme_minimal()

# Crear la aplicación Shiny
ui <- fluidPage(
  titlePanel("Stroke Risk Factors Dashboard"),
  sidebarLayout(
    sidebarPanel(
      h3("Leyenda de Factores de Riesgo"),
      p("1. Enfermedad Cardíaca: Presencia o ausencia de enfermedad cardíaca."),
      p("2. ACV: Indica si el paciente ha tenido un accidente cerebrovascular."),
      p("3. Tipo de Trabajo: Categoría de empleo del paciente."),
      p("4. Género: Género del paciente."),
      p("5. Estado de Tabaquismo: Estado de tabaquismo del paciente."),
      p("6. Hipertensión: Presencia o ausencia de hipertensión."),
      p("7. Edad: Distribución de edades de los pacientes."),
      p("8. Estado Civil: Indica si el paciente ha estado alguna vez casado."),
      p("9. Tipo de Residencia: Tipo de residencia del paciente (Rural o Urbana)."),
      p("10. Nivel Promedio de Glucosa: Nivel promedio de glucosa en sangre."),
      p("11. BMI: Índice de Masa Corporal.")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Heart Disease", plotOutput("plot_heart_disease")),
        tabPanel("Stroke Status", plotOutput("plot_stroke")),
        tabPanel("Work Type", plotOutput("plot_work")),
        tabPanel("Gender", plotOutput("plot_gender")),
        tabPanel("Smoking Status", plotOutput("plot_smoking")),
        tabPanel("Hypertension", plotOutput("plot_hypertension")),
        tabPanel("Ever Married", plotOutput("plot_ever_married")),
        tabPanel("Residence Type", plotOutput("plot_residence_type")),
        tabPanel("Age Distribution", plotOutput("plot_age")),
        tabPanel("Average Glucose Level", plotOutput("plot_glucose")),
        tabPanel("BMI", plotOutput("plot_bmi"))
      )
    )
  )
)

server <- function(input, output) {
  output$plot_heart_disease <- renderPlot({ plot_heart_disease })
  output$plot_stroke <- renderPlot({ plot_stroke })
  output$plot_work <- renderPlot({ plot_work })
  output$plot_gender <- renderPlot({ plot_gender })
  output$plot_smoking <- renderPlot({ plot_smoking })
  output$plot_hypertension <- renderPlot({ plot_hypertension })
  output$plot_ever_married <- renderPlot({ plot_ever_married })
  output$plot_residence_type <- renderPlot({ plot_residence_type })
  output$plot_age <- renderPlot({ plot_age })
  output$plot_glucose <- renderPlot({ plot_glucose })
  output$plot_bmi <- renderPlot({ plot_bmi })
}

shinyApp(ui = ui, server = server)
