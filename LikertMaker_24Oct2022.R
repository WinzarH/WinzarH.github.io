##
## Likert Maker
##
## an app for generating Likert-scale data
## with exact mean and standard deviation
## for synthesising published data for better
## visualisation and analysis
##
## author: Hume Winzar
## affiliation: Macquarie Business School,
##              Macquarie University, Sydney Australia


####
#### Define UI
####

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "flatly"),
  h2("Likert Maker"),
  p(strong("Hume Winzar"), em("Macquarie University")),
  p(strong("This version: "), em("23 October 2022")),
  h4("Generate Likert scale data with exact Mean & StDev"),
  p("Synthesise published data for better visualisation and analysis"),

  ###
  ### begin sidebar layout
  sidebarLayout(
    ###
    ### sidebar
    sidebarPanel(
      h2("Input scale features"),
      h4("Scale properties"),
      sliderInput(
        "item_range",
        label = "Item scale range", value = 7, min = 1, max = 11, step = 1
      ),

      # Number of items in the scale
      sliderInput("items_n",
        label = "Number of items in scale",
        value = 5,
        min = 1, max = 20, step = 1
      ),
      tags$table(
        tags$tr(
          tags$td(numericInput("sample_n", "Sample size", 32,
            min = 10, max = 300, step = 1, width = "13ch"
          )),
          tags$td(HTML("<strong>Note: </strong> larger sample & skewed distribution take time"))
        )
      ),
      h4("Desired scale moments"),
      tags$table(
        tags$tr(
          tags$td(numericInput("target_mean_scale", "Target mean", 4.25,
            min = 1, width = "13ch"
          )),
          tags$td(numericInput("target_sd_scale", "Target st dev", 1.25,
            min = 0, width = "13ch"
          ))
        )
      ),
      hr(),
      actionButton("goButton", "Run simulation", icon("paper-plane"),
        style = "color: #fff; background-color: #337ab7; border-color: #2e6da4"
      ),
      # p("Start simulation"),
      hr(),
      downloadButton("downloadData", label = "Save data as CSV"),
      # hr(),
      downloadButton("downloadPlot", label = "Save histogram as PNG")
    ),
    ### end sidebar

    ###
    ### begin main panel
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Simulation",
          shinybusy::add_busy_bar(
            color = "#69b3a2",
            centered = TRUE,
            height = "10px"
          ),
          plotOutput("distPlot", height = 100, width = "90%"),
          hr(),
          plotOutput("notionalPlot", height = 400, width = "90%"),
          hr(),
          # ),
          # tabPanel(
          #   "Output data",

          reactable::reactableOutput("mystuff", width = "75%"),
          hr()
        ),
        tabPanel(
          "Output histogram",
          plotOutput("resultPlot", height = 500, width = "90%"),
          hr()
        ),
        tabPanel(
          "Background/ explanation",
          h1("Reconstructing Likert-scale data with only reported
             first and second moments"),
          p("Hume Winzar,", em(" Macquarie University")),
          p("October 2022"),
          h2("Abstract:"),
          p(
            "Most simulations of Likert-scale data are created to
            test analysis methods before finalising a questionnaire.
            Here, however, we want to reproduce published results where
            only the first two moments (mean and variance) are reported.
            Many publications report only means and standard deviations
            of rating scales, often only the means.
            The boundaries of a scale (1-5, 1-7, etc.) will often
            produce skewed data, so such simple statistics can be misleading.",
            em("LikertMaker"), 
            "is a novel method to synthesise Likert-scale data
            using only mean and standard deviation so that they can be
            properly visualised and interpreted.
            The algorithm is surprisingly accurate, producing synthetic data
            consistently correlated above 0.98 with known data."
          ),
          p(
            strong("Keywords:"),
            em("Likert Scale, Data Visualisation, Simulation, R, Optimisation")
          ),
          hr(),
          h2("Introduction"),
          p("An often-ignored tenet of data analysis is “always look at your
            data” to best understand the range, distribution, outliers, etc.
            (Anscombe, 1973; Matejka & Fitzmaurice, 2017).
            Unfortunately, in many published reports only the means and
            standard deviations of rating scales are reported, often only
            the means. In this study, we want to reproduce published results
            given only the first two moments, mean and variance.
            (The third and fourth moments are skewness and kurtosis
            respectively.)
            Scale boundaries (1-5, 1-7, etc.) cause most distributions to
            be skewed.
            If the standard deviation of a scale is more than the distance
            of the mean from the scale boundary, then most cases are closer
            to the boundaries than we might expect.
            Thus, simple univariate statistics can be misleading."),
          h2("Background"),
          p("A Likert scale is the mean, or sum, of several ordinal rating
            scales.
            They are bipolar (usually “agree-disagree”) responses to
            propositions that are determined to be moderately-to-highly
            correlated and capturing various facets of a construct.
            A 5-point Likert scale that is constructed with, say, five
            items (questions) will have a summed range of between 5
            (all five items rated ‘1’) and 25 (all five items rated ‘5’)
            with all integers in between, and the mean range will be ‘1’
            to ‘5’ with intervals of 1/5=0.20.
            A 7-point Likert scale constructed from eight
            items will have a summed range between 8 (all rated ‘1’) and
            56 (all rated ‘7’) with all integers in between, and the mean
            range will be ‘1’ to ‘7’ with intervals of 1/8=0.125
            (Likert, 1932; Warmbrod, 2014)."),
          p(
            "Typically, researchers wish to simulate Likert scales to create
            dummy data to check analyses ahead of finalising a questionnaire.
            The usual approach is to resample with a predetermined
            probability distribution (Heinz, 2021).
            Extensions of the same probabilistic distribution idea are
            available for correlated data
            (Grønneberg et al., 2022; Touloumis, 2016)."
          ),
          h2("Method"),
          p(
            "Unlike the task of creating mock data, the challenge here is to
            synthesise data that are already published but where only the mean
            and standard deviation are reported.
            A “probability distribution” approach is inapplicable because we
            cannot guarantee the desired moments.
            We found no examples of this simulation task in the social
            sciences or coding literature.
            We hit upon a workable solution in the Evolutionary algorithm in
            the Excel Solver add-in."
          ),
          p(
            "With two optimisation criteria, mean and standard
            deviation, the goal is to minimise the difference between the
            current mean and the target mean, and between the current sd and
            the target sd.
            We amplify the differences by calculating the
            difference between the squares of current and target moments.
            The objective value to be minimised is simply the sum of the two
            squared differences.
            This works well in Excel Solver but can take many minutes (hours)
            for a sample size in the hundreds."
          ),
          p(
            "A more efficient approach is an evolutionary algorithm in R,
            Python or Julia (and, no doubt, other languages).
            We chose the", strong("DEoptim"), "package for", strong("R"),
            "(Mullen et al., 2011) which produces simulated data with desired
            properties in much less time.
            R code and an R Shiny app are available on the author’s GitHub site
            (Winzar, 2022)."
          ),
          h2("Discussion"),
          p(
            "The optimisation algorithm produces simulated data with the
            desired mean and standard deviation accurate to at least two
            decimal places.
            When this doesn’t occur, then the combination of moments is
            infeasible."
          ),
          h2("References"),
          p(
            "Anscombe, F. J. (1973). Graphs in Statistical Analysis.",
            em("American Statistician"), ". 27 (1): 17–21.",
            a(href = "https://www.tandfonline.com/doi/abs/10.1080/00031305.1973.10478966", "doi:10.1080/00031305.1973.10478966")
          ),
          p(
            "Grønneberg, S., Foldnes, N., & Marcoulides, K. M. (2022).
            covsim: An R Package for Simulating Non-Normal Data for
            Structural Equation Models Using Copulas.",
            em("Journal of Statistical Software"), ", 102(3), 1 - 45.",
            a(
              href = "https://www.jstatsoft.org/article/view/v102i03",
              "doi:10.18637/jss.v102.i03"
            )
          ),
          p(
            "Heinz, A. (2021). Simulating Correlated Likert Scale Data.
            BLOG post Retrieved from ",
            a(
              href = "https://glaswasser.github.io/simulating-correlated-likert-scale-data/",
              "https://glaswasser.github.io/simulating-correlated-likert-scale-data/"
            )
          ),
          p(
            "Likert, R. (1932). A technique for the measurement of attitudes.",
            em("Archives of Psychology"), ", 22 140, 55-55."
          ),
          p(
            "Matejka, J. & Fitzmaurice, G. (2017). Same Stats, Different
            Graphs: Generating Datasets with Varied Appearance and Identical
            Statistics through Simulated Annealing.",
            em("Proceedings of the 2017 CHI Conference on Human Factors in
               Computing Systems"), ": 1290–1294.",
            a(
              href = "https://dl.acm.org/doi/10.1145/3025453.3025912",
              "doi:10.1145/3025453.3025912"
            )
          ),
          p(
            "Mullen, K., Ardia, D., Gil, D., Windover, D., & Cline, J. (2011).
            'DEoptim': An R Package for Global Optimization by
            Differential Evolution.",
            em("Journal of Statistical Software"), ", 40(5), 1-26.",
            a(
              href = "https://www.jstatsoft.org/article/view/v040i06",
              "doi:10.18637/jss.v040.i06"
            )
          ),
          p(
            "Touloumis, A. (2016). Simulating Correlated Binary and
            Multinomial Responses under Marginal Model Specification:
            The SimCorMultRes Package.",
            em("The R Journal"), ", 8(2), 79-81. Retrieved from ",
            a(
              href = "https://journal.r-project.org/archive/2016/RJ-2016-034/index.html",
              "https://journal.r-project.org/archive/2016/RJ-2016-034/index.html"
            )
          ),
          p(
            "Warmbrod, J.R. (2014). Reporting and Interpreting Scores Derived
            from Likert-type Scales.",
            em("Journal of Agricultural Education"), ". 55 (5): 30–47.",
            a(
              href = "https://www.jae-online.org/index.php/back-issues/189-volume-55-number-5-2014/1862-reporting-and-interpreting-scores-derived-from-likert-type-scales",
              "doi:10.5032/jae.2014.05030."
            )
          ),
          p(
            "Winzar, H. F. (2022). LikertMaker: R Shiny program for simulating
            Likert-scales with predefined mean and standard deviation.
            Retrieved from ",
            a(
              href = "https://winzarh.github.io/",
              "https://winzarh.github.io/"
            )
          )
        )
        ### end tab panel
      )
      ### end tabsetPanel
    )
    ### end main panel
  )
  ### end sidebar layout
)
### end UI
###




###
### Define server logic
server <- function(input, output) {
  ## bslib::bs_themer()

  ## Install any needed packages
  my_required_packages <- c(
    "shiny", "dplyr", "ggplot2", "reactable",
    "DEoptim", "betafunctions", "shinybusy",
    "bslib", "beepr"
  )
  lapply(
    my_required_packages,
    function(x) if (!require(x, character.only = TRUE)) install.packages(x)
  )
  ## finish package installation

  ## load packages
  library(shiny)
  library(dplyr) ## data manipulation
  library(ggplot2) ## plotting
  library(reactable) ## render data table
  library(DEoptim) ## Optimisation algorithm
  library(betafunctions) ## Notional density estimation
  library(shinybusy) ## progress indicator
  library(bslib) ## pretty themes
  library(beepr) ## ping when finished

  ## begin notional output ---
  output$distPlot <- renderPlot({
    xmin <- max(
      1,
      input$target_mean_scale - input$target_sd_scale
    )
    xmax <- min(
      input$target_mean_scale + input$target_sd_scale,
      input$item_range
    )
    mean_point <- data.frame(x = input$target_mean_scale, y = 0.1)
    minor.breaks <- seq(1, input$item_range, 1 / input$items_n)
    boundaries <- c(
      (1 - 1 / input$items_n),
      (input$item_range + 1 / input$items_n)
    )

    ## begin mean and sd visualisation
    ggplot() +
      scale_x_continuous(
        breaks = c(1:input$item_range),
        minor_breaks = minor.breaks,
        limits = boundaries,
        expand = c(0, 0),
        name = "scale value"
      ) +
      geom_segment(aes(
        x = input$target_mean_scale, xend = xmax,
        y = 0.1, yend = 0.1
      ),
      colour = "blue", alpha = 1.0, size = 5
      ) +
      geom_segment(aes(
        x = xmin, xend = input$target_mean_scale,
        y = 0.1, yend = 0.1
      ),
      colour = "blue", alpha = 0.6, size = 5
      ) +
      geom_point(
        data = mean_point, aes(x = x, y = y),
        size = 6, colour = "orange"
      ) +
      labs(
        title = "expected Likert scale",
        subtitle = paste(
          "items=", input$items_n,
          ", mean=", input$target_mean_scale,
          ", sd=", input$target_sd_scale,
          ", n=", input$sample_n
        )
      ) +
      ylab("range") +
      guides(y = "none") +
      theme_minimal() +
      theme(
        panel.background = element_rect(
          fill = "lightblue",
          colour = "lightblue",
          size = 0.5, linetype = "solid"
        ),
        panel.grid.major = element_line(
          size = 0.5, linetype = "solid",
          colour = "white"
        ),
        panel.grid.minor = element_line(
          size = 0.25, linetype = "solid",
          colour = "white"
        )
      ) +
      theme(legend.position = "none")
    # end mean and sd visualisation
  })
  ## end notional output ---

  ## begin notional histogram ---
  output$notionalPlot <- renderPlot({

    ## rescale moments to suit 0-1 beta distribution
    betamean <- (input$target_mean_scale * input$items_n - input$items_n) /
      (input$item_range * input$items_n - input$items_n)

    betasd <- (input$target_sd_scale * input$items_n) /
      (input$item_range * input$items_n - input$items_n)

    ## density estimate for each possible level in Likert scale
    betaintervals <- seq(0, 1, (1 / (input$items_n * (input$item_range - 1))))

    ## generate vector of density estimates
    dbet <- betafunctions::dBetaMS(
      x = betaintervals,
      mean = betamean,
      sd = betasd
    )

    ## bringing densities and intervals together
    ## and rescaling intervals to suit desired Likert scale properties
    mydat <- cbind(
      intervals = (betaintervals * (input$item_range - 1) + 1),
      density = dbet
    ) |>
      data.frame()

    ## plot the expected densities
    ggplot(data = mydat, aes(x = intervals, y = density)) +
      geom_line(stat = "identity", color = "steel blue", size = 2) +
      geom_bar(stat = "identity", fill = "sky blue", alpha = 0.5) +
      scale_x_continuous(
        breaks = c(1:input$item_range),
        limits = c(1 - 1 / input$items_n, input$item_range + 1 /
          input$items_n), expand = c(0, 0),
        name = NULL
      ) +
      theme_classic(base_size = 15) +
      labs(
        title = "expected outcome based on scaled Beta distribution",
        subtitle = paste(
          "items=", input$items_n,
          ", mean=", input$target_mean_scale,
          ", sd=", input$target_sd_scale
        ),
        y = NULL,
        x = NULL
      )
  })
  ## end notional histogram ---

  ###
  ### Reaction to Go button ---

  ### begin data generation function
  gendata <- reactive({
    ## parameters --
    scale_min <- input$items_n
    scale_max <- input$item_range * input$items_n

    target_mean <- input$target_mean_scale * input$items_n
    target_std <- input$target_sd_scale * input$items_n
    # end parameters --

    ## Optimisation target ---
    opt_scale <- function(x) {
      target_stat <- ((target_mean - mean(x)) * 200)^2 +
        ((target_std - sd(x)) * 100)^2
      target_stat
    }

    ## optimisation process ---
    lower <- rep(scale_min, each = input$sample_n)
    upper <- rep(scale_max, each = input$sample_n)
    itermax <- input$sample_n * 20

    fnmap_f <- function(x) round(x)
    my_vector <- DEoptim(opt_scale, lower, upper,
      control = DEoptim.control(
        itermax = itermax,
        trace = FALSE,
        parallelType <- 1
      ),
      fnMap = fnmap_f
    )

    mydat1 <- summary(my_vector)
    gendata <- data.frame(scale = mydat1[["optim"]][["bestmem"]])
    gendata <- gendata / input$items_n

    data.structure <- paste0(
      "r-", input$item_range,
      "_i-", input$items_n,
      "_m-", input$target_mean_scale,
      "_sd-", input$target_sd_scale
    )

    gendata$structure <- data.structure
    df <- gendata
    beepr::beep(1)
    df
  })
  ### end data generation function

  ### execute data generation
  mydata <- eventReactive(input$goButton, {
    gendata()
  })

  ### present generated data
  output$mystuff <- renderReactable({
    reactable(mydata())
  })

  ### Plot the derived data as histogram
  myhistogram <- eventReactive(input$goButton, {
    ggplot(data = mydata(), aes(x = scale)) +
      geom_histogram(
        binwidth = (1 / input$items_n),
        fill = "light blue", alpha = 0.5,
        colour = "blue"
      ) +
      scale_x_continuous(
        breaks = c(1:input$item_range),
        limits = c(
          1 - (1 / input$items_n),
          (input$item_range + (1 / input$items_n))
        ),
        expand = c(0, 0), name = "scale value"
      ) +
      labs(
        title = "Derived scale distribution",
        subtitle = paste(
          "items=", input$items_n,
          ", mean=", round(mean(mydata()$scale), 3),
          ", sd=", round(sd(mydata()$scale), 3),
          ", median=", round(median(mydata()$scale), 3),
          ", n=", input$sample_n
        ),
        y = NULL
      ) +
      guides(y = "none") +
      theme_classic(base_size = 15) +
      theme(legend.position = "none")
    ### end plot the derived data
  })

  ## render myhistogram
  output$resultPlot <- renderPlot({
    myhistogram()
  })

  ## begin save my histogram
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("r-", input$item_range,
        "_i-", input$items_n,
        "_m-", round(mean(mydata()$scale), 2),
        "_sd-", round(sd(mydata()$scale), 2),
        "_n-", input$sample_n,
        ".png",
        sep = ""
      )
    },
    content <- function(file) {
      ggsave(file,
        plot = last_plot(), device = "png",
        scale = 3, width = 80, height = 45,
        units = "mm", dpi = 150
      )
    }
  )
  ## end save my histogram

  ## Downloadable csv of selected dataset ---
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("r-", input$item_range,
        "_i-", input$items_n,
        "_m-", input$target_mean_scale,
        "_sd-", input$target_sd_scale,
        "_n-", input$sample_n,
        ".csv",
        sep = ""
      )
    },
    content = function(file) {
      write.csv(mydata(), file)
    }
  )
  ## end download csv
}
### end server logic

### Run the application
shinyApp(ui = ui, server = server)
