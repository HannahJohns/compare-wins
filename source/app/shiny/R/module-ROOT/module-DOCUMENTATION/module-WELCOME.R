# Demonstration of a module to be loaded into shiny
list(
  module_name = "WELCOME",
  module_label = "Introduction",
  imports=NULL,
  ui_element = fluidPage(
  tags$h2(HTML("<b>C</b>omparison <b>O</b>f <b>M</b>ultifaceted <b>P</b>references <b>A</b>s <b>R</b>esearch <b>E</b>ndpoints using <b>WIN</b> <b>S</b>tatistics")),
  hr(),
  fluidRow(
    column(width=6,
           tags$h3("Introduction"),
           tags$b("COMPARE WINS "),
           "provides a point-and-click interface to a type of statistical method",
           "called ",
           tags$em("Generalised Pairwise Comparisons."),
           tags$h4("What are Generalised Pairwise Comparisons?"),
           "Generalised Pairwise Comparisons estimate the chance that a randomly selected participant",
           "from the treatment group will have a better outcome than a randomly selected participant",
           "from the control group.",
           "Because the idea of 'better outcome' is extremely general, Generalised Pairwise Comparisons",
           "can consider trade-offs between multiple clinical outcomes.",
           "This allows the approach to better reflect the multifaceted nature of health outcomes.",
           
           hr(),
           tags$h3("How to use this software"),
           "This software can be navigated using the tabs at the top.",
           "The overall structure of the app is as follows:",
           tags$div(
             tags$ul(
               tags$li(
                 "The",
                 tags$em("Documentation"),
                 "tab provides detailed descriptions",
                 "of the tools provided by COMPARE WINS",
                 tags$ul(
                   tags$li(tags$em("Welcome"),"(you are here)"),
                   tags$li(tags$em("Defining Preferences:"),
                           "A detailed guide on methods for defining 'better off'"
                           ),
                   tags$li(tags$em("Conducting Analysis:"),
                           "A detailed guide on statistical methods"
                           )
                 )
               )
             ),
             tags$ul(
               tags$li("The",
                       tags$em("Analysis"),
                       "tab provides a pipeline for analysing data under the
                       Generalised Pairwise Comparisons approach. Within it",
                       "you will find:",
                       tags$ul(
                         tags$li(tags$em("Data Import:"),
                                 "Load in a data file for analysis"
                                 ),
                         tags$li(tags$em("Preference Definition:"),
                                 "Define a method for determining outcome preferences"
                                 ),
                         tags$li(tags$em("Analysis:"),
                                 "Run statistical analysis using the loaded data and outcome preference definition"
                                 )
                       )
               )
             )
           )
           ),
    column(width=6,
           tags$img(width=800,src="win_diagram.svg")
          )
  ),
  hr(),
  tags$h3("Credits"),
  "This research was funded in whole or part by the National Health and Medical Research Council Grant No. 1171422",
  tags$a(href="https://www.monash.edu/medicine/sphpm/austrim/home",
         target="_blank",
         "The Australian Trials Methodology Research Network")
  ),
  server_element = substitute({})
)
