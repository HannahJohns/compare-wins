# Demonstration of a module to be loaded into shiny
list(
  module_name = "DOCUMENT-PREFDESIGN",
  module_label = "Preference Designer Toolkit",
  imports=NULL,
  ui_element = fluidPage(
    
    fluidRow(
      column(width=6,
       "The Preference Designer Toolkit provides access to tools for articulating
       what health facets are the most important, and how they should be combined
       for designing and analysing studies using Generalised Pairwise Comparisons.
       "   
      )
    ),
    fluidRow(
      tags$h3("Tools available:"),
    ),
    fluidRow(
      style = "height:500px;",  
      column(width=12,
             navlistPanel(
               tabPanel(title="Vote Synthesiser",
                        absolutePanel(style="overflow: auto;", height="500px",width="100%",
                                      tags$h3("Overview"),
                                      "The Vote Synthesiser produces a consensus-based ranking
                                      of outcomes (from best to worst, most important to least important, etc).
                                      This consensus-based ranking may then be used to facilitate further discussion
                                      among stakeholders, or used to inform how to rank health facets using the
                                      Preference Definition tab in Data Analysis.
                                      ",
                                      tags$h3("How to use this tool"),
                                      tags$ol(
                                        tags$li("Ask individual stakeholders to rank each item individually"),
                                        tags$li("Enter this data into a csv file with the following columns:",
                                                tags$ul(
                                                  tags$li("Name or ID of the stakeholder"),
                                                  tags$li("One column per item that was ranked")
                                                ),
                                                "Each row should contain the rankings of
                                                those items provided by each stakeholder."),
                                        tags$li("Import this csv into the Vote Synthesiser."),
                                        tags$li("For each column, select if it is the name/ID of the voter,
                                                an option that was ranked, or if it should be ignored."),
                                        tags$li("Click 'Synthesise votes'"),
                                        tags$li("The ranking will be calculated and displayed"),
                                      ),
                                      tags$h3("Details"),
                                      tags$p("The Vote Synthesiser uses Condorcet Voting to generate
                                      a consensus-based ranking. Condorcet Voting operates by comparing
                                      each pair of items. If more stakeholders prefer item A over item B, then item A
                                      will be ranked higher than item B."),
                                      tags$p(
                                        "If an even number of stakeholders prefer each item, then they will be tied.
                                        If there is ambiguity about how to rank items in this
                                        manner (e.g. more stakeholders prefer item A over item B,
                                        prefer item B over item C, and prefer item C over item A),
                                        then each impacted item will be considered tied."
                                      ),
                                      tags$p("The resulting ranking is called the",
                                             tags$em("Condorcet Score."),
                                             "A higher score corresponds to being more preferred.",
                                             "The Vote Synthesiser will attempt to break tied Condorcet Scores
                                             by calculating for each item, how many others sharing the same Condorcet Score
                                             that it is preferred to. This number is called", tags$em("valency"),
                                             ". The Vote Synthesiser will repeat this process again to further break ties."
                                             ),
                                      tags$h3("References"),
                                      tags$a(href="https://doi.org/10.1007/s10729-006-9003-6",target="_blank",
                                      'Utley, Martin, et al. "A consensus process
                                      for identifying a prioritised list of study
                                      questions." Health care management
                                      science 10.1 (2007): 105-110.'
                                      )

                        )
               ),
               tabPanel(title="Profile Explorer",
                        absolutePanel(style="overflow: auto;", height="500px",width="100%",
                                      tags$h3("Overview"),
                        "The Profile Explorer tab makes it possible to identify which method for combining health facets
                        best reflects clinical reality in a more holistic fashion. It achieves this by taking a list of
                        hypothetical 'patient profiles', each of which have an outcome measured across each health facet.
                        It then takes a ranking of these profiles provided by the Vote Synthesiser, and identifies what
                        method for combining these health facets best matches the provided ranking.
                        ",
                        tags$h3("How to use this tool"),
                        tags$ol(
                          tags$li("Load a CSV file contianing patient profiles into the Profile Explorer. This CSV file should contain
                                  a column uniquely identifying each profile (profile ID) and then any outcome information
                                  "),
                          tags$li("Format the data using the Data Processing tab within the Profile Explorer.
                                  Select 'Drop' to remove any columns that should not be included in the analysis.
                                  The profile ID and all columns of 'character' type will be dropped from analysis,
                                  but all other variables will be included unless otherwise specified. This means
                                  that if you are working with survival data, you should drop the
                                  column containing the censoring status unless you want to include 
                                  censoring status as a separate variable in addition to the time to event.
                                  For each variable, select if a higher value is better or if a lower value is better.
                                  For binary variables, 'higher' indicates presence of the outcome is beneficial and 'lower'
                                  indicates the presence of the outcome is harmful."
                                  ),
                          tags$li("Confirm that the formatted patient profiles look correct in the Processed Data Tab"),
                          tags$li("Navigate to the Vote Synthesiser tab and use it to rank these profiles from best (1) to worst.
                                   Each alternative being ranked should match a profile ID.
                                   If the ranking for these profiles already exists, this ranking can be provided
                                   as a single row to the Vote Synthesiser.
                                  "),
                          tags$li("Navigate back to the to the Profile Explorer. Select the name of the column containing the profile IDS. Click 'Go!'
                                  "),
                          tags$li("Wait for the progress bar to complete. This may take some time.
                                  "),
                          tags$li("A list of candidate methods will appear at the bottom of the page, ordered from best fitting to worst fitting.
                                   You can cycle through these using the 'Display Candidate Number' field.
                                  ")
                        )
                        
                        
                                      
                        )
               )
             )
      )         
      
    )
  ),
  server_element = substitute({})
)
