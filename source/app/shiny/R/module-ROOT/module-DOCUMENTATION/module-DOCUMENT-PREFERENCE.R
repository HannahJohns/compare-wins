# Demonstration of a module to be loaded into shiny
list(
  module_name = "DOCUMENT-ANALYSIS",
  module_label = "Analysis Methods",
  imports=NULL,
  ui_element = fluidPage(
  fluidRow(
    column(width=6,
    "Generalised Pairwise Comparisons methods work by comparing all participants
      in the control group to all participants in the intervention group.
      These comparisons produce one of three types of outcomes:",
    tags$ul(
      tags$li(tags$b("Wins")," - the participant in the intervention group had a better outcome than the person in the control group"),
      tags$li(tags$b("Losses")," - the participant in the intervention group had a worse outcome than the person in the control group"),
      tags$li(tags$b("Ties")," - there was no observed difference between these two participants")
    )
    )
  ),
  fluidRow(
    column(width=6,
           "To use this, the Generalised Pairwise Comparisons approach requires
           a formalised method for determining which combination
           of outcomes is considered to be preferable. COMPARE WINS supports
           several approaches that have been proposed in medical literature."
    )
  ),
  fluidRow(
    tags$h3("Methods for defining preference supported by COMPARE WINS"),
  ),
  fluidRow(
    style = "height:500px;",  
    column(width=12,
           navlistPanel(
             tabPanel(
               title="Heirarchical",
               absolutePanel(style="overflow: auto;", height="500px",width="100%",
                             tags$h3("Overview"),
                             
                             "Also called the 'Prioritised Preferences'
                             method. Under this approach, each outcome's importance
                             is individually ranked. The procedure for determining
                             wins, losses and ties for each pair is then given
                             by the following steps:
                             ",
                             tags$ol(
                               tags$li("Start with the most important outcome"),
                               tags$li("Is there a difference in this outcome for this pair of participants?",
                                       tags$ul(
                                         tags$li(tags$b("If Yes:"),
                                                 tags$ul(
                                                   tags$li(tags$b("If this outcome is better for treatment participant:"),"This pair is a win"),
                                                   tags$li(tags$b("If this outcome is better for control participant:"),"This pair is a loss")
                                                 )
                                                 ),
                                         tags$li(tags$b("If No:"),"Go to step 3")
                                       )
                                       ),
                               tags$li("Is there an outcome that's less important than this one?",
                                       tags$ul(
                                         tags$li(tags$b("If Yes:"),"Consider next most important outcome and go to step 2"),
                                         tags$li(tags$b("If No:"),"Go to step 4")
                                       )
                               ),
                               tags$li("This pair is tied on all outcomes")
                             ),
                             
                             tags$h3("How Preference is Determined for Individual Outcomes"),
                             
                             "For individual outcomes in this approach vary depending on the
                             type of outcome supplied. COMPARE-WINS supports
                             Numeric/Ordinal, Binary, and Survival Endpoint data:",
                             
                             tags$h4("Numeric/Ordinal Endpoints"),
                             
                             "Wins/losses/ties based on a numeric or ordinal 
                             endpoint are determined based on the direction 
                             of effect, i.e. a lower score is considered better/worse.
                             ",
                             
                             "In COMPARE WINS, you can set the direction of effect using the",
                             tags$b("Preference Direction"),
                             " option. By default, Higher values are considered better.",
                             
                             tags$h4("Binary Endpoints"),
                             
                             "Because binary endpoints are conventionally coded as 1 (Yes) and 0 (No), 
                             wins/losses/ties are determined in the same manner as if they were numeric.
                             If the presence of the endpoint is considered beneficial, then
                             the direction is set to consider a higher value to be preferable
                             If the presence of the endpoint is considered harmful, then
                             the direction is set to consider a lower value to be preferable.
                             ",
                             
                             tags$h4("Survival Endpoints"),
                             
                             "Wins/losses/ties based on a survival endpoint are determined
                             based not just on the direction of effect, but also on both
                             the time of an event, and if the observation is censored.
                             
                             A win or loss is determined under the following circumstances
                             ",
                             tags$ul(
                               tags$li("Both participants experience events at different times"),
                               tags$li("One participant experiences an event, 
                                       and the other is censored at a time",
                                       tags$b("after"),
                                       " the event has taken place
                                       (i.e. if an unobserved event occurs,
                                       it has to happen after the observed event)")
                             ),
                             "In all other cases, the pair is considered tied.",
                             
                             tags$h3("Minimal Clinical Difference"),
                             
                             "By default a pair is considered a win/loss if there is
                             any observed difference between them. However, this definition
                             can be restricted to require that the difference must be larger
                             than some minimum difference, or it is considered a tie instead.
                             This allows the generalised pairwise comparison method to also
                             consider the size of the difference when determining preference.",
                             
                             "In COMPARE WINS, you can set a minimal clinical difference
                             by adjusting the ", tags$br("Clinical Difference Threshold"),
                             "., By default, this is zero (i.e. any difference is considered meaningful).",
                             

                             # tags$h3("Example"),
                             # 
                             # # Replace this with a static image
                             # tags$table(
                             #   tags$tr(
                             #     tags$th(
                             #       "Participant"
                             #     ),tags$th(
                             #       "Outcome 1"
                             #     ),tags$th(
                             #       "Outcome 2"
                             #     ),tags$th(
                             #       "Outcome 3"
                             #     ) 
                             #   ),
                             #   tags$tr(
                             #     tags$td(
                             #       "A"
                             #     ),tags$td(
                             #       "1"
                             #     ),tags$td(
                             #       "2"
                             #     ),tags$td(
                             #       "3"
                             #     ) 
                             #   ),
                             #   tags$tr(
                             #     tags$td(
                             #       "B"
                             #     ),tags$td(
                             #       "1"
                             #     ),tags$td(
                             #       "2"
                             #     ),tags$td(
                             #       "3"
                             #     ) 
                             #   )
                             # ),
                             
                             tags$h3("References and Further Reading"),
                             tags$ul(
                               tags$li(tags$a("Buyse, M. (2010). Generalized pairwise comparisons of prioritized
                                              outcomes in the two‐sample problem. Statistics in medicine,
                                              29(30), 3245-3257.",
                                              href="https://doi.org/10.1002/sim.3923")),
                               tags$li(tags$a(
                                 "Evans, S. R., & Follmann, D. (2016). Using outcomes to analyze patients rather
                                 than patients to analyze outcomes: a step toward pragmatism in benefit:
                                 risk evaluation. Statistics in biopharmaceutical research, 8(4), 386-393.",
                               ),
                               href="https://doi.org/10.1080/19466315.2016.1207561"),
                               tags$li(
                                 tags$a(
                                   "Pocock, S. J., Ariti, C. A., Collier, T. J., & Wang, D. (2012).
                                   The win ratio: a new approach to the analysis of composite endpoints
                                   in clinical trials based on clinical priorities. European heart journal, 33(2), 176-182.",
                                   href="https://doi.org/10.1093/eurheartj/ehr352"
                                 )
                               )
                             )
               )
             ),
             tabPanel(
               title="List-Based",
               absolutePanel(style="overflow: auto;", height="500px",width="100%",
                             tags$h3("Overview"),
                             
                             "The list-based approach to defining preference
                             works by the following process:
                             ",
                             
                             tags$ol(
                               tags$li("For each individual outcome create clinically meaningful bins"),
                               tags$li("Rank each possible combinations of possible bins across each outcome from best (1) to worst (max rank)"),
                               tags$li("Rank each participant based on the combination of binned outcomes")
                             ),
                             
                             "Unlike the Heirarchical approach, there is no
                             requirement that outcomes must be strictly ordered based on importance.
                             The list-based approach therefore allows for preference
                             definitions that reflect a trade-off between different outcomes.
                             
                             ",
                             
                             # tags$h3("Examples"),
                             
                             tags$h3("References and Further Reading"),
                             tags$ul(
                               tags$li(tags$a("Vivash, L., Johns, H., O'Brien, T. J., & Churilov, L. (2023).
                                       The adaptation of the desirability of outcome ranking for
                                       interventional clinical trials in epilepsy: A novel consumer‐led
                                       outcome measure. Epilepsia Open, 8(4), 1608-1615.",
                                       href="https://doi.org/10.1002/epi4.12839"
                                       )
                                      )
                             )
               )
              )
           )
      )
  )
  ),       
  server_element = substitute({})
)
