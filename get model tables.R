tab_model(best.model.a.1,
          pred.labels = c("Intercept", "Alien Status - Alien","Aspect Ratio",
                          "Tank Size", "Predator Mass",
                          "Prey Mass", "Temperature", "Habitat - Marine",
                          "Alien : Temperature", "Aspect Ratio : Predator Size",
                          "Aspect Ratio : Prey Size", "Predator Mass : Prey Size",
                          "Predator Mass : Temperature", "Aspect Ratio : Predator Mass : Prey Size"),
          dv.labels = "Space Clearance Rate (log transformed)",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value")

tab_model(best.model.h.1,
          pred.labels = c("Intercept", "Aspect Ratio",
                          "Predator Size",
                          "Prey Size", "Temperature", "Habitat - Marine",
                          "Aspect Ratio : Prey Size", "Aspect Ratio : Temperature",
                          "Predator Size : Prey Size", "Predator Size : Temperature", 
                          "Prey Size : Temperature", "Aspect Ratio : Prey Size : Temperature"),
          dv.labels = "Handling Time (log transformed)",
          string.ci = "Conf. Int (95%)",
          string.p = "P-Value")

