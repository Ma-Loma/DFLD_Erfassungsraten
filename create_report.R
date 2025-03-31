
# Erstellen eines docx --------------------------------------
# mit dem hier angegebenen Namen und Pfad und unter Verwendung der Vorlage eines Word-Dokumentes, unter dem angegebenen Pfad
rmarkdown::render("DFLD_Erfassungsraten.rmd",
                  officedown::rdocx_document(
                    plots = list(caption = list(style = "Abbildung", # Definition das Abbildungsuntertitel mit der Abbkürzung Abb eingeleitet werden
                                                pre = "Abb. ",
                                                sep = ": ")),
                    tables = list(caption = list(style = "Abbildung",
                                                 pre = "Tab.",
                                                 sep = ": ")),
                    reference_docx = "templates/Mittelteil.docx"
                  ), 
                  output_file = "export/DFLD_ErfassungsratenVer1.1.docx")

# erstellen eines html dokuments ------------------------------
rmarkdown::render("DFLD_Erfassungsraten.rmd",
                  rmarkdown::html_document(),
                  output_file = "export/DFLD_Erfassungsraten.html")

# erstellen eines pdf dokuments --------------------------------------
rmarkdown::render("DFLD_Erfassungsraten.rmd",
                  rmarkdown::pdf_document(),
                  output_file = "export/DFLD_Erfassungsraten.pdf")
