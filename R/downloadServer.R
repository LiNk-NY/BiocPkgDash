downloadServer <- function(id, email, biocver) {
    moduleServer(
        id,
        function(input, output, session) {
            output$btnSend <- downloadHandler(
                filename = function() {
                    em <- gsub("@", "_at_", email())
                    em <- gsub("\\.", "_dot_", em)
                    paste0(em, ".html")
                },
                content = function(file) {
                    BiocPkgDash:::renderDoc(
                        email = email(),
                        version = biocver(),
                        file = file
                    )
                }
            )
        }
    )
}
