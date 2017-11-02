library('SPARQL')
library('ggplot2')

arguments <- commandArgs (trailingOnly = TRUE)
if (length (arguments) == 0)
    stop ("Please pass the filename to write to as a parameter.")

endpoint  <- "http://localhost:8890/sparql"
filename  <- arguments[1]

plot_mutations_per_type <- function (endpoint)
{
    # The following query retrieves a table containing  the following columns:
    # variant, position1, chromosome1, position2, chromosome2, svtype
    query <- "PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX : <http://localhost:5000/cth/>

SELECT ?type COUNT(?type) as ?count {
  ?variant  rdf:type          :StructuralVariant .
  ?variant  :type             ?type .
}
GROUP BY ?type"

    # Perform the query.
    query_data <- NULL
    query_data <- tryCatch(SPARQL (endpoint, query),
                           error = function(e) { return (NULL) })

    # Handle connection errors and empty result sets.
    if (is.null (query_data) || nrow (query_data$results) == 0)
    {
        types             <- c("DEL", "DUP", "INS", "INV", "TRA")
        counts            <- c(0, 0, 0, 0, 0)
        data              <- as.data.frame(list(types, counts))
        colnames(data)    <- c("type", "count")

        query_data        <- c()
        query_data[[1]]   <- data
        names(query_data) <- c("results")
    }

    # Create the plot.
    qty_plot <- ggplot (data=query_data$results, aes(x=type, y=count)) +
        geom_bar (stat="identity") +
        xlab ("SV type") +
        ylab ("Number of SVs in the database")
        #ggtitle("Number of mutations per type") + 
        #theme (plot.title = element_text(lineheight=1.0, face="bold"))

    return (qty_plot)
}

mutation_types_plot <- plot_mutations_per_type (endpoint)
ggsave (file=filename, plot=mutation_types_plot, width=5, height=3, device="svg")
