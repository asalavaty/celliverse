
# Note that most of the free models have a daily limit and gpt3 seems to have the highest limit amongst all

# Also use check the prompts (inputs) suggested by Cell-o1, CellPuzzles (https://arxiv.org/html/2506.02911v1)

ceLLMarkup <- function(sample_source,
                       feature_type = "gene", # Indicate the feature type, i.e. gene, protein, etc.
                       marker_set_list = NULL, # This should be a named list of marker set dataframes. Each data frame should have two columns, feature (gene) names as the first column and the fold changes in the second column. It is recommended to only include the up-regulated features.
                       seuratClusters = NULL, # A data frame obtained as a result of running FindAllMarkers() of the Seurat package
                       padj = 0.05, # padj filtration threshold for filtering the markers of the seuratClusters
                       logFC = NULL, # logFC filtration threshold for filtering the markers of the seuratClusters. NULL means that no filtration will be done based on this parameter. However, only up-regulated genes will be retained.
                       reference_set_list = NULL,
                       model_choice = "gpt3", 
                       api_key, 
                       max_retries = 1) {
  
  # Load required libraries
  library(httr)
  library(jsonlite)
  library(janitor)
  library(tidyverse)

  if(all(is.null(seuratClusters) & is.null(marker_set_list)) | all(!is.null(seuratClusters) & !is.null(marker_set_list))) {
    stop("Either seuratClusters or marker_set_list should be provided!")
  }
  
  if(!is.null(seuratClusters)) {
    if(!all(c("avg_log2FC", "p_val_adj", "cluster", "gene") %in% colnames(seuratClusters))) {
      stop("The seuratClusters seems not to be the output of the FindAllMarkers function as it does not have all 'avg_log2FC', 'p_val_adj', 'cluster' and 'gene' columns")
    }
  }
  
  if(!is.null(marker_set_list)) {
    curr_list <- marker_set_list
  } else {
    
    # Filtering the data
    seuratClusters <- seuratClusters %>% 
      dplyr::filter(avg_log2FC > 0)
    
    if(!is.null(padj)) {
      seuratClusters <- seuratClusters %>% 
        dplyr::filter(p_val_adj < padj)
    }
    
    if(!is.null(logFC)) {
      seuratClusters <- seuratClusters %>% 
        dplyr::filter(avg_log2FC >= logFC)
    }
    
    seuratClusters <- seuratClusters %>% 
      dplyr::select(gene, avg_log2FC, cluster)
    
    # Making the list of marker sets
    curr_list <- seuratClusters %>% 
      group_by(cluster) %>% 
      group_split(.keep = FALSE)
    
    names(curr_list) <- paste("Cluster_", unique(seuratClusters$cluster), sep = "")
  }
  
  #__________________________________________________________
  
  # The ceLLMarkup bot function
  ceLLMarkup_bot <- function(sample_source,
                             feature_type = "gene", # Indicate the feature type, i.e. gene, protein, etc.
                             reference_set_list = NULL,
                             model_choice = "deepseek", 
                             marker_set_df, 
                             api_key, 
                             max_retries = 1) {
  
  # Map the model choice to the corresponding model identifier
  model_map <- list(
    gpt3 = "openai/gpt-3.5-turbo",
    gpt4 = "openai/gpt-4o-2024-11-20",
    qwen = "qwen/qwen-vl-plus:free", # Consistent results
    openchat = "openchat/openchat-7b:free",
    deepseek = "deepseek/deepseek-r1", # Consistent results but Very slow
    gemini = "google/gemini-2.0-flash-lite-preview-02-05:free", # The same (almost) consistent results as Deepseek but much faster
    claude = "anthropic/claude-3.5-sonnet"
  )
  
  if (!(model_choice %in% names(model_map))) {
    stop("Invalid model choice. Please choose from: gpt3, gpt4, qwen, openchat, deepseek, gemini, or claude.")
  }
  
  model <- model_map[[model_choice]]
  
  # Define the API endpoint and your API key (replace with your actual API key)
  url <- "https://openrouter.ai/api/v1/chat/completions"
  
  curr_terms <- marker_set_df %>% 
    apply(MARGIN = 1, FUN = paste0, collapse = ":") %>% 
    paste0(collapse = "\n")
  
  # Define the prompt
  if(is.null(reference_set_list)) {
    prompt <- paste(sep = "",
      "Given the following top differentially expressed ", 
      feature_type, "s", 
      "for a given cluster and their average log2 fold change in comparison to their expression in not that cluster of a dataset of single cells obtained from ", 
      sample_source, 
      ", please provide your best guess as to what the cell type is. Don't provide details. Just respond with the name of the cell type, nothing else. If you can be more specific (e.g. CD4 T cells as opposed to T cells) then please do. The data are as follows:\n",
      curr_terms
    )
  } else {
    prompt <- ""
  }
  
  # Quote prompt for Use in OS Shells
  prompt <- shQuote(prompt)
  
  # Create the JSON payload for the POST request
  payload <- list(
    model = model,
    messages = list(
      list(
        role = "user",
        content = prompt
      )
    )
  )
  
  # Convert the payload to JSON
  payload_json <- toJSON(payload, auto_unbox = TRUE)
  
  # Send the POST request to the API
  response <- POST(
    url,
    add_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    body = payload_json
  )
  
  # Check for errors in the API response
  if (status_code(response) != 200) {
    stop("Error: Received status code ", status_code(response), "\n", content(response, "text"))
  }
  
  # Parse the JSON response
  result <- content(response, "parsed")
  
  if(!is.null(result$error$message)) {
    reset_time <- as.POSIXct(as.numeric(result$error$metadata$headers$`X-RateLimit-Reset`) / 1000, origin = "1970-01-01", tz = "UTC")
    wait_time <- as.numeric(reset_time - Sys.time()) %>% round(digits = 1)
    stop(paste0("\n", result$error$message, "\n",
                    "Time left until reset: ", wait_time, " hours\n"))
  }
  
  if(is.null(result$choices[[1]]$message$content)) {
    
    message("Retrying...")
    
    # Local variable to track retries
    if (!exists("tmp_retries", envir = .GlobalEnv)) {
      assign("tmp_retries", 1, envir = .GlobalEnv)
    }
    if (tmp_retries < max_retries) {
      
      tmp_retries <<- tmp_retries + 1
      
      return(Recall(sample_source = sample_source,
                    feature_type = feature_type, 
                    reference_set_list = reference_set_list,
                    model_choice = model_choice, 
                    marker_set_df = marker_set_df,
                    api_key = api_key, 
                    max_retries = max_retries)
             )
    } else {
      rm(tmp_retries, envir = .GlobalEnv)
      stop("API request failed multiple times. The model you have selected is either not not available or is not free and you do not have enough credit for submitting the query!")
    }
  }
  
  # Return the reply
  if(is.null(result$choices[[1]]$message$content)) {
    rm(tmp_retries, envir = .GlobalEnv)
  }
  curr_cell_type <- result$choices[[1]]$message$content %>% 
    janitor::make_clean_names() %>% 
    gsub(pattern = "_cells$|_cell$", replacement = "") %>% 
    gsub(pattern = "_", replacement = " ") %>% 
    tools::toTitleCase()
  
  if(nchar(curr_cell_type) == 1) {
    curr_cell_type <- paste0(curr_cell_type, " Cell") %>% 
      tools::toTitleCase()
  }
  return(curr_cell_type)
  }
  #__________________________________________________________
  
  # Applying the ceLLMarkup_bot function on each cluster
  
  cell_types <- sapply(names(curr_list), function(i) {
    
    cat(paste0("Annotating ", i, " is in progress ... "))
    
    tmp_df <- curr_list[[i]]
    
    tmp_cluster_annot <- 
      ceLLMarkup_bot(sample_source = sample_source,
                     feature_type = feature_type,
                     reference_set_list = reference_set_list,
                     model_choice = model_choice, 
                     marker_set_df = tmp_df,
                     api_key = api_key, 
                     max_retries = max_retries)
    
    cat("Done!\n")
    Sys.sleep(2)
    return(tmp_cluster_annot)
  })
  return(cell_types)
}









