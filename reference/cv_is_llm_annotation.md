# Detect an LLM-based annotation request (annotateCellsLLM / ceLLMarkup) that has NOT yet specified tissue/condition/n -\> the agent must ask (the unified picker) before running. Round XXI: the LLM path now collects the same Tissue/Condition/n as the MarkerDB path. Returns TRUE only when the method is the LLM AND no tissue=/condition=/n= directive is present yet.

Detect an LLM-based annotation request (annotateCellsLLM / ceLLMarkup)
that has NOT yet specified tissue/condition/n -\> the agent must ask
(the unified picker) before running. Round XXI: the LLM path now
collects the same Tissue/Condition/n as the MarkerDB path. Returns TRUE
only when the method is the LLM AND no tissue=/condition=/n= directive
is present yet.

## Usage

``` r
cv_is_llm_annotation(msg)
```
