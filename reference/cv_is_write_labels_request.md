# Detect a request to WRITE existing cluster/annotation labels INTO a Seurat/SCE object (addClustoData / addTypoData), NOT to annotate. Round XIX: "add the clustocell labels to my seurat obj" was wrongly intercepted by the method-choice chips because it matches "label" + "cluster". These are write-back requests that must fall through to the model (which routes to addClustoData/addTypoData).

Detect a request to WRITE existing cluster/annotation labels INTO a
Seurat/SCE object (addClustoData / addTypoData), NOT to annotate. Round
XIX: "add the clustocell labels to my seurat obj" was wrongly
intercepted by the method-choice chips because it matches "label" +
"cluster". These are write-back requests that must fall through to the
model (which routes to addClustoData/addTypoData).

## Usage

``` r
cv_is_write_labels_request(msg)
```
