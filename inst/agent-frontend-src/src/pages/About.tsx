// About tab.
//
// A concise, professional overview: what the CelliVerse Agent is and what it can
// do, that it ships inside the celliverse R package (with the package's links),
// and a short bio of the developer. Static content page (no data fetching).
// Styled to match the rest of the app (cards, accent links, same fonts).

import type { CSSProperties } from "react";

const A: CSSProperties = { color: "#0279ee", textDecoration: "none" };

// One labelled link row.
function LinkRow({ label, href, display }: { label: string; href: string; display: string }) {
  return (
    <li>
      <strong>{label}:</strong>{" "}
      <a style={A} href={href} target="_blank" rel="noreferrer">{display}</a>
    </li>
  );
}

export default function About() {
  return (
    <div style={{ maxWidth: 780 }}>
      <p className="muted" style={{ marginTop: 0 }}>
        What this agent is, the R package it belongs to, and who built it.
      </p>

      {/* ---- The agent ---- */}
      <div className="card">
        <h3>CelliVerse Agent</h3>
        <p>
          The <strong>CelliVerse Agent</strong> is an AI assistant for single-cell analysis, embedded in the
          CelliVerse R package. You describe what you want in plain language — <em>"cluster this and annotate the
          cell types"</em>, <em>"show the top 10 markers of cluster C1"</em> — and the agent plans and runs the
          analysis for you by calling CelliVerse tools, streaming progress, and showing plots and tables inline.
        </p>
        <p style={{ marginBottom: 0 }}>It can:</p>
        <ul>
          <li>Cluster cells and sub-cluster major populations (clustoCell, markoClust).</li>
          <li>Identify and rank marker genes for clusters or custom cell subsets (getDatasetMarkers, getClusterMarkers, markoCell, markerPurity).</li>
          <li>Annotate cell types from a curated marker database or an LLM (typoClust, annotateCellsLLM / ceLLMarkup).</li>
          <li>Generate intuitive, downloadable visualizations (markoClustVis, typoClustVis) as editable SVG/PNG plus CSV tables.</li>
          <li>Run on fully-offline local models (Ollama, LM Studio) or cloud providers (OpenRouter, OpenAI, Anthropic, Gemini, …) — your data never leaves your machine on the local path.</li>
        </ul>
      </div>

      {/* ---- The package ---- */}
      <div className="card">
        <h3>The celliverse R package</h3>
        <p>
          The agent is part of <strong>celliverse</strong> — <em>An Ecosystem for Exploring the Universe of
          Single-Cell Data</em>. CelliVerse provides core functions for single-cell RNA-seq analysis: clustering
          cells, identifying markers for pre-defined clusters, sub-clustering major cell populations, and
          discovering markers within custom-selected subsets. It is designed to be independent of library size and
          other sample- or cell-level confounding effects, giving reliable, interpretable results across a wide
          range of datasets.
        </p>
        <ul style={{ marginBottom: 0 }}>
          <LinkRow label="GitHub" href="https://github.com/asalavaty/celliverse" display="github.com/asalavaty/celliverse" />
          <LinkRow label="Documentation" href="https://asalavaty.github.io/celliverse/" display="asalavaty.github.io/celliverse" />
          <LinkRow label="Vignettes" href="https://asalavaty.github.io/celliverse/articles/Vignettes.html" display="package vignettes &amp; workflows" />
          <LinkRow label="Hugging Face" href="https://huggingface.co/spaces/asalavaty/celliverse" display="huggingface.co/spaces/asalavaty/celliverse" />
          <LinkRow label="Report an issue" href="https://github.com/asalavaty/celliverse/issues" display="github.com/asalavaty/celliverse/issues" />
        </ul>
      </div>

      {/* ---- The developer ---- */}
      <div className="card">
        <h3>Developer</h3>
        <p>
          CelliVerse and the CelliVerse Agent are developed by <strong>Adrian Salavaty</strong>. Adrian earned his
          Ph.D. in Bioinformatics from Monash University (Australian
          Regenerative Medicine Institute) and works across bioinformatics and systems biology, graph-based model
          development, and multi-omics cancer analysis — with a track record of building network-analysis models and
          strategies for prioritizing candidate genes and proteins.
        </p>
        <p style={{ marginBottom: 0 }}>
          More about Adrian, his publications, and his other software:{" "}
          <a style={A} href="https://asalavaty.com/" target="_blank" rel="noreferrer">asalavaty.com</a>
        </p>
      </div>

      <p className="muted" style={{ fontSize: 12 }}>
        celliverse is released under the GPL-3 license. If you use CelliVerse in your work, please cite it — see{" "}
        <span className="mono">citation("celliverse")</span> in R.
      </p>
    </div>
  );
}
