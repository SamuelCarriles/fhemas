[![Status: Alpha](https://img.shields.io/badge/status-alpha-orange.svg)](#) [![License](https://img.shields.io/badge/license-MIT-blue.svg)](#)

# fhir-schemas

A lightweight, Clojure library that generates validation schemas for FHIR resources.
It parses FHIR `StructureDefinitions` and produces ready-to-use schemas for `Malli`, `clojure.spec`, or any other validation engine.

## Philosophy

fhir-schemas is built around the idea of a pure schema-generation engine for FHIR.

Rather than shipping opinionated resource definitions or coupling internally to a specific FHIR version, the library provides the minimal
core required to parse StructureDefinitions and generate validation schemas from them.

This keeps the system flexible, composable, and adaptable to different contexts, validation engines, and custom FHIR ecosystems.

## Status & Roadmap

> **fhir-schemas** is currently in early active development.

To see what is currently implemented, what we are working on, and future plans, please visit the [GitHub Projects board.](https://github.com/users/SamuelCarriles/projects/7/views/1)

## How it works

fhir-schemas operates in two main phases:

**Phase 1 — Engine Initialization**

The engine first parses an `ElementDefinition` StructureDefinition (provided by the user)
to understand the base rules that all other FHIR definitions must follow.
It generates an internal schema to validate any incoming structure.

**Phase 2 — Schema Generation**

Once initialized, the engine processes target StructureDefinitions (e.g. `Patient`,
`Observation`, or custom profiles) by:

1. Validating their structure using the schema generated in Phase 1.
2. Parsing and transforming the definition into an Intermediate Representation (IR).
3. Using the IR to generate ready-to-use schemas for the target validation
   engine (Malli, clojure.spec, etc.).
