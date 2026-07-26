# fhemas

A lightweight, Clojure-native, data-driven FHIR validator.

Based on: <https://hl7.org/fhir/validation.html>

## Philosophy

fhemas is built around the idea of validating FHIR resources directly,
without depending on the Java FHIR ecosystem. Given the FHIR resources
required for validation (the official FHIR package — StructureDefinitions,
ValueSets, etc.), the engine validates any target resource against them.

The library stays data-driven: a custom resource called **SchemaProfile**
tells the engine exactly what to extract from a StructureDefinition and
which functions to apply to process each field. The engine never guesses —
all extraction and compilation behavior is explicit, external configuration,
not hardcoded per FHIR version.

## Status & Roadmap

fhemas is currently in early active development.
See the [GitHub Projects board](https://github.com/users/SamuelCarriles/projects/7/views/1)
for current progress and planned work.

## Validation aspects (per the FHIR spec)

1. Structure
2. Cardinality
3. Value Domains
4. Coding/CodeableConcept bindings
5. Invariants
6. Profiles
7. Questionnaires
8. Business Rules (explicitly defined by the spec as _outside_ the specification itself)

## Scope decisions

- **Questionnaire / QuestionnaireResponse**: out of scope for v1. Self-
  contained sub-validator with its own rules, independent from
  StructureDefinition-based validation. Not discarded — revisited later.

- **Business Rules** (reference resolution, duplicate checks, authorization,
  etc.): the spec itself lists these as rules "made outside the
  specification". Treated as an optional, decoupled layer on top of the core
  engine. Behavior (hard error / warning / skip) is configurable by the
  implementer.

## How it works

### SchemaProfile

A SchemaProfile is a custom resource that fully describes how to process
StructureDefinitions for a given FHIR version: which root fields matter
(`:meta`), which ElementDefinition fields matter and how to compile them
(`:schema :elements`), including cardinality, type, slicing, bindings, and
FHIRPath constraints.

The engine does not derive this knowledge on its own — SchemaProfiles are
authored by hand and distributed as data. This also covers the special case
of validating StructureDefinition itself (since a StructureDefinition that
defines StructureDefinition/ElementDefinition is, structurally, just another
StructureDefinition): the same SchemaProfile-driven process handles it,
there is no separate bootstrap mechanism.

### Two layers of validation

1. **SchemaProfile validation (fixed, hand-written)**: every SchemaProfile
   is itself validated against a static Malli schema before use. This is
   not derived from any SchemaProfile — it's the one fixed piece of the
   system. This is what lets the engine trust the SchemaProfile it's about
   to use.

2. **Target resource validation (SchemaProfile-driven)**: once a
   SchemaProfile is validated, the engine uses it to extract and compile
   fields from any StructureDefinition, and to run the validation pipeline
   below against target resources.

### Compilation functions (`:compile/field`)

Fields in a SchemaProfile can declare a `:compile/field` (or
`:compile/with-group`) pointing to a namespaced keyword (e.g.
`:fhemas.compile.r4/inheritance`). The engine resolves this to an actual
function via `resolve`.

**Security note**: this means the engine executes whatever function the
SchemaProfile points to. Only use SchemaProfiles from trusted sources — the
official artifacts repo ([SamuelCarriles/artifacts](https://github.com/SamuelCarriles/artifacts))
is the recommended source. Loading a SchemaProfile from an untrusted source
means trusting arbitrary code resolution.

**Practical requirement**: the namespace containing the compile functions
(e.g. `fhemas.compile.r4`) must be `require` in the consumer's runtime for
`resolve` to find it. If it isn't required, resolution silently fails.

## Validation pipeline (build order)

The spec lists validation aspects but does not prescribe an order. The
order below was derived by asking, for each aspect: _"what must already be
resolved for this aspect to be meaningful?"_ This pipeline order is a fixed
responsibility of the engine — it is not something a SchemaProfile
declares or a consumer can reconfigure.

- **Hard dependency**: aspect B cannot be checked correctly without aspect A
  having happened first. Reordering breaks correctness.
- **Optimization**: aspects are independent of each other, but one runs
  first because it's cheaper to compute, enabling fail-fast.

| Step | Aspect                                             | Ordering reason                                                                                                                                                                                           |
| ---- | -------------------------------------------------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| 0    | **Resolve applicable Profile/StructureDefinition** | Hard dependency — prerequisite for everything else.                                                                                                                                                       |
| 1    | **Structure**                                      | Hard dependency on step 0.                                                                                                                                                                                |
| 2    | **Cardinality**                                    | Hard dependency on step 0. Runs before Value Domains as an optimization (cheaper, fails fast).                                                                                                            |
| 3    | **Value Domains**                                  | Hard dependency on step 0. Logically independent from Cardinality.                                                                                                                                        |
| 4    | **Invariants**                                     | Hard dependency on step 3. Runs before Bindings as an optimization: FHIRPath evaluation is local computation, while Bindings may require expanding a ValueSet or querying an external terminology server. |
| 5    | **Bindings** (Coding/CodeableConcept)              | Hard dependency on step 3. Logically independent from Invariants — ordering vs. step 4 is a cost-based optimization, not a correctness requirement.                                                       |

If ValueSet expansion is fully local and cached, the optimization reason for
ordering Invariants before Bindings no longer applies, and both could run in
any order — the hard dependencies on step 3 still hold regardless.

Deferred to later phases (not part of the core pipeline above):

- **Profiles** (validating rules _added by_ a specific profile, distinct
  from _resolving which_ profile applies in step 0): layered on top of
  steps 1–5.
- **Questionnaires**: separate sub-validator, out of scope for v1.
- **Business Rules** (including reference resolution): optional layer, see
  Scope decisions above.
