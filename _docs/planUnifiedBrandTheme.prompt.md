# Unified brand -> generated mode-specific themes

## Goal

Refine this Rhino + Shiny + bslib + brand.yml app so that branding has a single canonical source of truth while still producing excellent light and dark mode readability.

The desired outcome is:

- one canonical authoring brand file
- generated single-mode light and dark brand artifacts for Shiny/bslib consumption
- no hand-maintained drift between unified and split brand files
- Bootstrap 5.3 color-mode behavior used intentionally, not mixed with an unrelated dark-only compiled theme
- strong contrast/readability in both modes while preserving the retro Miami Vice palette

## Confirmed environment and version constraints

Plan against the versions actually present in this project:

- R 4.5.2
- bslib 0.10.0
- Bootstrap 5.3.1 via bslib 0.10.0
- shiny 1.12.1
- brand.yml 0.1.0
- rhino 1.11.0

Do not recommend APIs or behavior that require newer package versions unless explicitly marked as future-looking.

## Key project facts

- The canonical candidate unified brand file already exists at [app/brand/brand.yml](app/brand/brand.yml).
- Existing split files already exist at [app/brand/brand-light.yml](app/brand/brand-light.yml) and [app/brand/brand-dark.yml](app/brand/brand-dark.yml).
- Current Shiny app wiring is inconsistent:
  - [app/main.R](app/main.R) uses `theme_brand_dark`
  - [app/main.R](app/main.R) uses `brand_dark_yml` for the navbar logo
  - [app/main.R](app/main.R) also includes `bslib$input_dark_mode()`
- Current theme module lives at [app/view/themes/dark.R](app/view/themes/dark.R).
- Brand assets are synced into the repo by [app/utils/update_brand.R](app/utils/update_brand.R).
- Repo conventions require `box::use` imports and Rhino-style module structure per [.github/copilot-instructions.md](.github/copilot-instructions.md).

## Final recommendation

Use this pattern:

1. `app/brand/brand.yml` is the only canonical authoring file.
2. Generate resolved single-mode brand artifacts from it:
   - `app/brand/brand-light.yml`
   - `app/brand/brand-dark.yml`
3. Feed those resolved single-mode artifacts into `bslib$bs_theme()`.
4. Do not hand-maintain unified and split files in parallel.

Short version:

**Author unified, generate split, consume split.**

## Why this is the best fit

### Package guidance

- `brand.yml` guidance is centered on a single `_brand.yml` as the main source of truth.
- Quarto supports both:
  - one unified brand file with `light`/`dark` values
  - separate `light` and `dark` brand files
- `bslib::bs_theme(brand = ...)` consumes one brand source at a time.
- `bslib::input_dark_mode()` is specifically for Bootstrap 5.3 color modes, not for magically swapping between two unrelated brand files.

### Why not choose only one split file as source of truth?

Do not use only `brand-light.yml` or only `brand-dark.yml` as the canonical source and let Bootstrap infer the opposite mode.

That is weaker for this palette because the design goal is not merely “works”; it is:

- readable in both modes
- visually intentional in both modes
- brand-consistent in both modes

### Why not compute everything ad hoc at runtime?

Avoid repeatedly stripping/plucking light or dark values from the unified file at app runtime.

That is less transparent, harder to debug, and less reusable than generating concrete resolved artifacts.

## Ranked options

### Best

1. Unified canonical file -> generated light/dark resolved files -> `bs_theme()`

### Good

2. Keep current split runtime files, but make them generated artifacts from unified

### Acceptable

3. Resolve light/dark brand objects in memory from unified and pass those objects to `bs_theme()`

### Weakest

4. Choose either `brand-light.yml` or `brand-dark.yml` as the source of truth and infer the opposite mode

## Implementation direction

### Phase 1: establish source of truth

- Treat [app/brand/brand.yml](app/brand/brand.yml) as canonical.
- Confirm that the `tp-*` palette remains mode-invariant.
- Treat only semantic role values as mode-varying where needed:
  - `color.background`
  - `color.foreground`
  - optional semantic surface roles like `tertiary`
  - typography colors/backgrounds where needed for readability
  - logo variants only if actually needed

### Phase 2: generate resolved artifacts

Create a generation step that resolves the unified brand into two single-mode brand files:

- [app/brand/brand-light.yml](app/brand/brand-light.yml)
- [app/brand/brand-dark.yml](app/brand/brand-dark.yml)

This generation step should:

- keep shared palette entries unchanged
- resolve `light`/`dark` scalar objects to one concrete value per file
- preserve brand structure expected by Quarto and `read_brand_yml()`
- make generated files inspectable and deterministic

### Phase 3: consume resolved artifacts in Shiny

In [app/view/themes/dark.R](app/view/themes/dark.R):

- read the generated split brand files
- create one `bs_theme()` per mode from those files
- keep any additional `bs_add_variables()` / Sass layering small and intentional

In [app/main.R](app/main.R):

- stop mixing a dark-only compiled theme with `input_dark_mode()`
- decide whether runtime switching means:
  - switching between the compiled light and dark `bs_theme()` objects, or
  - using a single compiled theme with Bootstrap color modes in a way that actually matches the brand architecture

Given current package behavior, compiled per-mode themes will likely be clearer.

### Phase 4: Quarto alignment

Keep Quarto compatible with whichever form is easiest operationally.

If Quarto still benefits from split files, that is fine, but they should be generated from unified rather than hand-maintained.

## Specific cautions

- Do not assume Bootstrap 5.3.8 docs map exactly to this app. This app is effectively on Bootstrap 5.3.1 through `bslib` 0.10.0.
- Do not assume `input_dark_mode()` swaps brands. It toggles Bootstrap color modes.
- Do not rely on `bs_theme_preview()` as architecture guidance. Use it only for inspection.
- Do not introduce `library()` or `::` in app modules; use `box::use` only.
- Do not keep unified plus split files all hand-edited.

## Useful references and excerpts

### bslib `bs_theme()` guidance

Current `bslib` reference says `brand` can be:

- auto-discovered `_brand.yml`
- `TRUE`
- `FALSE`
- a file path to a specific brand file
- a list following the brand.yml structure

Important implication: `bs_theme()` consumes one brand source per call.

Reference:

- https://rstudio.github.io/bslib/reference/bs_theme.html

### bslib dark mode guidance

Current `bslib` docs for `input_dark_mode()`:

- it toggles between light and dark **Bootstrap color modes**
- the server value is a string: `"light"` or `"dark"`

Important implication: this is not a two-brand-file orchestration API.

Reference:

- https://rstudio.github.io/bslib/reference/input_dark_mode.html

### Quarto brand guidance

Current Quarto docs explicitly support both:

- unified file with mode-aware values
- separate light and dark brand files

Important excerpt summary:

- unified file is supported for colors/typography with `light` and `dark`
- separate files are supported if you prefer separate light/dark brands or need non-color customization
- palette entries themselves cannot currently hold light/dark variants

Reference:

- https://quarto.org/docs/authoring/brand.html

### brand.yml guidance

Current `brand.yml` homepage frames the model as:

- unified branding with a simple YAML file
- one `_brand.yml` to codify brand guidelines across tools

Reference:

- https://posit-dev.github.io/brand-yml/

## Helpful snippets to keep in mind

### Current `bslib` pattern being used

From this repo’s current theming approach:

```r
bslib$bs_theme(
  version = 5,
  preset = "bootstrap",
  brand = brand_dark_yml,
  "enable-gradients" = TRUE,
  "enable-shadows" = TRUE
) |>
  bslib$bs_add_variables(
    "navbar-bg" = "$primary",
    .where = "declarations"
  )
```

### Current unified brand already models mode-aware values

From [app/brand/brand.yml](app/brand/brand.yml):

```yaml
color:
  background:
    light: "#f0f0f0"
    dark: "#121212"
  foreground:
    light: "#222222"
    dark: "#f2f2f2"
```

This is why unified authoring is still the right source-of-truth model even if `bs_theme()` wants one resolved mode at a time.

## Success criteria

The final system should make these statements true:

- There is exactly one authoritative brand definition.
- Light and dark Shiny themes are generated from that authority, not hand-maintained separately.
- Both modes have intentional contrast and readable text on surfaces.
- The vibrant `tp-*` palette remains stable across modes.
- Navbar styling continues to work cleanly with `primary = tp-purple`.
- Quarto compatibility is preserved.
- App theming logic is simpler and more explicit than it is now.

## If implementation begins next

Start by designing the generation contract from unified brand -> resolved light/dark brand objects/files, then refactor theme construction around those outputs before touching deeper UI styling.
