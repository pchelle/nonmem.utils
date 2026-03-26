---
name: shiny-bslib-theming
description: Advanced theming for Shiny apps using bslib and Bootstrap 5. Use when customizing app appearance with bs_theme(), Bootswatch themes, custom colors, typography, brand.yml integration, Bootstrap Sass variables, custom Sass/CSS rules, dark mode and color modes, dynamic theme switching, real-time theming, theme inspection, or making R plots match the app theme with thematic.
metadata:
  author: Garrick Aden-Buie (@gadenbuie)
  version: "1.0"
  source: https://github.com/posit-dev/skills/blob/main/shiny/shiny-bslib-theming/SKILL.md
license: MIT
---

# Theming Shiny Apps with bslib

Customize Shiny app appearance using bslib's Bootstrap 5 theming system. From quick Bootswatch themes to advanced Sass customization and dynamic color mode switching.

## Quick Start

**"shiny" preset (recommended starting point):**
```r
page_sidebar(
  theme = bs_theme(),  # "shiny" preset by default — polished, not plain Bootstrap
  ...
)
```

**Bootswatch theme (for a different visual style):**
```r
page_sidebar(
  theme = bs_theme(preset = "zephyr"),  # or "cosmo", "minty", "darkly", etc.
  ...
)
```

**Custom colors and fonts:**
```r
page_sidebar(
  theme = bs_theme(
    version = 5,
    bg = "#FFFFFF",
    fg = "#333333",
    primary = "#2c3e50",
    base_font = font_google("Lato"),
    heading_font = font_google("Montserrat")
  ),
  ...
)
```

## Theming Workflow

1. Start with the `"shiny"` preset (default) or a Bootswatch theme close to your desired look
2. Customize main colors (`bg`, `fg`, `primary`)
3. Adjust fonts with `font_google()` or other font helpers
4. Fine-tune with Bootstrap Sass variables via `...` or `bs_add_variables()`
5. Add custom Sass rules with `bs_add_rules()` if needed
6. Enable `thematic::thematic_shiny()` so plots match the theme
7. Use `bs_themer()` during development for interactive preview

**Example:**
```r
theme <- bs_theme(preset = "minty") |>
  bs_theme_update(
    primary = "#1a9a7f",
    base_font = font_google("Lato")
  ) |>
  bs_add_rules("
    .card { box-shadow: 0 2px 8px rgba(0,0,0,0.1); }
  ")
```

## bs_theme()

Central function for creating Bootstrap themes. Returns a `sass::sass_bundle()` object.

```r
bs_theme(
  version = version_default(),
  preset = NULL,        # "shiny" (default for BS5+), "bootstrap", or Bootswatch name
  ...,                  # Bootstrap Sass variable overrides
  brand = NULL,         # brand.yml: NULL (auto), TRUE (require), FALSE (disable), or path
  bg = NULL, fg = NULL,
  primary = NULL, secondary = NULL,
  success = NULL, info = NULL, warning = NULL, danger = NULL,
  base_font = NULL, code_font = NULL, heading_font = NULL,
  font_scale = NULL,    # Scalar multiplier for base font size (e.g., 1.5 = 150%)
  bootswatch = NULL     # Alias for preset
)
```

Use `bs_theme_update(theme, ...)` to modify an existing theme. Use `is_bs_theme(x)` to test if an object is a theme.

### Presets and Bootswatch

**The "shiny" preset (recommended):** `bs_theme()` defaults to `preset = "shiny"` for Bootstrap 5+. This is a polished, purpose-built theme designed specifically for Shiny apps.

**Vanilla Bootstrap:** Use `preset = "bootstrap"` to remove the "shiny" preset and get unmodified Bootstrap 5 styling.

**Built-in presets:** `builtin_themes()` lists bslib's own presets.

**Bootswatch themes:** `bootswatch_themes()` lists all available Bootswatch themes.

Popular options: `"zephyr"` (light, modern), `"cosmo"` (clean), `"minty"` (fresh green), `"flatly"` (flat design), `"litera"` (crisp), `"darkly"` (dark), `"cyborg"` (dark), `"simplex"` (minimalist), `"sketchy"` (hand-drawn).

### Main Colors

The most influential colors — changing these affects **hundreds** of CSS rules via variable cascading:

| Parameter | Description |
|---|---|
| `bg` | Background color |
| `fg` | Foreground (text) color |
| `primary` | Primary brand color (links, nav active states, input focus) |
| `secondary` | Default for action buttons |
| `success` | Positive/success states (typically green) |
| `info` | Informational content (typically blue-green) |
| `warning` | Warnings (typically yellow) |
| `danger` | Errors/destructive actions (typically red) |

### Typography

Three font arguments: `base_font`, `heading_font`, `code_font`. Use `font_scale` to uniformly scale all font sizes.

```r
bs_theme(
  base_font = font_google("Roboto"),
  heading_font = font_google("Montserrat"),
  code_font = font_google("Fira Code")
)
```

## Low-Level Theming Functions

### bs_add_variables()

Add or override Bootstrap Sass variable defaults:

```r
theme <- bs_add_variables(
  bs_theme(preset = "sketchy", primary = "orange"),
  "body-bg" = "#EEEEEE",
  "font-family-base" = "monospace",
  "font-size-base" = "1.4rem"
)
```

### bs_add_rules()

Add custom Sass/CSS rules that can reference Bootstrap variables and mixins:

```r
theme <- bs_theme(primary = "#007bff") |>
  bs_add_rules("
    .custom-card {
      background: mix($bg, $primary, 95%);
      border: 1px solid $primary;
      padding: $spacer;
    }
  ")
```

From external file: `bs_add_rules(sass::sass_file("www/custom.scss"))`

## Bootstrap Sass Variables

Pass any Bootstrap 5 Sass variable through `bs_theme(...)` or `bs_add_variables()`.

**Finding variable names:** https://rstudio.github.io/bslib/articles/bs5-variables/

**Common variables:**
```r
bs_theme(
  "border-radius" = "0.5rem",
  "card-border-radius" = "1rem",
  "navbar-bg" = "$primary",
  "font-size-base" = "1rem"
)
```

## Dark Mode and Color Modes

Bootstrap 5.3's client-side color mode system:

- `input_dark_mode()` and `toggle_dark_mode()` for user-controlled switching
- Server-side theme switching with `session$setCurrentTheme()`

## Theming R Plots

`bs_theme()` only affects CSS. R plot output won't auto-match. Use the `thematic` package:

```r
library(thematic)
thematic_shiny(font = "auto")  # Call before shinyApp()
shinyApp(ui, server)
```

- Works with base R, ggplot2, and lattice
- Translates CSS colors into R plotting defaults
- `font = "auto"` also matches fonts from `bs_theme()`

## Dashboard Background Styling

The `bslib-page-dashboard` CSS class adds a light gray background behind the main content area:

```r
page_sidebar(
  class = "bslib-page-dashboard",
  title = "My Dashboard",
  sidebar = sidebar(...),
  ...
)
```

## Interactive Theming Tools

### bs_theme_preview()

Standalone demo app for previewing a theme:

```r
bslib::bs_theme_preview()
bslib::bs_theme_preview(bs_theme(preset = "darkly"))
```

### bs_themer()

Add the theme editor to your own app's server function (development only):

```r
server <- function(input, output, session) {
  bs_themer()  # Add during development, remove for production
  # ...
}
```

## Theme Inspection

```r
vars <- c("body-bg", "body-color", "primary", "border-radius")
bs_get_variables(bs_theme(), varnames = vars)

# Check contrast (for accessibility)
bs_get_contrast(bs_theme(), c("primary", "dark", "light"))
```

## Best Practices

1. **Prefer `bs_theme()` over custom CSS** -- variables cascade to all related components automatically
2. **Pin Bootstrap version**: `bs_theme(version = 5)` prevents breakage if defaults change
3. **Use fallback fonts** with `font_collection()` to avoid FOIT on slow connections
4. **Test across components**: inputs, buttons, cards, navs, plots, tables, modals, toasts, mobile
5. **Check accessibility** with `bs_get_contrast()` and browser dev tools
6. **Organize complex themes** in a separate `theme.R`:

```r
# theme.R
app_theme <- function() {
  bs_theme(
    version = 5,
    primary = "#2c3e50",
    base_font = font_google("Lato"),
    heading_font = font_google("Montserrat", wght = c(400, 700))
  ) |>
    bs_add_rules(sass::sass_file("www/custom.scss"))
}
```
