---
name: shiny-bslib
description: Build modern Shiny dashboards and applications using bslib (Bootstrap 5). Use when creating new Shiny apps, modernizing legacy apps (fluidPage, fluidRow/column, tabsetPanel, wellPanel, shinythemes), or working with bslib page layouts, grid systems, cards, value boxes, navigation, sidebars, filling layouts, theming, accordions, tooltips, popovers, toasts, or bslib inputs. Assumes familiarity with basic Shiny.
metadata:
  author: Garrick Aden-Buie (@gadenbuie)
  version: "1.0"
  source: https://github.com/posit-dev/skills/blob/main/shiny/shiny-bslib/SKILL.md
license: MIT
---

# Modern Shiny Apps with bslib

Build professional Shiny dashboards using bslib's Bootstrap 5 components and layouts. This skill focuses on modern UI/UX patterns that replace legacy Shiny approaches.

## Quick Start

**Single-page dashboard:**
```r
library(shiny)
library(bslib)

ui <- page_sidebar(
  title = "My Dashboard",
  theme = bs_theme(version = 5),  # "shiny" preset by default
  sidebar = sidebar(
    selectInput("variable", "Variable", choices = names(mtcars))
  ),
  layout_column_wrap(
    width = 1/3,
    fill = FALSE,
    value_box(title = "Users", value = "1,234", theme = "primary"),
    value_box(title = "Revenue", value = "$56K", theme = "success"),
    value_box(title = "Growth", value = "+18%", theme = "info")
  ),
  card(
    full_screen = TRUE,
    card_header("Plot"),
    plotOutput("plot")
  )
)

server <- function(input, output, session) {
  output$plot <- renderPlot({
    hist(mtcars[[input$variable]], main = input$variable)
  })
}

shinyApp(ui, server)
```

**Multi-page dashboard:**
```r
ui <- page_navbar(
  title = "Analytics Platform",
  theme = bs_theme(version = 5),
  nav_panel("Overview", overview_ui),
  nav_panel("Analysis", analysis_ui),
  nav_panel("Reports", reports_ui)
)
```

## Core Concepts

### Page Layouts

- **`page_sidebar()`** -- Single-page dashboard with sidebar (most common)
- **`page_navbar()`** -- Multi-page app with top navigation bar
- **`page_fillable()`** -- Viewport-filling layout for custom arrangements
- **`page_fluid()`** -- Scrolling layout for long-form content

### Grid Systems

- **`layout_column_wrap()`** -- Uniform grid with auto-wrapping (recommended for most cases)
- **`layout_columns()`** -- 12-column Bootstrap grid with precise control

### Cards

Primary container for dashboard content. Support headers, footers, multiple body sections, and full-screen expansion.

### Value Boxes

Display key metrics and KPIs with optional icons, sparklines, and built-in theming.

### Navigation

- **Page-level**: `page_navbar()` for multi-page apps
- **Component-level**: `navset_card_underline()`, `navset_tab()`, `navset_pill()` for tabbed content

### Sidebars

- **Page-level**: `page_sidebar()` or `page_navbar(sidebar = ...)`
- **Component-level**: `layout_sidebar()` within cards
- Supports conditional content, dynamic open/close, accordions

### Theming

- **`bs_theme()`** with Bootswatch themes for quick styling
- **Custom colors**: `bg`, `fg`, `primary` affect hundreds of CSS rules
- **Fonts**: `font_google()` for typography
- **Dynamic theming**: `input_dark_mode()` + `session$setCurrentTheme()`

### Icons

**Recommended: `bsicons` package** (Bootstrap Icons, designed for bslib):
```r
bsicons::bs_icon("graph-up")
bsicons::bs_icon("people", size = "2em")
```

Browse icons: https://icons.getbootstrap.com/

**Accessibility for icon-only triggers:** When an icon is used as the sole trigger for a tooltip, popover, or similar interactive element (no accompanying text), it must be accessible to screen readers. By default, icon packages mark icons as decorative (`aria-hidden="true"`), which hides them from assistive technology.

- **`bsicons::bs_icon()`**: Provide `title` — this automatically sets `a11y = "sem"`
  ```r
  tooltip(
    bs_icon("info-circle", title = "More information"),
    "Tooltip content here"
  )
  ```

### Special Inputs

- **`input_switch()`** -- Toggle switch (modern checkbox alternative)
- **`input_dark_mode()`** -- Dark mode toggle
- **`input_task_button()`** -- Button for long-running operations
- **`input_code_editor()`** -- Code editor with syntax highlighting
- **`input_submit_textarea()`** -- Textarea with explicit submission

## Common Workflows

### Building a Dashboard

1. Choose page layout: `page_sidebar()` (single-page) or `page_navbar()` (multi-page)
2. Add theme with `bs_theme()` (consider Bootswatch for quick start)
3. Create sidebar with inputs for filtering/controls
4. Add value boxes at top for key metrics (set `fill = FALSE` on container)
5. Arrange cards with `layout_column_wrap()` or `layout_columns()`
6. Enable `full_screen = TRUE` on all visualization cards
7. Add `thematic::thematic_shiny()` for plot theming

### Modernizing an Existing App

Key steps:

1. Replace `fluidPage()` with `page_sidebar()` or `page_navbar()`
2. Replace `fluidRow()`/`column()` with `layout_columns()`
3. Wrap outputs in `card(full_screen = TRUE)`
4. Add `theme = bs_theme(version = 5)`
5. Convert key metrics to `value_box()` components
6. Replace `tabsetPanel()` with `navset_card_underline()`

## Guidelines

1. **Prefer bslib page functions** (`page_sidebar()`, `page_navbar()`, `page_fillable()`, `page_fluid()`) over legacy equivalents (`fluidPage()`, `navbarPage()`)
2. **Use `layout_column_wrap()` or `layout_columns()`** for grid layouts instead of `fluidRow()`/`column()`, which don't support filling layouts
3. **Wrap outputs in `card(full_screen = TRUE)`** when building dashboards -- full-screen expansion is a high-value feature
4. **Set `fill = FALSE`** on `layout_column_wrap()` containers holding value boxes (they shouldn't stretch to fill height)
5. **Pin Bootstrap version**: include `theme = bs_theme(version = 5)` or a preset theme
6. **Use `thematic::thematic_shiny()`** in the server so base R and ggplot2 plots match the app theme
7. **Use responsive widths** like `width = "250px"` in `layout_column_wrap()` for auto-adjusting columns
8. **Group sidebar inputs** with `accordion()` when sidebars have many controls

## Avoid Common Errors

1. Avoid directly nesting `card()` containers. `navset_card_*()` functions are already cards; `nav_panel()` content goes directly inside them without wrapping in `card()`
2. Only use `layout_columns()` and `layout_column_wrap()` for laying out multiple elements. Single children should be passed directly to their container functions.
3. Never nest `page_*()` functions. Only use one top-level page function per app.
