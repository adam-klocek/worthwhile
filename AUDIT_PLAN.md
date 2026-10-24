# Worthwhile — audit and implementation plan

Audit completed before changing application code, against the uploaded ZIP on 2026-10-23.

## Existing architecture and reusable features

`app.R` is the Shiny entry point. It sources pure models and namespaced modules. Dependencies are Shiny and jsonlite only. `R/model.R` owns quarter-hour capacity, interval union, priority scoring, dependencies, greedy weekly scheduling, salary/hourly/piecework economics, goals and opportunity assumptions. `R/history.R` handles schema 1→2 migration, actual-time logs, completion, removed history and JSON saving. `R/analytics.R` computes verified-completion summaries. Modules cover tasks, recurring calendar, jobs, goals, scenarios, insights and a five-step setup. JSON backups and a single-user local save preserve current and removed records. Existing regression tests cover the above.

Partial logging already creates distinct sessions, allows overruns, and does not complete the task. Completion adds only the difference from an entered cumulative actual total. These functions will be extended rather than replaced. Completed tasks and removal history will remain reportable.

## Gaps and risks found

- Partial logging currently subtracts logged hours from remaining work when no revised estimate is supplied. This incorrectly couples two independent quantities.
- Sessions lack notes, timestamps and task-specific history; Start/Resume/Pause context is missing.
- The default surface exposes management controls before the daily decision, and navigation has eight competing destinations.
- The stylesheet has accumulated overlapping overrides and repeated colours; type and contrast are inconsistent.
- Recurring capacity has no dated exceptions or protected manual work blocks. Replanning cannot be previewed before application.
- Scenario planning covers opportunity odds but not alternative weekly allocations.
- Saves use a temporary file but have no previous-good backup, migration snapshot or stale-session guard.
- Completing a task after partial logging must continue to avoid duplicate hours; unknown historical actuals must remain unknown.
- Statistical feedback needs coverage and sample sizes, not a productivity score or causal claims.

## Implementation sequence

1. Keep existing models/modules and add schema 3 migration for session metadata, focus context, dated exceptions and manual locks.
2. Extend manual work logging, per-task session editing/deletion and resume context. Start/Pause controls are explicit focus bookmarks, not an unreliable elapsed-time timer.
3. Add a planning overlay around the existing scheduler: dated availability changes, fixed interruptions and locked work; preview and explicitly apply changes.
4. Build Today / Plan / Decide / Reflect with contextual access to all existing modules and the setup/data tools.
5. Add inspectable ranking factors, competing-task comparisons and a sandboxed trade-off simulation; retain the existing opportunity model.
6. Replace the visual layer with semantic Modern Intellect tokens, local Inter/Lora when available, accessible controls and responsive layouts.
7. Run existing and new model, Shiny interaction, migration, restart, persistence, scenario and navigation tests. Attempt browser visual inspection where supported and report limits candidly.

## Data protection

Work occurs in a separate `worthwhile` checkout extracted from the attachment. Uploaded files and any real local user save are not modified. Legacy JSON is migrated on read, saved values are preserved, and snapshots precede writes. An existing legacy save path is retained when present; a Worthwhile default and environment variable are added with the old variable accepted. No cloud, database, authentication or AI service is introduced.
