# Worthwhile 0.6 — implementation summary

The Modern Intellect app and its four primary experiences remain in place. This update adds hosted accounts, deployment preparation and a connected uncertainty planner without replacing the scheduling engine or existing workflows.

## Supabase accounts and storage

- Default hosted mode shows the sign-in screen until the server configuration and user credentials are verified. Personal `WORTHWHILE_MODE=local` remains available explicitly.
- Supabase email/password sign-up, email confirmation, sign-in, access-token refresh and sign-out are implemented through REST using httr2. A returned session is checked against `/auth/v1/user` before a private plan is loaded. Only a publishable project key is used.
- Each Shiny connection holds its own client and tokens in server memory. Tokens and passwords are absent from JSON plans and browser localStorage. Unused email-confirmation hash credentials are discarded. Refreshing or signing out requires a fresh sign-in.
- The application never reads a shared local plan or falls back to a shared file in cloud mode. New accounts start blank and use the existing guide. Import of an old local backup is an explicit operation by the signed-in user.
- `public.worthwhile_plans` stores one complete JSON plan per Supabase Auth user. Owner-only RLS restricts reads. Direct client writes are revoked; a narrow SQL function writes only `auth.uid()`'s row, locks it, checks the expected revision and keeps the previous payload. It ignores any user ID inside the JSON payload.
- Save failures preserve the current reactive state. A stale session is rejected rather than overwriting newer data. Cloud Undo is another revision-checked save of the prior state. Independent JSON export remains available.

## A connected opportunity planner

The old isolated single-calculator interface is replaced by saved, editable Grant / Book / Other opportunities associated with work roles. Each stores a stage, deadline, low/central/high success chances, personal proceeds for success and failure, upfront and outcome-specific costs, timing, discount rate, alternative hourly value, notes and assumption basis. Total grant/team funding is context only.

Comparisons show discounted expected cash, economic value after the value of preparation time, economic value per remaining hour, conservative/optimistic assumptions, nominal cash-loss probability and worst entered cash loss. Several sort lenses and a risk/return plot expose the trade-offs. Negative value and loss-only outcomes are valid. Overlapping/exclusive groups are flagged; the app does not add speculative awards into a portfolio income total or guaranteed task cash.

**Create / open goal** creates a preparation goal and non-billable task in one action, saves the link and opens the existing goal editor for adjustment. Repeating the action opens the same goal. The linked task breakdown supplies the remaining effort; session history supplies actual effort. Unfinished deleted work does not automatically imply preparation is free. Finishing a preparation goal and recording a Won/Lost outcome are separate events.

Reflect adds an optional follow-through table with preparation effort, goal progress and user-entered outcome stages. Normal opportunity deletion retains backup history and leaves goals/tasks intact. Reflect can purge an individual removed opportunity. Deleting a work role archives its opportunities; deleting a goal unlinks it safely.

## Financial compatibility

Schema 4 adds a task `billable` flag. Existing tasks migrate with `TRUE`, preserving prior hourly/piecework behaviour. Preparation tasks generated from opportunities use `FALSE`, so an hourly role cannot turn speculative work into guaranteed rate-based income. Guaranteed task bonuses remain separate and editable. The existing salary, expense, paid-unit and priority calculations otherwise retain their behaviour. Conditional opportunity cash is never copied to task bonuses or injected into Cash now ranking.

## Attribution

The sign-in screen and normal app footer name **Mgr. Adam Klocek, Ph.D.** and display exactly:

Funded by the project Mediated Society (MEDIS:ON), registration number: CZ.02.01.01/00/23_025/0008713, supported by the European Union and Czech Republic.

The same attribution appears in README.md and NOTICE.md. CITATION.cff supplies GitHub citation metadata.

## Main file changes

| Files | Purpose |
| --- | --- |
| `app.R` | Auth/local entry selection; existing planner with injected cloud storage; goal navigation; account controls and credits. |
| `R/supabase.R`, `R/mod_auth.R`, `www/auth.js` | Session-scoped Auth/REST client, verified identity, refresh, private load/save, sign-in UI and logout. |
| `supabase/migrations/001_private_plans.sql` | RLS, restricted grants, previous payload and transactional optimistic saves. |
| `R/opportunities.R`, `R/mod_opportunities.R` | Persisted opportunity model, comparisons, risk arithmetic, CRUD and goal/task creation. |
| `R/mod_decide.R`, `R/mod_goals.R`, `R/mod_insights.R` | Integration with Decide, goal editor and Reflect. |
| `R/history.R`, `R/demo.R`, `R/setup_model.R`, `R/ui_helpers.R`, `R/model.R`, `R/mod_tasks.R` | Schema 4, archive/reference handling, backup decoding, examples and non-billable preparation. |
| `R/data_ui.R`, `R/design.R`, `www/style.css` | Mode-specific storage help, release version and small auth/footer layout additions. Existing design tokens/fonts remain. |
| `renv.lock`, `.github/workflows/checks.yml`, `.Renviron.example`, `.gitignore`, `.rscignore`, `tools/`, `.Rproj` | Dependency resolution, CI, credential/data exclusions and deployment preparation. |
| `tests/`, `supabase/tests/` | Existing regressions plus opportunity, cloud-client, UI gate, authenticated save and SQL isolation tests. |
| `DEPLOYMENT.md`, `README.md`, `MODEL_REFERENCE.md`, `VALIDATION.md`, `NOTICE.md`, `CITATION.cff` | Setup, formulas, test evidence and credit. |

## Migration

Schemas 1, 2 and 3 migrate to schema 4. Missing opportunity tables become empty in current and archive data. Existing task pay remains enabled; older missing notes/timestamps and unknown actual totals retain their prior migration behaviour. IDs, estimates, actuals, goals, roles and logs are preserved. Local saves keep pre-migration snapshots; cloud writes retain the previous payload. The example adds two grant options and a book with explicitly illustrative assumptions.

## Deliberate limits

No live project, GitHub repository or Posit deployment was created without account configuration. Native lockfile installation, hosted email delivery and live Supabase/Auth integration still need the owner's acceptance test. REST client tests use mock HTTP; SQL policies were executed in a real PostgreSQL engine with a test Auth fixture. Password recovery/OAuth/MFA screens, joint correlated-outcome portfolios, an income ledger and statistical grant-probability estimation remain future work. Opportunity forecasts are user assumptions. Existing manual time sessions, focus bookmarks, calendar views, locks, replanning, priority strategies and data tools remain available.
