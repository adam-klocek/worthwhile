# Worthwhile 0.6 — validation report

Executed on 2026-10-23. R/Shiny execution used R 4.6.0 / WebR 0.6.0, Shiny 1.13.0.8000 and jsonlite 2.0.0. PostgreSQL isolation checks used PGlite's actual PostgreSQL engine with a minimal Auth fixture. Neither WebR nor PGlite is an app runtime dependency.

## Executed checks

- Every R source parses and the app builds. The existing **1,157 model checks** pass, including all eleven strategies, capacity, overnight time, deadlines, dependencies, FTE, finances, goals and JSON round trips.
- Existing Shiny suites pass: task creation/edit/review, partial/final actuals, goal credit, Undo, self-employment, recurring/overnight commitments, calendar views, setup, deletion/archive/purge and real-file restart. Test fixtures now clear the new table when constructing an intentionally empty plan, and distinguish legacy save-path tests from the new environment variable.
- The **27 named Worthwhile checks** pass, including independent sessions, corrections, confirmation, overruns, manual locks, dated replanning, stale previews, sandbox application and restart persistence.
- Design checks pass: generated tokens match R/design.R, the versioned theme follows Bootstrap, four main landmarks and the skip link are present, and quieter task actions render. Fourteen text pairs exceed 4.5:1; five control/focus pairs exceed 3:1. JavaScript syntax and CSS parsing pass.
- Opportunity/client checks pass for exact two-outcome expected value, preparation time cost, negative/loss outcomes, schema-3 migration, role/goal/task linkage, idempotent goal creation, actual versus remaining time, deletion/unlinking, and non-billable preparation in an hourly role.
- Real Shiny inputs compare several opportunities, create a goal/task and open the goal editor, adjust the goal, avoid duplicate goal creation, delete an opportunity without deleting its work, and expose follow-through in Reflect.
- Mock HTTP checks pass for verified user IDs, separate authenticated clients, user-scoped query URLs and JWTs, private saves/loads, token rotation, conflict errors and logout. The signed-out cloud UI does not expose the planner. A verified sign-in mounts the real planner; a task edit saves only that account's row. Missing server configuration remains closed.
- Exact author and funding strings are present in the normal UI. The sign-in UI uses the same credit footer.

## SQL checks actually executed

The real PostgreSQL engine executed:

1. `supabase/tests/000_auth_fixture.sql` in a disposable test database only.
2. The exact delivered `supabase/migrations/001_private_plans.sql`.
3. `supabase/tests/002_isolation.sql`.

Assertions passed for own-row reads, A/B read isolation, user-scoped RPC writes, stale revision rejection, denied direct writes, previous-good payload preservation, rejection of anonymous reads and RPC calls, and immunity to a different user ID placed inside a JSON payload. The tests simulate the request Auth claim; they do not test Supabase's live token-verification service or email delivery. GitHub CI repeats the SQL suite using a disposable PostgreSQL 16 service.

## Deployment preparation

`renv.lock` records 37 CRAN packages, including Shiny, jsonlite and httr2, for target R 4.5.1. Records and dependency closure were resolved from repository metadata. This environment did not perform a native restore of that lockfile; the included GitHub workflow runs that restore and the app suites when pushed. No GitHub/Posit account was connected and no hosted application was published.

Credentials and local data are excluded by the repository and deployment ignore files. Cloud mode uses a publishable key plus a user JWT; no service-role or secret key is required. No Supabase project credentials were provided, so the live acceptance test in DEPLOYMENT.md remains necessary.

## Reproduce

From the repository root in R:

```r
source("tools/install.R")
renv::load()
source("tests/run-all.R")
```

Optional local checks:

```bash
node tests/check-assets.js
node --check www/app.js
node --check www/auth.js
```

For SQL testing, use a disposable PostgreSQL database and execute the fixture, migration and isolation script in that order with `psql -v ON_ERROR_STOP=1`. Never execute the fixture in a real Supabase project.

## Remaining acceptance checks

Native R HTTP/browser interaction, keyboard/screen-reader traversal, live Supabase signup/email confirmation, token refresh against the owner's project and Posit-hosted restart behaviour remain unverified here. Chromium cannot start in this environment (SIGTRAP), so no browser screenshots are claimed. Offline previews export the actual UI and illustrative Shiny outputs; they are visual references, not a connected hosted service.

Live tests should use two accounts, separate browser sessions, a conflicting save, sign-out/restart, a local-backup import into the correct account, an opportunity-to-goal action, and task logging reflected back into the opportunity comparison. Full instructions are in DEPLOYMENT.md.
