# Worthwhile

**Make better decisions about what deserves your time.**

Worthwhile 0.6 is an R Shiny app with private Supabase accounts and an optional personal local mode for balancing several work roles, fixed commitments, income, personal meaning and longer-term goals. It builds on the supplied application, preserving its scheduling engine and financial models.

## GitHub and Posit deployment

See **DEPLOYMENT.md** for the complete path: upload this folder's contents to GitHub, run `supabase/migrations/001_private_plans.sql` in your project, and publish `app.R` with the included `renv.lock` on Posit Connect Cloud. Set the project URL, publishable key and app mode in the host's Variables settings. The default Supabase mode keeps the planner behind sign-in; incomplete configuration does not expose a shared local plan.

Author: **Mgr. Adam Klocek, Ph.D.**

Funded by the project Mediated Society (MEDIS:ON), registration number: CZ.02.01.01/00/23_025/0008713, supported by the European Union and Czech Republic.

## Install and launch

Install R 4.1 or later. Extract the ZIP, open R in the extracted `worthwhile` folder, and run:

```r
install.packages(c("shiny", "jsonlite", "httr2"))  # once
Sys.setenv(WORTHWHILE_MODE = "local")  # personal offline mode
shiny::runApp(".")
```

An internet connection is needed for initial package installation. Local mode needs no external service; Supabase mode uses your configured Supabase Auth and database project. Its usage follows that project’s plan. Its styles and Inter/Lora fonts are bundled locally. WebR and Node are not required to run the app. For locked hosting dependencies use `tools/install.R` and `renv::load()`; see DEPLOYMENT.md.

## Inspect the visual redesign first

Open `preview/index.html` (planner) or `preview/login.html` (sign-in screen) in your browser after extracting the ZIP. This offline visual reference uses the actual Shiny page markup, the same theme and bundled fonts, and illustrative outputs from the app. Its tabs and disclosures work; saving and task actions require the R app. It sends no data anywhere.

The Modern Intellect theme uses the requested ink, iris, lavender, clay and warm-paper palette; Lora headings; Inter controls; spacious cards; readable forms; and restrained status colours. Routine cards have no decorative priority badge. Extra task actions sit under **More**. A skip link and arrow/Home/End tab navigation support keyboard use.

To launch the redesigned app, stop any older running Shiny process and run the **complete newly extracted `worthwhile` folder**. Copying only `app.R` omits the modules, theme and fonts. The theme and JavaScript use versioned asset URLs to avoid a stale browser cache. Existing data remains compatible; keep a JSON backup before moving an existing save.

## Find your way

| Experience | Use it for |
| --- | --- |
| **Today** | Review the day, resume unfinished work, choose a next step, create/edit tasks, log time and complete work. |
| **Plan** | Calendar with Week/Day/Month views; dated replanning; editable work roles; goals; guided setup; backups and preferences. |
| **Decide** | Eleven priority lenses, task comparisons, a sandbox for alternative weeks, and saved comparisons of uncertain grants, books and other opportunities. |
| **Reflect** | Actual versus original estimates, role-level patterns, historical effort, goal progress and complete task history. |

The first launch opens Today with the labelled scientist example. Choose **Create my own plan** to follow five small steps: working rhythm, work roles, personal time and meetings, first tasks, then a capacity review. Each draft job/task/event must be added before continuing. The guide replaces the example only when you choose **Create my plan**. Later edits belong in the relevant task, role or calendar editor; the guide is for a fresh plan.

When reopening a saved plan, a task check-in appears so you can revisit nudges, deadlines, remaining work, collaborators and consequences. Close it when nothing needs changing, or open **Review tasks** at any time.

## Log work without pretending it is finished

Use **Log time** on a task card or **Resume work** to record an additional work session. Enter a date, actual duration, optional note and next action. A revised remaining estimate is optional.

- Each entry remains a separate work session, including several entries on the same date.
- Actual time and remaining effort are independent. Logging 4.25 hours on an 8-hour task leaves the remaining estimate at 8 hours until you revise it.
- **Start / Pause / Resume are persistent focus bookmarks, not a live stopwatch.** They never manufacture time entries. Pausing or closing the browser does not record elapsed time.
- **History** opens the task's sessions. Select an entry to correct hours, date or notes; deletion asks for confirmation. Reflect also permits corrections for removed tasks.
- **Finish** asks for the cumulative actual hours for the whole task. If 4.25 hours are already logged and you enter 6, it adds 1.75. It rejects a total below existing logs.
- The original estimate stays unchanged. Overruns are allowed. Completion is always explicit and remains in reports.

Task cards show Not started, In progress or Blocked; completed tasks are available in the task desk and Reflect. Today shows the latest session, total logged effort, remaining forecast, note and next action for in-progress tasks; further resume cards are under a disclosure after the first three. All tasks also remain available in the backlog/task selector.

## Replan when the day changes

In **Plan → Replan a day**, enter a one-off work window, unavailable period, or unexpected meeting. You can also revise a task's remaining effort or add a manual locked work block. Preview first: the comparison shows which tasks gain or lose planned hours. Apply only when the proposed change is useful.

Essential and protected commitments still constrain the plan. Manual locks remain in place and conflicts are reported; a conflicting preview cannot be applied. Existing locks and dated adjustments can be replaced or removed explicitly. An edit to the underlying data or planning context invalidates an old preview. Form changes require another Preview; Apply uses the displayed preview.

The weekly calendar is a draft of remaining work, not a time log. Logging earlier work does not subtract it again from your remaining forecast. Past dates and elapsed quarter hours are excluded from new automatic focus blocks. The month overview schedules the selected week only and shows deadlines for the whole month.

## Explore alternatives safely

**Decide → Trade-off simulator** copies the current plan. Try fewer usable focus hours, an additional project, or a different decision lens. Compare shifted hours and marginal cash before explicitly applying the scenario. An unapplied scenario never changes real tasks or commitments. Manual-lock conflicts and stale previews prevent application. Reductions are limited by the remaining available capacity.

Salary stays fixed and is not counted again per task. Hourly work, paid pages/rooms, guaranteed bonuses and entered expenses produce marginal estimates before tax. These are work-value estimates, not received income. **Future opportunities** compares several saved alternatives by success chances, expected cash, economic value per hour and potential cash loss. Each belongs to a work role. **Create / open goal** adds an editable preparation goal and non-billable starter task in one action, then opens the goal editor. Linked task estimates and actual logs feed back into the comparison. Proposal completion and funding decisions remain distinct. Enter personal proceeds separately from a whole team award; uncertain amounts never become guaranteed task bonuses. The probabilities and two outcome amounts are your assumptions, not predictions from publication counts. See MODEL_REFERENCE.md for the full formulas and limitations.

## Your records and backups

**Cloud mode:** sign in using Supabase email/password. Your complete plan saves to your own database row, with an atomic revision check. New accounts begin with a blank plan and guided setup; each person imports their own old JSON backup explicitly. Sign-out clears this connection, and refreshing requires sign-in again. No shared local file is read or written.

**Local mode:** changes save automatically to `data/worthwhile-state.json`. The folder is created on first save. An existing `data/portfolio-state.json` is reused if the new default does not exist. Set a different path before launch if desired:

```r
Sys.setenv(WORTHWHILE_STATE_FILE = "/your/writable/folder/worthwhile.json")
shiny::runApp(".")
```

`WORTHWHILE_STATE_FILE` takes precedence over the supported legacy `PORTFOLIO_STATE_FILE`. Set either applicable variable to `""` for session-only operation. Keep a JSON download from **Plan → Data & preferences** as an independent backup.

Schema 1, 2 and 3 backups migrate to schema 4 on read. The new tables store opportunities; old tasks default to their existing rate-based pay behaviour. New uncertain preparation tasks use a separate non-billable flag. Missing historical notes and timestamps stay blank; missing historical actual durations remain unknown. Before replacing an older on-disk save, Worthwhile keeps a `.schema1.backup.json` or `.schema2.backup.json` snapshot. Every replacement keeps `.previous.json` and uses a temporary file followed by rename. An unreadable old save is left intact and cannot be silently overwritten. A revision check rejects a stale session; this is not a multi-user transaction system.

Normal task/role deletion removes active work but preserves labelled report history. Deleting a prerequisite blocks dependent tasks for review. Completed goal credit survives normal task removal. Reflect has a separate confirmed purge for records you want removed from saved report history. **Undo** retains twenty recent data changes in this session and does not survive restart. Automatic preference synchronisation does not add extra Undo steps. Restoring a backup or resetting the dataset requires confirmation.

Local mode is for one person on their own computer. Shared deployments use Supabase mode, with verified per-user sessions, owner-only database reads and transactional user-scoped saves. See DEPLOYMENT.md for the SQL setup and live acceptance steps.

## Tests and implementation notes

From the application directory:

```r
source("tests/run-all.R")
```

Optional asset/contrast checks use standard Node.js: `node tests/check-assets.js`.

See IMPLEMENTATION.md for the change map and migration details, VALIDATION.md for the executed checks and environment limits, AUDIT_PLAN.md for the initial audit, and FUTURE.md for the short next-development list. Font licenses are in `www/fonts/`.
