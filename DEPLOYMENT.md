# Deploy Worthwhile 0.6

The source is prepared for GitHub and Posit Connect Cloud. No deployment or Supabase project has been created on your behalf. You supply the account/project configuration below; the app defaults to a closed sign-in screen until configured.

## 1. Put the app on GitHub

Extract the ZIP. Open the **worthwhile folder**, which contains `app.R`, `R/`, `www/`, `renv.lock`, `supabase/` and `.github/`. Upload this folder's **contents** to your repository root. Keep `app.R` and `renv.lock` next to each other.

With the Git command line, after creating an empty repository on GitHub:

```bash
git init
git add .
git commit -m "Prepare Worthwhile with private accounts and opportunity planning"
git branch -M main
git remote add origin https://github.com/YOUR-USERNAME/YOUR-REPOSITORY.git
git push -u origin main
```

`.gitignore` excludes local plans, downloaded backup patterns, `.Renviron`, package libraries and publishing credentials. `preview/` is an illustrative offline reference and is excluded from Git; it is unnecessary for hosting. Do not force-add private plans or credentials. `.Renviron.example` is a template, not a credential file.

The included workflow runs R/Shiny checks and a separate PostgreSQL isolation test on pushes and pull requests. It does not deploy or access your real Supabase project.

## 2. Create the Supabase backend

1. Create a Supabase project you control.
2. Open its **SQL Editor** and run **only** `supabase/migrations/001_private_plans.sql`. It creates one private plan per authenticated user, an owner-only read policy, and an atomic save function. Do not run files under `supabase/tests/` in a real project; they are disposable test fixtures.
3. Enable email/password authentication in Supabase Auth. Email confirmation is supported: users confirm the email, return to Worthwhile and sign in with their password. Configure an appropriate minimum password length and signup/SMTP settings for your audience.
4. Copy your project HTTPS URL and its **publishable key** (`sb_publishable_...`). Worthwhile intentionally rejects secret/service-role keys. Those keys are not required.
5. Once the hosted URL is known, set Supabase Auth's **Site URL** to that URL and allow the intended confirmation redirect. The app discards unused implicit-session hash credentials and still requires server-verified sign-in.

Database model: `public.worthwhile_plans` holds the current JSON plan, revision, previous payload and update timestamp. It preserves the app's existing data shape. Ordinary clients have no direct table write permission. `save_worthwhile_plan` derives ownership from `auth.uid()` rather than any user ID supplied in a form or JSON payload. Every save checks and advances the revision under a row lock. A stale tab cannot overwrite a newer plan.

## 3. Test from your computer or a Posit Cloud project

Posit Cloud's RStudio workspace can clone the GitHub repository and run/test the code. Posit **Connect Cloud** is the publishing route described in the next section.

Open the project root in RStudio or a Posit Cloud project, then:

```r
source("tools/install.R")
renv::load()
```

The included lockfile lists CRAN dependencies and targets R 4.5.1. The running app needs Shiny, jsonlite and httr2; the dependency tree is recorded for reproducible hosting. The lockfile was resolved from repository metadata; its native installation is checked by the included GitHub workflow when you push it.

For a personal offline session:

```r
Sys.setenv(WORTHWHILE_MODE = "local")
shiny::runApp(".")
```

For a cloud-connected test, copy `.Renviron.example` to `.Renviron`, replace the placeholders, and restart R. This is the configuration:

```text
WORTHWHILE_MODE=supabase
SUPABASE_URL=https://YOUR-PROJECT.supabase.co
SUPABASE_PUBLISHABLE_KEY=sb_publishable_YOUR_KEY
WORTHWHILE_ALLOW_SIGNUP=true
```

Then `renv::load(); shiny::runApp(".")`. No secret key, database password or user password belongs in the repository. The publishable key identifies the project; each user's JWT separately controls database access.

## 4. Publish on Posit Connect Cloud from GitHub

1. Sign in to **Posit Connect Cloud** and choose **Publish → Shiny**.
2. Connect the GitHub repository and intended branch, usually `main`.
3. Select `app.R` as the primary file. The included `renv.lock` supplies the R dependencies; do not add an unrelated `manifest.json` that overrides it.
4. Under the content's **Variables** settings, add the four variables above. The app must use `WORTHWHILE_MODE=supabase` for a shared/public deployment.
5. Publish or restart after saving variables. A missing/invalid backend configuration shows a setup message and does not open the planner.
6. Set the final URL in Supabase Auth as described above. Create two test accounts and perform the live acceptance steps below before sharing the link.

Repository visibility and publishing features depend on your Connect Cloud account. Choose sharing settings appropriate to the people who should use the app. Supabase login controls individual plans within the app.

`tools/deploy.R` offers optional R-console publishing through rsconnect instead. Set the same Variables in Connect Cloud afterwards; they are not bundled in the source. The optional rsconnect package is a publishing tool, not a runtime dependency.

For a managed Posit Connect/Shiny Server installation, deploy the same app and set the environment variables for the R process. Use HTTPS. The database remains Supabase; no shared local JSON file should be used on a multi-user server.

## 5. Move an existing local plan into your own account

1. In your existing local app, download the complete JSON backup.
2. Sign into **your own** account on the hosted app.
3. Use **Plan → Data & preferences → Restore a JSON backup**, review the replacement prompt and confirm.

Schemas 1–3 migrate to schema 4. Older tasks keep their existing pay behaviour; new uncertain preparation tasks are non-billable by default. The app never silently imports a server's local file into another account. Each user explicitly imports their own backup.

## 6. Live acceptance test

- Account A creates a role/task/opportunity and logs time; sign out.
- Account B sees its own fresh plan and cannot see A's data. Add a different task.
- Return to A; its original plan and notes remain after an app restart.
- Open A in two browser sessions. Save in one; the stale second session must report a revision conflict without applying the rejected change. Copy any edited form fields, optionally export the existing session state, then reload/sign in and reconcile with the saved plan. Do not blindly restore an older whole-plan backup over newer work.
- Compare two grants and a cost-bearing book. Create a goal from an opportunity, adjust the goal, and log preparation time in Today. Verify the opportunity's remaining/actual hours update.
- Download a backup; restore it into the same account after reviewing the replacement prompt.

## Scope and operations

Email/password signup, confirmation, sign-in, access-token refresh and sign-out are implemented. Tokens stay in that Shiny connection's server memory; they are not stored in the plan or localStorage. Refreshing the browser starts a new sign-in. Each browser connection signs in independently; signing out ends that connection, not other devices. Password recovery, OAuth, MFA and administrative account deletion screens are not implemented; configure the account administration/recovery process before a wider release.

No real Supabase credentials were supplied here, so your project's live Auth configuration, email delivery, hosted HTTPS session and Connect Cloud deployment still need the acceptance test. Automated client tests use mock HTTP. SQL access-control tests execute the migration and policies in a real PostgreSQL engine with a minimal Auth fixture, not a real Supabase project.

Cloud mode never falls back to writing a shared local plan. Failed changes are not applied; the existing reactive state stays intact. Copy rejected form edits separately before reloading. A downloaded backup contains the accepted session state, not a failed form submission. The database keeps one previous payload; project backup retention is configured separately in Supabase. Local mode remains single-person storage, with its existing previous-good and migration backups.

## Official references

- https://docs.posit.co/connect-cloud/how-to/r/shiny-r.html
- https://docs.posit.co/connect-cloud/how-to/r/dependencies.html
- https://rstudio.github.io/rsconnect/reference/deployApp.html
- https://supabase.com/docs/guides/auth/passwords
- https://supabase.com/docs/guides/getting-started/api-keys
- https://supabase.com/docs/guides/database/postgres/row-level-security
