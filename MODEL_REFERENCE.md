# Worthwhile — model reference

## Five-job demo

All pay, rates, expenses and task-duration estimates are fictional editable examples. University requirements come from the requested use case.

| Job | Pay | Contract / demo goals |
| --- | --- | --- |
| University A | 450 EUR/week salary | 0.5 FTE: Teaching 0.3, Research 0.2. One first-author paper/year, two guaranteed courses/semester, five supervised theses/semester. Extra five theses link to a separate promotion-related goal and add no salary. |
| University B | 550 EUR/week salary | 0.5 FTE research. One first-author paper and two coauthored papers/year; grant application; outreach talk with an explicit example bonus. |
| English-to-German books | 12 EUR/page | Per-page work, with a book-length goal and estimated business expenses. |
| Room cleaning | 25 EUR/room | Per-room work; travel is included in the task estimate. |
| Development | 45 EUR/hour | Urgent sprint, small update and longer-term project. Hours can be concentrated in available windows; no weekly billing quota is assumed. |

The demo needs more effort than its protected week supports. It is deliberately an overload example.

## Scheduling and ranking

Times are recurring local wall-clock times on a 15-minute grid. Add several availability windows for split shifts. An interval ending before its start crosses midnight; Sunday wraps into Monday. Start and end must differ. Overlapping blocks consume time once. Sleep remains essential even when night availability is added.

The buffer and daily cap reserve the end of each day's otherwise free capacity. The planner excludes past dates and elapsed quarter hours on the current local date. It does not model daylight-saving transitions as 23/25-hour days, travel across time zones or exact deadline times. Dated one-off availability and interruptions are supported in Plan → Replan a day; split overnight dated exceptions across two dates. The full calendar shows recurring commitments; the focus plan covers only the remaining days of the selected week.

Every strategy uses these guardrails:

- Consequence 5/5 or a fixed deadline within two days: first tier.
- Other fixed deadlines within seven days: second tier.
- Everything else: third tier.
- Unfinished prerequisites: blocked regardless of score. Even if the prerequisite is planned, dependent work waits until it is actually finished.

This makes random and quick-win modes safer, but means they are not unrestricted random/cash sorts. Already overdue fixed tasks are surfaced for renegotiation, not silently scheduled late. A flexible task can use an explicitly agreed extension and remains eligible if overdue.

Within each tier, the lens controls ordering. Balanced weights are urgency 25%, consequences 20%, marginal cash 15%, contract balance 10%, personal meaning 15%, career 10%, social impact 5%. Urgency combines deadline proximity with capped nudge count. Cash scores normalize positive net marginal income/hour against the current backlog; they change when the backlog changes. Quick wins use inverse remaining duration. Random uses a reproducible seed and can be reshuffled.

Contract balance compares actual logged hours and recurring meetings with job targets. In the weekly scheduler it also counts already planned focus blocks, then updates the next selection. Named contract areas contribute half the balance score for matching tasks. Area names must match exactly. Meetings count toward the job; their teaching/research split is not inferred. Targets are soft preference targets, not enforced quotas. Scheduled work is never treated as actual work.

The greedy scheduler assigns tasks in priority order into usable contiguous windows, ordinarily using blocks up to two hours, with a larger block when a task's stated minimum requires it. It honors minimum blocks and never schedules more than remaining effort. Contract balance updates as it fills the week. It is **not a global income or multi-objective optimizer**.

One big + small first looks for a task with at least two hours remaining, then limits subsequent selections that day to tasks with at most one remaining hour. It may leave deliberate gaps. All strategies can be overridden by editing estimates, priorities and commitments; Plan → Replan a day supports explicit locked work blocks; drag-and-drop is not implemented.

## Money calculations

All jobs use the same currency; there is no currency conversion or tax engine.

- Hourly remaining gross = rate × remaining hours + guaranteed task bonus.
- Piecework remaining gross = rate × remaining paid units + guaranteed task bonus.
- Salaried task marginal gross = guaranteed task bonus only.
- Estimated net before tax = marginal gross × (1 − business expense %) − remaining direct task expenses.
- Marginal net/hour = estimated net / remaining task hours.
- Salary reference/hour = weekly salary / (FTE × full-time hours), or the personal weekly target when FTE is zero. It is a reference, never added to marginal cash.
- Weekly task marginal value = task's remaining net value × fraction of remaining effort scheduled this week.

Salary is shown once as fixed weekly gross income, separately from net marginal task values. Weekly task values are proportional work estimates, **not guaranteed cash receipts**. A completed piecework task might pay only after acceptance. Work logs do not automatically reduce remaining hours or remaining paid pages/rooms: edit those units when partially delivered or split the job into payable batches. Actual payments, accrued bonuses, invoices and cash timing are not tracked. Completion confirms the total actual hours and adds only previously unlogged time. Partial work can be logged before completion; actual work hours are effort records rather than verified billable receipts. Reopening retains actual-time entries and restores the current overall estimate as remaining work; revise that number for a correction. The original estimate stays unchanged.

## Comparing uncertain opportunities

Each opportunity belongs to a work role and stores a low, central and high success chance; personal income in success and failure; remaining upfront cash cost; extra outcome-specific costs; a time horizon and discount rate; remaining preparation effort; and your alternative hourly value. A total grant/team budget is context only. It is never added to personal income.

For success probability `p`, discount factor `D = (1 + annual_discount)^years`, upfront cost `C`, personal income `I` and conditional costs `K`:

```
win_cash  = (success_income - success_cost) / D - C
lose_cash = (failure_income - failure_cost) / D - C
expected_cash = p * win_cash + (1 - p) * lose_cash
economic_value = expected_cash - remaining_hours * alternative_hourly_value
value_per_hour = economic_value / remaining_hours
nominal_win = success_income - success_cost - C
nominal_lose = failure_income - failure_cost - C
cash_loss_probability = p * (nominal_win < 0) + (1 - p) * (nominal_lose < 0)
maximum_cash_loss = max(0, -min(nominal_win, nominal_lose))
break_even_chance = (remaining_hours * alternative_hourly_value - lose_cash) /
                    (win_cash - lose_cash)
```

The break-even threshold applies when success improves the entered cash outcome. If it does not, the threshold is not applicable. Values below 0 or above 100% are meaningful bounds rather than probabilities to clamp silently. Zero remaining hours has no value-per-hour result. Finishing linked preparation does not mark the opportunity Won.

Upfront cost is paid now; conditional proceeds/costs are discounted to the present. All amounts are in the app's single currency before tax. Role expense percentages are not automatically applied again to this separate forecast: enter the relevant remaining cash costs explicitly. Review costs after they are incurred; the module does not infer a payment ledger from work logs.

The low/high range changes the probability while holding the two conditional cash outcomes fixed. It is not a statistical confidence interval. A book is represented by a simplified success/unsuccessful outcome; real royalties and returns can have a wider distribution. Negative returns and costs without income are valid. No empirical probability of an individual's wealth or grant success is inferred.

Create/open goal adds a preparation goal and a non-billable task linked to it, then opens the existing goal editor. Task completion credits the goal; it does not imply a funding award. Linked tasks become the remaining-effort forecast and logs become actual preparation time. A goal without an unfinished breakdown and still below its target falls back to the opportunity's entered effort forecast, so deleting unfinished work does not imply free preparation. Goals, opportunity details and outcome status remain editable.

Non-billable preparation does not inherit an hourly/piecework rate. Conditional income is never copied into task bonuses or the Cash now ranking. If genuinely guaranteed paid work exists, edit its billable flag or record a separately guaranteed bonus. Ordinary existing tasks preserve their previous pay behaviour.

Compare alternatives by chance, cash, time-adjusted value, conservative assumptions and downside. Mutually exclusive/overlapping groups are explicitly flagged and no portfolio sum of expected income is displayed. Do not add several rows aimed at the same eventual award or income stream. Shared draft effort and correlated outcomes require further modelling; the current module compares alternatives transparently.

The old pure `opportunity_scenario()` odds-uplift function is retained for backward compatibility; the user-facing planner now uses the stored conditional-outcome model above.
