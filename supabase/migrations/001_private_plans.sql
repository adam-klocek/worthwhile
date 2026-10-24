-- Run once in your Supabase SQL Editor as the project owner.
-- Idempotent: does not drop existing user data.
begin;
create table if not exists public.worthwhile_plans (
 user_id uuid primary key references auth.users(id) on delete cascade,
 revision bigint not null default 0 check(revision>=0),
 payload jsonb not null default '{}'::jsonb check(jsonb_typeof(payload)='object'),
 previous_payload jsonb,
 updated_at timestamptz not null default now()
);
alter table public.worthwhile_plans enable row level security;
alter table public.worthwhile_plans force row level security;
revoke all on public.worthwhile_plans from public,anon,authenticated;
grant usage on schema public to authenticated;
grant select on public.worthwhile_plans to authenticated;
drop policy if exists own_plan_read on public.worthwhile_plans;
create policy own_plan_read on public.worthwhile_plans for select to authenticated
 using ((select auth.uid())=user_id);
-- Writes are allowed only through this narrowly scoped transaction, not direct DML.
-- The owner executes SQL, but auth.uid() still comes from the verified request JWT.
create or replace function public.save_worthwhile_plan(p_payload jsonb,p_expected_revision bigint)
returns bigint language plpgsql security definer set search_path=''
as $$
declare uid uuid:=auth.uid(); current_revision bigint;
begin
 if uid is null then raise exception 'Authentication required' using errcode='42501'; end if;
 if p_expected_revision is null or p_expected_revision<0 or p_payload is null
   or jsonb_typeof(p_payload)<>'object' or octet_length(p_payload::text)>5242880
   or not(p_payload ? 'schema') or not(p_payload ? 'revision') then
  raise exception 'Invalid plan payload' using errcode='22023';
 end if;
 insert into public.worthwhile_plans(user_id,revision,payload)
  values(uid,0,'{}'::jsonb) on conflict(user_id) do nothing;
 select revision into current_revision from public.worthwhile_plans where user_id=uid for update;
 if current_revision<>p_expected_revision then
  raise exception 'Revision conflict; reload your plan' using errcode='40001';
 end if;
 if (p_payload->>'revision')::bigint<>current_revision+1 then
  raise exception 'Payload revision mismatch' using errcode='22023';
 end if;
 update public.worthwhile_plans set previous_payload=payload,payload=p_payload,
  revision=current_revision+1,updated_at=now() where user_id=uid;
 return current_revision+1;
end;
$$;
revoke all on function public.save_worthwhile_plan(jsonb,bigint) from public,anon,authenticated;
grant execute on function public.save_worthwhile_plan(jsonb,bigint) to authenticated;
notify pgrst, 'reload schema';
commit;
