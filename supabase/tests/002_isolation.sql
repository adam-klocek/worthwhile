-- Runs after 000_auth_fixture.sql and the migration in a disposable test DB.
begin;
set local role authenticated;
select set_config('request.jwt.claim.sub','11111111-1111-4111-8111-111111111111',true);
select public.save_worthwhile_plan('{"schema":4,"revision":1,"marker":"A-private"}',0);
do $$ begin
 if (select count(*) from public.worthwhile_plans)<>1 then raise exception 'A cannot read own plan'; end if;
 if (select count(*) from public.worthwhile_plans where user_id='22222222-2222-4222-8222-222222222222')<>0 then raise exception 'A saw B'; end if;
 begin
  perform public.save_worthwhile_plan('{"schema":4,"revision":1}',0);
  raise exception 'Stale revision was accepted';
 exception when serialization_failure then null; end;
 begin
  update public.worthwhile_plans set payload='{}';
  raise exception 'Direct write was allowed';
 exception when insufficient_privilege then null; end;
end $$;
select set_config('request.jwt.claim.sub','22222222-2222-4222-8222-222222222222',true);
do $$ begin
 if (select count(*) from public.worthwhile_plans)<>0 then raise exception 'B saw A'; end if;
end $$;
select public.save_worthwhile_plan('{"schema":4,"revision":1,"marker":"B-private"}',0);
select public.save_worthwhile_plan('{"schema":4,"revision":2,"marker":"B-updated"}',1);
do $$ begin
 if (select count(*) from public.worthwhile_plans)<>1 then raise exception 'B row isolation failed'; end if;
 if (select previous_payload->>'marker' from public.worthwhile_plans)<>'B-private' then raise exception 'Previous good snapshot not kept'; end if;
 begin
  perform public.save_worthwhile_plan('{"schema":4,"revision":1,"user_id":"11111111-1111-4111-8111-111111111111"}',2);
  raise exception 'Wrong revision accepted';
 exception when invalid_parameter_value then null; end;
 perform public.save_worthwhile_plan('{"schema":4,"revision":3,"user_id":"11111111-1111-4111-8111-111111111111","marker":"B-still-own"}',2);
 if (select count(*) from public.worthwhile_plans)<>1 then raise exception 'Payload user_id escaped isolation'; end if;
end $$;
reset role;
set local role anon;
select set_config('request.jwt.claim.sub','',true);
do $$ begin
 begin
  perform payload from public.worthwhile_plans;
  raise exception 'Anonymous read was allowed';
 exception when insufficient_privilege then null; end;
 begin
  perform public.save_worthwhile_plan('{"schema":4,"revision":1}',0);
  raise exception 'Anonymous RPC was allowed';
 exception when insufficient_privilege then null; end;
end $$;
reset role;
do $$ begin
 if (select payload->>'marker' from public.worthwhile_plans where user_id='11111111-1111-4111-8111-111111111111')<>'A-private' then raise exception 'A was overwritten by B'; end if;
end $$;
rollback;
