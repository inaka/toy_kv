-module(local_SUITE).

-include_lib("common_test/include/ct.hrl").

%% Common Test
-export([all/0,
         init_per_suite/1,
         end_per_suite/1]).

%% Tests
-export([t_operations/1, t_persistence/1]).

%%==============================================================================
%% Common Test
%%==============================================================================

all() ->
  [t_operations, t_persistence].

init_per_suite(Config) ->
  toy_kv:start(),
  Config.

end_per_suite(Config) ->
  toy_kv:stop(),
  Config.

%%==============================================================================
%% Exported Test functions
%%==============================================================================

t_operations(_Config) ->
  lists:foreach(fun t_operations_/1, [b1, b2]).

t_operations_(Bucket) ->
  %% Set
  ok = toy_kv:set(Bucket, k1, v1),
  ok = toy_kv:set(Bucket, k2, v2),
  ok = toy_kv:set(Bucket, k3, v3),

  %% Get
  {ok, v1} = toy_kv:get(Bucket, k1),
  {ok, v2} = toy_kv:get(Bucket, k2),
  {ok, v3} = toy_kv:get(Bucket, k3),

  %% Delete
  ok = toy_kv:del(Bucket, k3),
  {error, notfound} = toy_kv:get(Bucket, k3),

  ct:print("\e[1;1m t_operations Bucket: ~p \e[0m\e[32m[OK] \e[0m", [Bucket]),
  ok.

t_persistence(_Config) ->
  %% Set some K/V pairs
  Set = fun({B, K, V}) -> ok = toy_kv:set(B, K, V) end,
  lists:foreach(
    fun(B) -> [Set({B, X, X}) || X <- lists:seq(1, 3)] end, [b1, b2]),

  %% Check
  Get = fun({B, K, V}) -> {ok, V} = toy_kv:get(B, K) end,
  lists:foreach(
    fun(B) -> [Get({B, X, X}) || X <- lists:seq(1, 3)] end, [b1, b2]),

  %% Restart app
  ok = toy_kv:stop(),
  {ok, _} = toy_kv:start(),
  timer:sleep(1500),

  %% Check persistent bucket
  [Get({b1, X, X}) || X <- lists:seq(1, 3)],

  %% Check memory bucket
  BadGet = fun({B, K}) -> {error, notfound} = toy_kv:get(B, K) end,
  [BadGet({b2, X}) || X <- lists:seq(1, 3)],

  ct:print("\e[1;1m t_persistence \e[0m\e[32m[OK] \e[0m"),
  ok.
