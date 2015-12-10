-module(toy_kv_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @hidden
init([]) ->
  Buckets = application:get_env(toy_kv, buckets, default_buckets()),
  Fun = fun({Name, Options}) -> child(Name, Options) end,
  Children = lists:map(Fun, Buckets),
  {ok, {{one_for_one, 5, 10}, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
child(Name, Options) ->
  {
   Name,
   {toy_kv, start_link, [Name, Options]},
   permanent,
   5000,
   worker,
   [toy_kv]
  }.

%% @private
default_buckets() ->
  [{default, [{copies, ram_copies}]}].
