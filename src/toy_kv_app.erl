-module(toy_kv_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%% @hidden
start(_StartType, _StartArgs) ->
  init_mnesia(),
  toy_kv_sup:start_link().

%% @hidden
-spec stop(term()) -> _.
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
init_mnesia() ->
  mnesia:stop(),
  mnesia:create_schema([node()]),
  mnesia:start(),
  ok.
