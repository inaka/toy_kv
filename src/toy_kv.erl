-module(toy_kv).

-behaviour(gen_server).

%% API
-export([start/0,
         stop/0,
         start_link/2,
         set/3,
         set/4,
         get/2,
         get/3,
         del/2,
         del/3,
         preflist/2,
         preflist/3,
         get_state/1,
         get_nodes/1,
         join/2,
         leave/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%% Types
-type option()   :: {atom(), term()}.
-type options()  :: [option()].
-type bucket()   :: atom().
-type key()      :: term().
-type value()    :: term().
-type replicas() :: pos_integer().
-type flag()     :: {replicas, replicas()}.
-type flags()    :: [flag()].

%% KV Table spec.
-define(TOY_KV, {key, value}).

%% State
-record(state, {bucket                  :: bucket(),
                copies = ram_copies     :: ram_copies | disc_copies,
                auto_node_list = true   :: boolean(),
                auto_eject_nodes = true :: boolean(),
                nodes = []              :: [node()]}).
-type state() :: #state{}.

-export_type([options/0,
              bucket/0,
              key/0,
              value/0,
              replicas/0,
              state/0]).

%%%===================================================================
%%% API
%%%===================================================================

-spec start() -> {ok, _} | {error, term()}.
start() ->
  application:ensure_all_started(toy_kv).

-spec stop() -> ok.
stop() ->
  application:stop(toy_kv).

-spec start_link(atom(), options()) -> gen:start_ret().
start_link(Name, Options) ->
  gen_server:start_link({local, Name}, ?MODULE, [Name, Options], []).

%% @equiv set(Bucket, Key, Value, [])
set(Bucket, Key, Value) ->
  set(Bucket, Key, Value, []).

-spec set(bucket(), key(), value(), flags()) -> ok | {error, atom()}.
set(Bucket, Key, Value, Flags) ->
  multicall(Bucket, Key, Flags, {set, Key, Value}).

%% @equiv get(Bucket, Key, [])
get(Bucket, Key) ->
  get(Bucket, Key, []).

-spec get(bucket(), key(), flags()) -> {ok, value()} | {error, atom()}.
get(Bucket, Key, Flags) ->
  multicall(Bucket, Key, Flags, {get, Key}).

%% @equiv del(Bucket, Key, [])
del(Bucket, Key) ->
  del(Bucket, Key, []).

-spec del(bucket(), key(), flags()) -> ok | {error, atom()}.
del(Bucket, Key, Flags) ->
  multicall(Bucket, Key, Flags, {del, Key}).

%%%===================================================================
%%% Extended API
%%%===================================================================

%% @equiv preflist(Key, N, [node() | nodes()])
preflist(Key, N) ->
  preflist(Key, N, [node() | nodes()]).

-spec preflist(key(), replicas(), [node()]) -> [node()].
preflist(Key, N, Nodes) ->
  CHashKey = erlang:phash2(Key),
  SortedNodes = lists:usort(Nodes),
  PivotNode = toy_kv_jchash:compute(CHashKey, length(SortedNodes)) + 1,
  ring(SortedNodes, PivotNode, N).

-spec get_state(bucket()) -> state().
get_state(Bucket) ->
  gen_server:call(Bucket, get_state).

-spec get_nodes(bucket()) -> [node()].
get_nodes(Bucket) ->
  gen_server:call(Bucket, get_nodes).

-spec join(bucket(), [node()]) -> ok | invalid_node.
join(Bucket, Nodes) ->
  CurrentNodes = [node() | get_nodes(Bucket)],
  AllNodes = rem_dups_from_list(CurrentNodes ++ Nodes),
  {OkRep, _} = global:trans(
    {{?MODULE, Bucket}, self()},
    fun() -> gen_server:multi_call(CurrentNodes, Bucket, {join, AllNodes}) end),
  case lists:keyfind(invalid_node, 2, OkRep) of
    false -> ok;
    _     -> leave(Bucket, Nodes), invalid_node
  end.

-spec leave(bucket(), [node()]) -> ok.
leave(Bucket, Nodes) ->
  CurrentNodes = [node() | get_nodes(Bucket)],
  global:trans(
    {{?MODULE, Bucket}, self()},
    fun() -> gen_server:multi_call(CurrentNodes, Bucket, {leave, Nodes}) end),
  ok.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%% @hidden
init([Name, Options]) ->
  State = parse_options(Options, #state{bucket = Name}),
  init_bucket(State),
  {ok, State}.

%% @hidden
handle_call({set, Key, Value}, _From, #state{bucket = Bucket} = State) ->
  {reply, set_(Bucket, Key, Value), State};
handle_call({get, Key}, _From, #state{bucket = Bucket} = State) ->
  {reply, get_(Bucket, Key), State};
handle_call({del, Key}, _From, #state{bucket = Bucket} = State) ->
  {reply, del_(Bucket, Key), State};
handle_call(get_state, _From, State) ->
  {reply, State, State};
handle_call(get_nodes, _From, State) ->
  {reply, State#state.nodes, State};
handle_call({join, Nodes}, _From, #state{nodes = CurrentNodes} = State) ->
  NewNodes = Nodes -- [node() | CurrentNodes],
  NewNodesOk = lists:foldl(
    fun(E, Acc) -> net_adm:ping(E) =:= pong andalso Acc end, true, NewNodes),
  case NewNodesOk of
    true ->
      {reply, ok, State#state{nodes = CurrentNodes ++ NewNodes}};
    _ ->
      {reply, invalid_node, State}
  end;
handle_call({leave, Nodes}, _From, #state{nodes = CurrentNodes} = State) ->
  case lists:member(node(), Nodes) of
    true ->
      {reply, ok, State#state{nodes = []}};
    _ ->
      F = fun(E, Acc) -> lists:delete(E, Acc) end,
      NewNodes = lists:foldl(F, CurrentNodes, Nodes),
      {reply, ok, State#state{nodes = NewNodes}}
  end;
handle_call(Request, From, State) ->
  error_logger:warning_msg(
    "Unexpected message:\nhandle_call(~p, ~p, ~p)\n",
    [Request, From, State]),
  {reply, ok, State}.

%% @hidden
handle_cast(_Request, State) ->
  {noreply, State}.

%% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

%% @hidden
terminate(_Reason, _State) ->
  ok.

%% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private
preflist_(Key, N, #state{nodes = [], auto_node_list = true}) ->
  preflist(Key, N, [node() | nodes()]);
preflist_(Key, N, #state{nodes = Nodes}) ->
  preflist(Key, N, [node() | Nodes]).

%% @private
parse_options([], State) ->
  State;
parse_options([{copies, Val} | Opts], State) when is_atom(Val) ->
  parse_options(Opts, State#state{copies = Val});
parse_options([{auto_node_list, Val} | Opts], State) when is_boolean(Val) ->
  parse_options(Opts, State#state{auto_node_list = Val});
parse_options([{auto_eject_nodes, Val} | Opts], State) when is_boolean(Val) ->
  parse_options(Opts, State#state{auto_eject_nodes = Val});
parse_options([_Opt | Opts], State) ->
  parse_options(Opts, State).

%% @private
init_bucket(#state{bucket = Bucket, copies = Copies}) ->
  mnesia:create_table(
    Bucket, [{Copies, [node()]}, {attributes, tuple_to_list(?TOY_KV)}]),
  ok.

%% @private
set_(Bucket, Key, Value) ->
  F = fun() -> mnesia:write({Bucket, Key, Value}) end,
  mnesia:activity(transaction, F).

%% @private
get_(Bucket, Key) ->
  case mnesia:dirty_read(Bucket, Key) of
    [{_, Key, Value}] -> {ok, Value};
    []                -> {error, notfound}
  end.

%% @private
del_(Bucket, Key) ->
  F = fun() -> mnesia:delete({Bucket, Key}) end,
  mnesia:activity(transaction, F).

%% @private
multicall(Bucket, Key, Flags, Msg) ->
  State = #state{auto_eject_nodes = AutoEject} = get_state(Bucket),
  N = get_replicas(Flags),
  PrefList = preflist_(Key, N, State),
  {OkRep, BadNodes} = global:trans(
    {{?MODULE, Key}, self()},
    fun() -> gen_server:multi_call(PrefList, Bucket, Msg) end),
  case {BadNodes, AutoEject} of
    {_, true} when length(BadNodes) > 0 ->
      leave(Bucket, BadNodes);
    _ ->
      ok
  end,
  reconcile_values(OkRep, BadNodes).

%% @private
reconcile_values([], BadNodes) ->
  error_logger:error_msg("No available nodes. Bad Nodes: ~p~n", [BadNodes]),
  throw(no_available_nodes);
reconcile_values(OkNodes, _) ->
  F = fun({_, {ok, _} = X}, {OkL, ErrL})    -> {[X | OkL], ErrL};
         ({_, {error, _} = X}, {OkL, ErrL}) -> {OkL, [X | ErrL]};
         ({_, ok}, {OkL, ErrL})             -> {[ok | OkL], ErrL};
         ({_, X}, {OkL, ErrL})              -> {OkL, [X | ErrL]}
      end,
  case lists:foldl(F, {[], []}, OkNodes) of
    {[Res | _], _} -> Res;
    {_, [Res | _]} -> Res
  end.

%% @private
get_replicas(Flags) ->
  case lists:keyfind(replicas, 1, Flags) of
    {_, N} -> N;
    _      -> application:get_env(toy_kv, replicas, 1)
  end.

%% @private
ring(L, Idx, N) when (length(L) - Idx) >= (N - 1) ->
  lists:sublist(L, Idx, N);
ring(L, Idx, N) when N > length(L) ->
  ring(L, Idx, length(L));
ring(L, Idx, N) ->
  D1 = (length(L) - Idx) + 1,
  D2 = N - D1,
  lists:sublist(L, Idx, D1) ++ lists:sublist(L, 1, D2).

%% @private
rem_dups_from_list(L) ->
  F = fun(E, Acc) ->
        case lists:member(E, Acc) of
          true  -> Acc;
          false -> [E | Acc]
        end
      end,
  lists:foldl(F, [], L).
