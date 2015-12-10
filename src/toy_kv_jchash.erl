-module(toy_kv_jchash).

-export([compute/2]).
-on_load(init/0).

-define(APPNAME, toy_kv).
-define(LIBNAME, toy_kv_jchash).

%%==============================================================================
%% API
%%==============================================================================

%% @doc
%% This implementation stub is only called if the NIF is not loaded.
%% @end
compute(_, _) ->
  not_loaded(?LINE).

%% @doc
%% This function is called by the implementation stub to produce an error
%% in case the <code>compute</code> function is called while the module is not
%% loaded.
%% @end
not_loaded(Line) ->
  erlang:nif_error({not_loaded, [{module, ?MODULE}, {line, Line}]}).

%%==============================================================================
%% NIF init
%%==============================================================================

%% @doc
%% Finds the path to the .so file and loads the NIF.
%% @end
init() ->
  SoName = case code:priv_dir(?APPNAME) of
             {error, bad_name} ->
               case filelib:is_dir(filename:join(["..", priv])) of
                 true ->
                   filename:join(["..", priv, ?LIBNAME]);
                 _ ->
                   filename:join([priv, ?LIBNAME])
               end;
             Dir ->
               filename:join(Dir, ?LIBNAME)
           end,
  erlang:load_nif(SoName, 0).
