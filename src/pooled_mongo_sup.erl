%%%-------------------------------------------------------------------
%%% @doc
%%% pooled_mongo supervisor.
%%% @end
%%%-------------------------------------------------------------------
-module(pooled_mongo_sup).
-author("clonable-eden").

-behaviour(supervisor).

%% API
-export([start_link/0, start_link/2]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @end
%%--------------------------------------------------------------------
-spec(start_link() ->
  {ok, Pid :: pid()} | ignore | {error, Reason :: term()}).
start_link() ->
  {ok, Pools} = application:get_env(pooled_mongo, pools),
  {ok, GlobalOrLocal} = application:get_env(pooled_mongo, global_or_local),
  start_link(Pools, GlobalOrLocal).

start_link(Pools, GlobalOrLocal) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, [Pools, GlobalOrLocal]).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

init([Pools, GlobalOrLocal]) ->
  RestartStrategy = one_for_all,
  MaxRestarts = 10,
  MaxSecondsBetweenRestarts = 10,

  SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

  PoolSpecs = lists:map(
    fun({Name, SizeArgs, WorkerArgs}) ->
      PoolArgs = [{name, {GlobalOrLocal, Name}}, {worker_module, mc_worker}] ++ SizeArgs,
      poolboy:child_spec(Name, PoolArgs, WorkerArgs)
    end, Pools),

  {ok, {SupFlags, PoolSpecs}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
