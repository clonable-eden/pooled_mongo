%%%-------------------------------------------------------------------
%%% @doc
%%% pooled_mongo API.
%%% @end
%%%-------------------------------------------------------------------
-module(pooled_mongo).
-author("clonable-eden").

-export([start/0, stop/0]).

%% API
-export([
  pool/0,
  pools/0
]).
-export([
  insert/3,
  insert2/2
]).
-export([
  update/4,
  update/6,
  update2/3,
  update2/5
]).
-export([
  delete/3,
  delete2/2
]).
-export([
  delete_one/3,
  delete_one2/2
]).
-export([
  find_one/3,
  find_one/4,
  find_one2/2,
  find_one2/3
]).
-export([
  find/3,
  find/4,
  find2/2,
  find2/3
]).
-export([
  count/3,
  count/4,
  count2/2,
  count2/3
]).
-export([
  ensure_index/3,
  ensure_index2/2
]).
-export([
  command/2,
  command2/1
]).
-export([
  execute/2,
  execute2/1
]).

-type poolid() :: atom().
-type collection() :: binary() | atom(). % without db prefix
-type selector() :: bson:document().
-type cursor() :: pid().

-define(TCP_CLOSED_WAIT_MS, 10).
-define(NOPROC_WAIT_MS, 100).

start() ->
  application:start(?MODULE).

stop() ->
  application:stop(?MODULE).

%% @doc Retrieve poolId.
-spec pool() -> poolid().
pool() ->
  {ok, Pools} = application:get_env(pooled_mongo, pools),
  % thanks: https://github.com/lasp-lang/rand_compat
  Slot = case (code:which(rand) /= non_existing) of
    true ->
      rand:seed(exs1024),
      rand:uniform(length(Pools));
    false ->
      (fun random:seed/1)(erlang:timestamp()),
      (fun random:uniform/1)(length(Pools))
    end,
  {Name, _, _} = lists:nth(Slot, Pools),
  Name.

%% @doc Retrieve poolIds as List.
-spec pools() -> [poolid()].
pools() ->
  {ok, Pools} = application:get_env(pooled_mongo, pools),
  lists:map(fun({Name, _, _}) -> Name end, Pools).

%% @doc Insert a document or multiple documents into a collection.
%%      Returns the document or documents with an auto-generated _id if missing.
-spec insert(poolid(), collection(), A) -> {ok, A} | {error, Reason::term()}.
insert(PoolId, Collection, Doc) ->
  exec(PoolId,
    fun(Worker) -> (fun mc_worker_api:insert/3)(Worker, Collection, Doc) end,
    fun() -> insert(PoolId, Collection, Doc) end
  ).

%% @doc Insert a document or multiple documents into a collection.
%%      Returns the document or documents with an auto-generated _id if missing.
-spec insert2(collection(), A) -> {ok, A} | {error, Reason::term()}.
insert2(Collection, Doc) ->
  insert(pool(), Collection, Doc).

%% @doc Replace the document matching criteria entirely with the new Document.
-spec update(poolid(), collection(), selector(), bson:document()) -> {ok, map()} | {error, Reason::term()}.
update(PoolId, Collection, Selector, Doc) ->
  update(PoolId, Collection, Selector, Doc, false, false).

%% @doc Replace the document matching criteria entirely with the new Document.
-spec update(poolid(), collection(), selector(), bson:document(), boolean(), boolean()) -> {ok, map()} | {error, Reason::term()}.
update(PoolId, Collection, Selector, Doc, Upsert, MultiUpdate) ->
  exec(PoolId,
    fun(Worker) -> (fun mc_worker_api:update/6)(Worker, Collection, Selector, Doc, Upsert, MultiUpdate) end,
    fun() -> update(PoolId, Collection, Selector, Doc, Upsert, MultiUpdate) end
  ).

%% @doc Replace the document matching criteria entirely with the new Document.
-spec update2(collection(), selector(), bson:document()) -> {ok, map()} | {error, Reason::term()}.
update2(Collection, Selector, Doc) ->
  update(pool(), Collection, Selector, Doc, false, false).

%% @doc Replace the document matching criteria entirely with the new Document.
-spec update2(collection(), selector(), bson:document(), boolean(), boolean()) -> {ok, map()} | {error, Reason::term()}.
update2(Collection, Selector, Doc, Upsert, Multiple) ->
  update(pool(), Collection, Selector, Doc, Upsert, Multiple).

%% @doc Delete selected documents
-spec delete(poolid(), collection(), selector()) -> {ok, map()} | {error, Reason::term()}.
delete(PoolId, Collection, Selector) ->
  exec(PoolId,
    fun(Worker) -> (fun mc_worker_api:delete/3)(Worker, Collection, Selector) end,
    fun() -> delete(PoolId, Collection, Selector) end
  ).

%% @doc Delete selected documents
-spec delete2(collection(), selector()) -> {ok, map()} | {error, Reason::term()}.
delete2(Collection, Selector) ->
  delete(pool(), Collection, Selector).

%% @doc Delete first selected document.
-spec delete_one(poolid(), collection(), selector()) -> {ok, map()} | {error, Reason::term()}.
delete_one(PoolId, Collection, Selector) ->
  exec(PoolId,
    fun(Worker) -> (fun mc_worker_api:delete_one/3)(Worker, Collection, Selector) end,
    fun() -> delete_one(PoolId, Collection, Selector) end
  ).

%% @doc Delete first selected document.
-spec delete_one2(collection(), selector()) -> {ok, map()} | {error, Reason::term()}.
delete_one2(Collection, Selector) ->
  delete_one(pool(), Collection, Selector).

%% @doc Return first selected document, if any
-spec find_one(PoolId :: poolid(), Collection :: collection(), Selector :: selector()) ->
  {ok, {}} | {ok, map()} | {error, Reason::term()}.
find_one(PoolId, Collection, Selector) ->
  find_one(PoolId, Collection, Selector, #{}).

%% @doc Return first selected document, if any
-spec find_one(PoolId :: poolid(), Collection :: collection(), Selector :: selector(), Args :: map()) ->
  {ok, {}} | {ok, map()} | {error, Reason::term()}.
find_one(PoolId, Collection, Selector, Args) ->
  exec(PoolId,
    fun(Worker) -> mc_worker_api:find_one(Worker, Collection, Selector, Args) end,
    fun() -> find_one(PoolId, Collection, Selector, Args) end
  ).

%% @doc Return first selected document, if any
-spec find_one2(Collection :: collection(), Selector :: selector()) ->
  {ok, {}} | {ok, map()} | {error, Reason::term()}.
find_one2(Collection, Selector) ->
  find_one(pool(), Collection, Selector, #{}).

%% @doc Return first selected document, if any
-spec find_one2(Collection :: collection(), Selector :: selector(), Args :: map()) ->
  {ok, {}} | {ok, map()} | {error, Reason::term()}.
find_one2(Collection, Selector, Args) ->
  find_one(pool(), Collection, Selector, Args).

%% @doc Return selected documents.
-spec find(PoolId :: poolid(), Collection :: collection(), Selector :: selector()) ->
  {ok, cursor()} | {error, Reason::term()}.
find(PoolId, Collection, Selector) ->
  find(PoolId, Collection, Selector, #{}).

%% @doc Return selected documents.
-spec find(PoolId :: poolid(), Collection :: collection(), Selector :: selector(), Args :: map()) ->
  {ok, cursor()} | {error, Reason::term()}.
find(PoolId, Collection, Selector, Args) ->
  exec(PoolId,
    fun(Worker) -> mc_worker_api:find(Worker, Collection, Selector, Args) end,
    fun() -> find(PoolId, Collection, Selector, Args) end
  ).

%% @doc Return selected documents.
-spec find2(Collection :: collection(), Selector :: selector()) ->
  {ok, cursor()} | {error, Reason::term()}.
find2(Collection, Selector) ->
  find(pool(), Collection, Selector, #{}).

%% @doc Return selected documents.
-spec find2(Collection :: collection(), Selector :: selector(), Args :: map()) ->
  {ok, cursor()} | {error, Reason::term()}.
find2(Collection, Selector, Args) ->
  find(pool(), Collection, Selector, Args).

%@doc Count selected documents
-spec count(poolid(), collection(), selector()) -> {ok, integer()} | {error, Reason::term()}.
count(PoolId, Collection, Selector) ->
  count(PoolId, Collection, Selector, #{limit => 0}).

%@doc Count selected documents
-spec count(poolid(), collection(), selector(), map()) -> {ok, integer()} | {error, Reason::term()}.
count(PoolId, Collection, Selector, Limit) ->
  exec(PoolId,
    fun(Worker) -> (fun mc_worker_api:count/4)(Worker, Collection, Selector, Limit) end,
    fun() -> count(PoolId, Collection, Selector, Limit) end
  ).

%@doc Count selected documents
-spec count2(collection(), selector()) -> {ok, integer()} | {error, Reason::term()}.
count2(Collection, Selector) ->
  count(pool(), Collection, Selector, #{limit => 0}).

%@doc Count selected documents
-spec count2(collection(), selector(), map()) -> {ok, integer()} | {error, Reason::term()}.
count2(Collection, Selector, Limit) ->
  count(pool(), Collection, Selector, Limit).

%% @doc Create index on collection according to given spec.
%%      The key specification is a bson documents with the following fields:
%%      key      :: bson document, for e.g. {field, 1, other, -1, location, 2d}, <strong>required</strong>
%%      name     :: bson:utf8()
%%      unique   :: boolean()
%%      dropDups :: boolean()
-spec ensure_index(poolid(), collection(), bson:document()) -> {ok, ok} | {error, Reason::term()}.
ensure_index(PoolId, Coll, IndexSpec) ->
  exec(PoolId,
    fun(Worker) -> mc_worker_api:ensure_index(Worker, Coll, IndexSpec) end,
    fun() -> ensure_index(PoolId, Coll, IndexSpec) end
  ).

%% @doc Create index on collection according to given spec.
%%      The key specification is a bson documents with the following fields:
%%      key      :: bson document, for e.g. {field, 1, other, -1, location, 2d}, <strong>required</strong>
%%      name     :: bson:utf8()
%%      unique   :: boolean()
%%      dropDups :: boolean()
-spec ensure_index2(collection(), bson:document()) -> {ok, ok} | {error, Reason::term()}.
ensure_index2(Coll, IndexSpec) ->
  ensure_index(pool(), Coll, IndexSpec).

%% @doc Execute given MongoDB command and return its result.
-spec command(poolid(), bson:document()) -> {ok, bson:document()} | {error, Reason::term()}. % Action
command(PoolId, Command) ->
  exec(PoolId,
    fun(Worker) -> mc_worker_api:command(Worker, Command) end,
    fun() -> command(PoolId, Command) end
  ).

%% @doc Execute given MongoDB command and return its result.
-spec command2(bson:document()) -> {ok, bson:document()} | {error, Reason::term()}. % Action
command2(Command) ->
  command(pool(), Command).

%% @doc Execute function to use raw mongodb APIs.
-spec execute(poolid(), Function :: fun()) -> {ok, _} | {error, Reason::term()}.
execute(PoolId, Function) ->
  exec(PoolId, Function, fun() -> execute(PoolId, Function) end).

%% @doc Execute function to use raw mongodb APIs.
-spec execute2(Function :: fun()) -> {ok, _} | {error, Reason::term()}.
execute2(Function) ->
  execute(pool(), Function).

%% @private
%% @doc Execute.
%%      First function is given Connection.
%%      Second function is called when any error occurs. (Please see handle_error)
-spec exec(PoolId :: poolid(), Function :: fun(), Fallback :: fun()) ->
  {ok, _} | {error, Reason::term()}.
exec(PoolId, Function, Fallback) ->
  try poolboy:transaction(PoolId, Function) of
    {{true, #{<<"writeErrors">> :=WerrMap}}, _}->
      handle_error({error,WerrMap}, Fallback); 
    {{true, _}, Result} ->
      {ok, Result};
    {true, Result} ->
      {ok, Result};
    Result ->
      {ok, Result}
  catch
    _:Reason ->
      handle_error(Reason, Fallback)
  end.

%% @private
%% @doc Handle error.
-spec handle_error(Reason :: term(), Fun :: fun()) ->
  {ok, _} | {error, Reason::term()}.
handle_error(Reason, Fun) ->
  case Reason of
    {tcp_closed, _} ->
      %% retry, because worker socket has been closed.
      retry(Fun, ?TCP_CLOSED_WAIT_MS);
    {noproc, _} ->
      %% restart supervisor, if noproc raised.
      catch pooled_mongo_sup:start_link(),
      retry(Fun, ?NOPROC_WAIT_MS);
    {error, _} ->
      %% others.
      {error, Reason}
  end.

%% @private
%% @doc Retry with another worker.
retry(Fun, Wait) ->
  %% execute Fun, after Wait ms.
  timer:sleep(Wait),
  Fun().
