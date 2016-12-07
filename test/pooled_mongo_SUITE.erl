-module(pooled_mongo_SUITE).
-author("clonable-eden").

-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([
  all/0,
  init_per_suite/1,
  end_per_suite/1
]).

-export([
  simple_test/1
]).

init_per_suite(Config) ->
  application:ensure_all_started(pooled_mongo),
  Config.

end_per_suite(_Config) ->
  application:stop(pooled_mongo),
  ok.

all() ->
  [
    simple_test
  ].

simple_test(_Config) ->

  %% insert
  {ok, Teams} = pooled_mongo:insert2(<<"Teams">>, [
    {<<"name">>, <<"Yankees">>, <<"home">>, {<<"city">>, <<"New York">>, <<"state">>, <<"NY">>}, <<"league">>, <<"American">>},
    {<<"name">>, <<"Mets">>, <<"home">>, {<<"city">>, <<"New York">>, <<"state">>, <<"NY">>}, <<"league">>, <<"National">>},
    {<<"name">>, <<"Phillies">>, <<"home">>, {<<"city">>, <<"Philadelphia">>, <<"state">>, <<"PA">>}, <<"league">>, <<"National">>},
    {<<"name">>, <<"Red Sox">>, <<"home">>, {<<"city">>, <<"Boston">>, <<"state">>, <<"MA">>}, <<"league">>, <<"American">>}
  ]),
  ?assertEqual(length(Teams), 4),
  ct:log("Teams ~p", [Teams]),

  %% ensure_index
  %% pooled_mongo:ensure_index2(<<"Teams">>, {}),

  %% count
  {ok, Count} = pooled_mongo:count2(<<"Teams">>, {}),
  ?assertEqual(Count, 4),

  %% delete_one
  {ok, M} = pooled_mongo:delete_one2(<<"Teams">>, {<<"name">>, <<"Mets">>}),
  ?assertEqual(1, maps:get(<<"n">>, M)),
  ct:log("delete_one result ~p", [M]),

  %% count
  {ok, Count2} = pooled_mongo:count2(<<"Teams">>, {}),
  ?assertEqual(Count2, 3),

  %% find_one
  {ok, Team} = pooled_mongo:find_one2(<<"Teams">>, {<<"name">>, <<"Red Sox">>}),
  ?assertEqual(maps:get(<<"name">>, Team), <<"Red Sox">>),
  ct:log("find_one result ~p", [Team]),

  %% update
  Doc = #{<<"$set">> => #{<<"hoge">> => <<"fuga">>}},
  {ok, M2} = pooled_mongo:update2(<<"Teams">>, {<<"name">>, <<"Red Sox">>}, Doc),
  ?assertEqual(maps:get(<<"n">>, M2), 1),
  ct:log("update result ~p", [M2]),

  %% find
  {ok, Cursor} = pooled_mongo:find2(<<"Teams">>, {}),
  Teams2 = mc_cursor:rest(Cursor),
  ct:log("find result ~p", [Teams2]),

  %% delete
  {ok, M3} = pooled_mongo:delete2(<<"Teams">>, {}),
  ct:log("delete result ~p", [M3]),

  ?assert(true).
