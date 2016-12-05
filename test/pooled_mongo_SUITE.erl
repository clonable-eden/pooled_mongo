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
  ok.

all() ->
  [
    simple_test
  ].

simple_test(_Config) ->
  ?assert(true).

