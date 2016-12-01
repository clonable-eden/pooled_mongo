# pooled_mongo

Connection pool for [mongodb](https://github.com/comtihon/mongodb-erlang) via [poolboy](https://github.com/devinus/poolboy).

## Configuration

Add below to your app.config.

```
  {pooled_mongo,
    [
      {global_or_local, local},
      {pools,
        [
          {mongo_pool1,
            [
              {size, 10},
              {max_overflow, 20}
            ],
            [
              {host, "127.0.0.1"},
              {port, 27017},
              {database, <<"test">>},
              {connect_timeout, 500},
              {w_mode, safe},
              {r_mode, slave_ok}
            ]
          }
        ]
      }
    ]
  }
```

## Usage

If you use any pool, available functions are:  
`insert/3`, `update/4`, `update/5`, `delete/3`, `delete_one/3`, `find_one/3`, `find_one/4`, `find/3`, `find/4`, `count/3`, `count/4`, `ensure_index/3`, `command/2`, `execute/2`

If you retrieve one of the pools, use `pool/0` or choose from the result of `pools/0`.

If you use random pool, available functions are:
`insert2/2`, `update2/3`, `update2/4`, `delete2/2`, `delete_one2/2`, `find_one2/2`, `find_one2/3`, `find2/2`, `find2/3`, `count2/2`, `count2/3`, `ensure_index2/2`, `command2/1`, `execute2/1`

## Example

```erlang
> application:start(pooled_mongo).
> PoolId = pooled_mongo:pool().
> {ok, Result} = pooled_mongo:find_one(PoolId, <<"test">>, {<<"key">>, <<"value">>}).
  (Result may be Map)
> {ok, Result} = pooled_mongo:find_one(<<"test">>, {<<"key">>, <<"value">>}).
  (Result may be Map)
```  

## TODO

* Behavior for no connection. (Badmatch occurs in poolboy for now.)
* Behavior to reconnect. (should be like [eredis](https://github.com/wooga/eredis).)
* Functions to create/delete pool dynamically.
