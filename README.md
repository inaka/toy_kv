# Toy KV Store

A simple and reduced Key-Value Store written in Erlang.

> **Note:**
> This project was done for educational and demonstrative purposes.

## Building toy_kv

    $ git clone https://github.com/inaka/toy_kv.git
    $ cd toy_kv
    $ make

## Quick Start Example

Open an Erlang terminal:

    $ erl -pa ./_build/default/lib/*/ebin

Now let's play a little bit:

```erlang
% Start toy_kv
toy_kv:start().

=INFO REPORT==== 10-Dec-2015::12:11:31 ===
    application: mnesia
    exited: stopped
    type: temporary
{ok,[mnesia,toy_kv]}

% Set a key/value pair. The 1st argument is the name of the bucket, and
% due to we didn't provide any configuration, only the default bucket
% is created
toy_kv:set(default, k1, <<"Hello">>).
ok

% Get the stored value
toy_kv:get(default, k1).
{ok,<<"Hello">>}

% Delete the stored value
toy_kv:del(default, k1).
ok

% Check
toy_kv:get(default, k1).
{error,notfound}
```

## Configuration

You can configure `toy_kv`:

```erlang
[
 {toy_kv,
  [
   %% Default number of replicas
   {replicas, 1},

   %% Buckets
   {buckets,
    [
     {b1,
      [
       {copies, disc_copies}
      ]
     },
     {b2,
      [
       {copies, ram_copies}
      ]
     }
    ]
   }
  ]
 }
].
```

## Distributed toy_kv

You can also deploy `toy_kv` in distributed way without any complex or magical
effort. In fact, `toy_kv` was developed to be distributed.

The first thing we have to do is start a set of Erlang nodes with `toy_kv`
running. Then, we can use `toy_kv:join/2` and `toy_kv:leave/2` functions
in order to setup our KV cluster and reads/writes can be distributed
across all nodes.

Once we have configured our cluster, the usage is the same as in the example
above, we can start to use the KV basic functions (`set/3`, `get/2`, `del/2`)
normally.

## Running Tests

Tests are divided in two: local tests and distributed tests. To run them all:

    $ make tests

To run only local tests:

    $ make local_tests

And run only distributed tests:

    $ make dist_tests
