# list_as_table

#### _Pretty print proplists in erl shell_

## Synopsis

This is a simple module to present lists of proplists in form of ascii table.

## How to install

Include into dep section of `rebar.config`

```
{deps, [
  {list_as_table, "0.1.0", {git, "https://github.com/eiri/list_as_table.git", {tag, "0.1.0"}}}
]
```

If you prefer `erlang.mk`, add into your Makefile

```
DEPS = list_as_table
dep_list_as_table = https://github.com/eiri/list_as_table 0.1.0

```

## How to use

`last_as_table` is not Erlang application, i.e. it does not require to be started or stopped. Just make sure it is in `--pa` path and then pass a list of proplists to it: `list_as_table:print(List)`

### Example

Lets output EVM's process list as a table, simlar to erl's `i().`

```
Eshell V6.0  (abort with ^G)
(list_as_table@StarFortress)1> Keys = [registered_name,initial_call,current_function,heap_size,reductions,stack_size].
[registered_name,initial_call,current_function,heap_size,
 reductions,stack_size]
(list_as_table@StarFortress)2> F = fun(Pid, L) -> [{pid, Pid}|[{K, proplists:get_value(K, L)} || K <- Keys]] end.
#Fun<erl_eval.12.106461118>
(list_as_table@StarFortress)3> P = [F(Pid, erlang:process_info(Pid)) || Pid <- erlang:processes()].
[[{pid,<0.0.0>},
  {registered_name,init},
  {initial_call,{otp_ring0,start,2}},
... looong list ...

(list_as_table@StarFortress)4> list_as_table:print(P).
+----------+------------------------+---------------------------------+----------------------------------+-----------+------------+-------------+
| pid      | registered_name        | initial_call                    | current_function                 | heap_size | reductions | stack_size  |
+----------+------------------------+---------------------------------+----------------------------------+-----------+------------+-------------+
| <0.0.0>  | init                   | {otp_ring0,start,2}             | {init,loop,1}                    | 987       | 6322       | 2           |
| <0.3.0>  | erl_prim_loader        | {erlang,apply,2}                | {erl_prim_loader,loop,3}         | 1598      | 184393     | 6           |
| <0.6.0>  | error_logger           | {proc_lib,init_p,5}             | {gen_event,fetch_msg,5}          | 376       | 264        | 8           |
| <0.7.0>  | application_controller | {erlang,apply,2}                | {gen_server,loop,6}              | 987       | 409        | 7           |
| <0.9.0>  | undefined              | {proc_lib,init_p,5}             | {application_master,main_loop,2} | 376       | 44         | 6           |
| <0.10.0> | undefined              | {application_master,start_it,4} | {application_master,loop_it,4}   | 233       | 69         | 5           |
| <0.11.0> | kernel_sup             | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 6772      | 51801      | 9           |
| <0.12.0> | rex                    | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 233       | 35         | 9           |
| <0.13.0> | global_name_server     | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 233       | 51         | 9           |
| <0.14.0> | undefined              | {erlang,apply,2}                | {global,loop_the_locker,1}       | 233       | 19         | 5           |
| <0.15.0> | undefined              | {erlang,apply,2}                | {global,loop_the_registrar,0}    | 233       | 3          | 2           |
| <0.16.0> | inet_db                | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 233       | 243        | 9           |
| <0.17.0> | net_sup                | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 987       | 285        | 9           |
| <0.18.0> | erl_epmd               | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 233       | 268        | 9           |
| <0.19.0> | auth                   | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 987       | 864        | 9           |
| <0.20.0> | net_kernel             | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 610       | 758        | 9           |
| <0.21.0> | undefined              | {inet_tcp_dist,accept_loop,2}   | {prim_inet,accept0,2}            | 233       | 29         | 9           |
| <0.22.0> | undefined              | {net_kernel,ticker,2}           | {net_kernel,ticker_loop,2}       | 233       | 9          | 3           |
| <0.23.0> | global_group           | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 233       | 59         | 9           |
| <0.24.0> | file_server_2          | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 233       | 91         | 9           |
| <0.25.0> | code_server            | {erlang,apply,2}                | {code_server,loop,1}             | 6772      | 121642     | 3           |
| <0.26.0> | standard_error_sup     | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 233       | 41         | 9           |
| <0.27.0> | standard_error         | {erlang,apply,2}                | {standard_error,server_loop,1}   | 233       | 9          | 2           |
| <0.28.0> | undefined              | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 610       | 89         | 9           |
| <0.29.0> | user_drv               | {user_drv,server,2}             | {user_drv,server_loop,5}         | 2586      | 7913       | 8           |
| <0.30.0> | user                   | {group,server,3}                | {group,server_loop,3}            | 233       | 36         | 4           |
| <0.31.0> | undefined              | {group,server,3}                | {group,server_loop,3}            | 2586      | 2882       | 4           |
| <0.32.0> | undefined              | {erlang,apply,2}                | {shell,shell_rep,4}              | 2586      | 3979       | 17          |
| <0.33.0> | undefined              | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 233       | 286        | 9           |
| <0.34.0> | kernel_safe_sup        | {proc_lib,init_p,5}             | {gen_server,loop,6}              | 233       | 58         | 9           |
| <0.38.0> | undefined              | {erlang,apply,2}                | {erl_eval,do_apply,6}            | 4185      | 53852      | 51          |
+----------+------------------------+---------------------------------+----------------------------------+-----------+------------+-------------+
ok
```

Much more readable now, isn't it?

Simple example application included in _example_ directory. Run `make run` to compile and start it, then call `tabula:demo()` in erlang shell.

## Version history

   - 0.1.0 First, naive implementation.

## License

MIT. See [License](https://github.com/eiri/grass/blob/master/License "MIT License")
