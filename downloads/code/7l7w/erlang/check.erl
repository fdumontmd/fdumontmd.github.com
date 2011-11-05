-module(check).
-export([print_result/1]).
-export([check_result/1]).

print_result(M) -> io:fwrite("~s\n", [check_result(M)]).

check_result(success) -> "success";
check_result({error, Message}) -> lists:append("Error: ", Message).
