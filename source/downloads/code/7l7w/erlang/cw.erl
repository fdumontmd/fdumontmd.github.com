-module(cw).
-export([count_words/1]).

count_words([]) -> 0;
count_words([32|T]) -> count_words(T);
count_words(WS) -> skip_word(WS).

skip_word([]) -> 1;
skip_word([32|T]) -> 1+count_words(T);
skip_word([_|T]) -> skip_word(T).