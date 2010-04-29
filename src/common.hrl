-ifdef(debug).
-define(LOG(X,Args), util:format(?MODULE_DEBUG,X,Args,{?MODULE,?LINE})).
-else.
-define(LOG(X,Args), true).
-endif.

-define(MODULE_DEBUG,true).
