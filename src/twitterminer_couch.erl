-module(twitterminer_couch).
-export([connect_server/0, open_db/1, save_doc/3, open_doc/2, read_value/3, update_value/4, extend_doc/4, save_to_doc/5, doc_exists/2, db_save/2]).
 %TODO: When saving to database: First check if Key already exists. If so: Update Value instead of creating Key, Value. (Check if there's already
 %a built-in support for this - can two keys have the same value, or are they merged automatically?)
connect_server() ->
	couchbeam:server_connection("127.0.0.1", 5984, "", []).

open_db(Name) ->
	couchbeam:open_db(connect_server(), Name).

save_doc(CountryID, DocName, DbName) ->
    {ok, Db} = open_db(DbName),
    couchbeam:save_doc(Db, {[{<<"_id">>, CountryID},{<<"title">>, DocName}]}).

open_doc(DbName, DocID) ->
    {ok, Db} = open_db(DbName),
    couchbeam:open_doc(Db,DocID).

read_value(DbName, DocID, Key) -> 
	{ok, Doc} = open_doc(DbName, DocID),
	Value = couchbeam_doc:get_value(Key, Doc),
	io:format("value: ~p~n", [Value]).

update_value(Key, Value, DocID, DbName) ->
	{ok, Db} = open_db(DbName),
	{ok, Doc} = open_doc(DbName, DocID),
    OriginalValue = couchbeam_doc:take_value(Key, Doc),
    NewValue = OriginalValue ++ Value,
	Revision = couchbeam_doc:set_value(Key, NewValue, Doc),
	couchbeam:save_doc(Db, Revision).

extend_doc(Key, Value, DocID, DbName) -> 
	{ok, Db} = open_db(DbName),
	{ok, Doc} = open_doc(DbName, DocID),
	Revision = couchbeam_doc:extend(Key, Value, Doc),
	couchbeam:save_doc(Db, Revision).


save_to_doc(Key, Value, DbName, DocID, CountryName) ->
	case doc_exists(DbName, DocID) of
		false -> save_doc(DocID, CountryName, DbName),
                 extend_doc(Key, Value, DocID, DbName); %create the key where our value will be updated
		true -> ok
	end,
    update_value(Key, Value, DocID, DbName). %update value
	%extend_doc(Key, Value, DocID, DbName).

doc_exists(DbName, DocID) ->
	{ok, Db} = open_db(DbName),
	couchbeam:doc_exists(Db, DocID).

db_save(L, Db) ->
    T = find_value:find_time(L),
    CN = find_value:find_country_name(L),
    CID = find_value:find_country_id(L),
        case CID of
        	ok -> ok;
        	_  ->
            	H = find_value:find_hashlist(L),
            	case H of
                	ok -> ok;
                	_  ->
                		AggList = [T, CID, H],
                		io:format("~p~n", [AggList]),
                		save_to_doc("data", H, Db, CID, CN); %hardcoded key
                		%W = [T, C, H],
                        %io:format("~p~n", [W]);
                  	false -> ok
                end;
            false -> ok
        end.