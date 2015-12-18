-module(twitterminer_couch).
-export([connect_server/0, open_db/1, save_doc/3, open_doc/2, read_value/3, update_value/4, extend_doc/4, save_to_doc/5, doc_exists/2, db_save/2]).

%Connect to CouchDB
connect_server() ->
	couchbeam:server_connection("127.0.0.1", 5984, "", []).

%Open databse with name Name
open_db(Name) ->
	couchbeam:open_db(connect_server(), Name).

%Save a document, given the document ID, document name, and name of the database
save_doc(DocID, DocName, DbName) ->
    {ok, Db} = open_db(DbName),
    couchbeam:save_doc(Db, {[{<<"_id">>, DocID},{<<"title">>, DocName}]}).

%Open a document, given the name of the database and document ID
open_doc(DbName, DocID) ->
    {ok, Db} = open_db(DbName),
    couchbeam:open_doc(Db,DocID).

%Read (and print) the value of key Key in document DocID in database DbName
read_value(DbName, DocID, Key) -> 
	{ok, Doc} = open_doc(DbName, DocID),
	Value = couchbeam_doc:get_value(Key, Doc),
	io:format("value: ~p~n", [Value]).

%Increment Key's value with Value
update_value(Key, Value, DocID, DbName) ->
	{ok, Db} = open_db(DbName),
	{ok, Doc} = open_doc(DbName, DocID),
    OriginalValue = couchbeam_doc:take_value(Key, Doc),
    NewValue = OriginalValue ++ Value,
	Revision = couchbeam_doc:set_value(Key, NewValue, Doc),
	couchbeam:save_doc(Db, Revision).

%Create a new Key-Value in document DocID
extend_doc(Key, Value, DocID, DbName) -> 
	{ok, Db} = open_db(DbName),
	{ok, Doc} = open_doc(DbName, DocID),
	Revision = couchbeam_doc:extend(Key, Value, Doc),
	couchbeam:save_doc(Db, Revision).

%
save_to_doc(Key, Value, DbName, DocID, CountryName) ->
	case doc_exists(DbName, DocID) of
        %If the document doesn't exist, create it and extend it with Key, Value
		false -> save_doc(DocID, CountryName, DbName),
                 extend_doc(Key, Value, DocID, DbName);
        %If document exists, update the value (in this version, the key is always the same)
		true -> update_value(Key, Value, DocID, DbName)
	end.
    update_value(Key, Value, DocID, DbName).
	%extend_doc(Key, Value, DocID, DbName).

%Checks if document DocID exists in DbName, returns true or false.
doc_exists(DbName, DocID) ->
	{ok, Db} = open_db(DbName),
	couchbeam:doc_exists(Db, DocID).

%Main function. Calls functions for finding the desirable data.
db_save(L, Db) ->
    T = find_value:find_time(L),
    CN = find_value:find_country_name(L),
    CID = find_value:find_country_id(L),
        case CID of
        	ok -> ok;
            %If a country ID (country code) exists, go on and look for hashtags.
        	_  ->
            	H = find_value:find_hashlist(L),
            	case H of
                	ok -> ok;
                    %If hashtags also exist, print the aggregated data to terminal (for clarity)
                    %And save it to the appropriate document in the database with the key "data" (in this version)
                	_  ->
                		AggList = [T, CID, H],
                		io:format("~p~n", [AggList]),
                		save_to_doc("data", H, Db, CID, CN); %hardcoded key
                  	false -> ok
                end;
            false -> ok
        end.