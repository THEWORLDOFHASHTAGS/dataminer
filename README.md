#Twitterminer

#####This app mines data from twitter using its Public Streaming API, and saves relevant tweets into a database. Couchbeam is used to interact with a CouchDB database.

<b>Differences from the dit029-twitter-miner:</b>
Added a module for finding and extracting specific values from within the JSON data, in the form of bitstrings.
Added couchbeam as a dependency.
Added a module for CouchDB interaction (twitterminer_couchbeam).
Edited twitterminer_source to run the new functionality.

###Setup
1. Fill in your credentials (API key, API secret, access token, and access token secret) in twitterminer.config.

2. In src/twitterminer_source, go to row 170 in function my_print and change "testdb" to the name of your CouchDB database.

3. Using rebar, get the dependencies (rebar get-deps). You might have to go into the rebar.config file to change the version(s) if you get a mitchmatch error.

4. Using rebar, compile! (rebar compile)

5. Run the erlang shell:
erl -pa deps/*/ebin -pa ebin -config twitterminer.config

6. Start everything 
application:ensure_all_started(twitterminer)

7. Run twitterminer_source:twitter_example().

If it works, tweets contaning both country information and hashtags will be stored into the specified database. Each country will have its own separate document, using the country code as the document ID (these will be created automatically if they do not already exist).
