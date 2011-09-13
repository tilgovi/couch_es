couch_es
==========

couch_es is an elasticsearch helper for CouchDB familly helper.
Compatible with [Apache CouchDB](http://couchdb.apache.orh),
[BigCouch](http://github.com/cloudant/bigcouch) and
[Refuge](http://github.com/refuge).


While [Elasticsearch](http://elasticsearch.org) provides the system of
[river](http://www.elasticsearch.org/guide/reference/river/couchdb.html)
it imply you manually set the river in it. Also you don't have an easy
way to query elasticsearch using the CouchDB rest API like you could do
with couchdb lucene or cloudant search (proprietary). 

This helper allows you to:

- index in real time databases in elastic search
- search directly data using the CouchDB search api.
- access to the _collator api.

Installing with CouchDB
-----------------------

To install on last trunk of couchdb you just have to run the following
command line:

    $ make
    $ make install
    ==> ibrowse (get-deps)
    ==> couch_es (get-deps)
    ==> ibrowse (compile)
    ==> couch_es (compile)

    couch_es has been installed in /Users/benoitc/misc/couchdb/lib/couchdb/erlang/lib/couch_es
    To launch it run the commandline:

    ERL_CFLAGS="-pa /path/to/lib/couchdb/erlang/lib/couch_es" couchdb -a /path/to/etc/couchdb/couch_es.ini

*Note*: you need couch-config available in your script.


Installation with CouchDB 1.1x should be done manually. To compile it do
the following


     $ export COUCHDB_SRC=/path/to/sources/src/couchdb
     $ erlc -I $COUCHDB_SRC *.erl

Then launch CouchDB :

    $  ERL_FLAGS="-pa /path/to/couch_es" -a /path/to/couch_es.ini


Installing with bigcouch
------------------------

Installing with bigcouch is easy. First clone bigcouch on your disk:

    $ git clone git://github.com/refuge/bigcouch.git

And do the usual bigcouch installation process.


Test it:
--------

Create a db with one doc.  

    $ curl -XPUT "localhost:5984/testdb"
    $ curl -XPOST "localhost:5984/testdb" -d'{"test": 1}' -H"Content-Type: application/json"


Test to search on a db:

    $ curl -XGET "localhost:5984/testdb/_search?q=test:1&pretty=true"

    {
      "took" : 2,
      "timed_out" : false,
      "_shards" : {
        "total" : 5,
        "successful" : 5,
        "failed" : 0
      },
      "hits" : {
        "total" : 1,
        "max_score" : 1.0,
        "hits" : [ {
          "_index" : "testdb",
          "_type" : "testdb",
          "_id" : "64c9d5c574c9eb8853d07e4b0c000d9a",
          "_score" : 1.0, "_source" : {"_rev":"1-ea7a185b492abc69a6c8e0358d244a98","_id":"64c9d5c574c9eb8853d07e4b0c000d9a","test":1}
        } ]
      }
    }
    
Search globally :

    $ curl -XGET "localhost:5984/_search?q=test:1"

Multi db:

    
    $ curl -XPUT "localhost:5984/testdb1"
    $ curl -XPOST "localhost:5984/testdb1" -d'{"test": 1}' -H"Content-Type: application/json"
    $ curl -XGET "localhost:15984/_search?q=test:1&pretty=true"
    {
      "took" : 11,
      "timed_out" : false,
      "_shards" : {
        "total" : 21,
        "successful" : 21,
        "failed" : 0
      },
      "hits" : {
        "total" : 2,
        "max_score" : 1.0,
        "hits" : [ {
          "_index" : "testdb",
          "_type" : "testdb",
          "_id" : "64c9d5c574c9eb8853d07e4b0c000d9a",
          "_score" : 1.0, "_source" : {"_rev":"1-ea7a185b492abc69a6c8e0358d244a98","_id":"64c9d5c574c9eb8853d07e4b0c000d9a","test":1}
        }, {
          "_index" : "testdb1",
          "_type" : "testdb1",
          "_id" : "64c9d5c574c9eb8853d07e4b0c000e00",
          "_score" : 1.0, "_source" : {"_rev":"1-ea7a185b492abc69a6c8e0358d244a98","_id":"64c9d5c574c9eb8853d07e4b0c000e00","test":1}
        } ]
      }
    } 

    Or specify the db you want to search in:


    $ curl -XGET "localhost:15984/_search/testdb,testdb1?q=test:1&pretty=true"
    {
      "took" : 2,
      "timed_out" : false,
      "_shards" : {
        "total" : 10,
        "successful" : 10,
        "failed" : 0
      },
      "hits" : {
        "total" : 2,
        "max_score" : 1.0,
        "hits" : [ {
          "_index" : "testdb",
          "_type" : "testdb",
          "_id" : "64c9d5c574c9eb8853d07e4b0c000d9a",
          "_score" : 1.0, "_source" : {"_rev":"1-ea7a185b492abc69a6c8e0358d244a98","_id":"64c9d5c574c9eb8853d07e4b0c000d9a","test":1}
        }, {
          "_index" : "testdb1",
          "_type" : "testdb1",
          "_id" : "64c9d5c574c9eb8853d07e4b0c000e00",
          "_score" : 1.0, "_source" : {"_rev":"1-ea7a185b492abc69a6c8e0358d244a98","_id":"64c9d5c574c9eb8853d07e4b0c000e00","test":1}
        } ]
      }
    }


Configuration
-------------

    [couch_es]
    ;backend = couchdb
    ;public_host = localhost
    ;public_port = 5984
    ;bulk_size = 100
    ;bulk_timeout = 10
    ;username = 
    ;password = 
    ;concurrency = 10


backend could be couchdb or bigcouch for now.


