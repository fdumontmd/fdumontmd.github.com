---
layout: post
title: "Seven Databases in Seven Weeks CouchDB Day 1"
date: 2012-01-30 13:57
comments: true
categories: [Books]
tags: [7databases7weeks, databases, couchdb]
series: "Seven Databases in Seven Weeks"
---

Another beta version of the book, finally with the chapter on
[CouchDB](http://couchdb.apache.org/). I was going through
the Redis chapter, but the third day uses other databases, in
particular CouchDB. So I'll get back to Redis after I'm done with
CouchDB.

<!--more-->

Today is just a short introduction: CouchDB is (yet another) key-value
store; it has a ReST API, stores JSON data, and, like Riak, only
supports full updates. Unlike Riak, however, it does not support
concurrent updates; instead it requires the client to only update from
the latest version of the data.

I thought at first that the data was versioned, like in HBase, but
this is not the case: the version id (`_rev`) is there to ensure that
updates occur sequentially, not concurrently. CouchDB can keep
previous versions of documents, but the retention is unreliable as
explained [here](http://wiki.apache.org/couchdb/Document_revisions).

Besides the HTTP based ReST API, CouchDB also provides a web
interface; among other tools, there is a complete test suite, which is
always nice to check the installation.

## Exercises

### CouchDB HTTP Document API documentation

The documentation is
[here](http://wiki.apache.org/couchdb/HTTP_Document_API); there is
also a [reference](http://wiki.apache.org/couchdb/Complete_HTTP_API_Reference)

### HTTP commands

Besides the basic CRUD `POST` `GET` `PUT` and `DELETE`, there is also
`HEAD` (for basic information on a document):

```
$ curl -I -X HEAD http://localhost:5984/music/ee6637073ab24aaeeda094dcb3749a22 
HTTP/1.1 200 OK
Server: CouchDB/1.1.1 (Erlang OTP/R15B)
Etag: "4-e70582ded641cebc5b259da96805344b"
Date: Mon, 30 Jan 2012 09:18:44 GMT
Content-Type: text/plain;charset=utf-8
Content-Length: 246
Cache-Control: must-revalidate
```

When using `cURL`, the command `HEAD` must be used with the flag `-I`,
otherwise `cURL` will wait (endlessly) for data after the headers.

Finally, there is a `COPY` command, which as expected copies a
document (without having to retrieve it first):

```
$ curl -X COPY  http://localhost:5984/music/ee6637073ab24aaeeda094dcb3749a22 \
-H 'Destination: beatles'
{"id":"beatles","rev":"1-6ea1608de6609c9985ff06aa9bc23a16"}
$ curl http://localhost:5984/music/beatles | python -mjson.tool
  % Total    % Received % Xferd  Average Speed   Time    Time     Time  Current
                                 Dload  Upload   Total   Spent    Left  Speed
100   221  100   221    0     0  78396      0 --:--:-- --:--:-- --:--:--  215k
{
    "_id": "beatles", 
    "_rev": "1-6ea1608de6609c9985ff06aa9bc23a16", 
    "albums": [
        {
            "title": "Help!", 
            "year": 1965
        }, 
        {
            "title": "Sgt. Pepper's Lonely Hearts Club Band", 
            "year": 1967
        }, 
        {
            "title": "Abbey Road", 
            "year": 1969
        }
    ], 
    "name": "The Beatles"
}
```

### `PUT` a new document with a specific `_id`

It is just a matter of specifying an id when creating the document:

```
$ curl -i -X PUT http://localhost:5984/music/sonic_youth \
-H "Content-Type: application/json" --data @-
{
        "name": "Sonic Youth",
        "albums": [
                { "title": "Bad Moon Rising", "year": 1985
                },
                { "title": "Daydream Nation", "year": 1988
                },
                { "title": "Goo", "year": 1990
                }
]               
}       
HTTP/1.1 201 Created
Server: CouchDB/1.1.1 (Erlang OTP/R15B)
Location: http://localhost:5984/music/sonic_youth
Etag: "1-69886eb003b1f007cabaac678d5edc16"
Date: Mon, 30 Jan 2012 09:35:37 GMT
Content-Type: text/plain;charset=utf-8
Content-Length: 74
Cache-Control: must-revalidate

{"ok":true,"id":"sonic_youth","rev":"1-69886eb003b1f007cabaac678d5edc16"}
```
### Document with a text attachment

To create an attachment, it is necessary to know the version of the
document, as it is considered an update. The URL for the attachment is
just the URL for its document, with any suffix (the suffix naming the
attachment). The `_rev` is specified by passing a `rev` parameter.

Using the document with `_id` 'beatles' created above, the attachment
is uploaded with:

```
$ curl -i -X PUT http://localhost:5984/music/beatles/lyrics?rev=1-6ea1608de6609c9985ff06aa9bc23a16 \
-H "Content-type: text/plain" --data @-
It was twenty years ago today
Sgt. Pepper taught the band to play...
Ctrl-D

HTTP/1.1 201 Created
Server: CouchDB/1.1.1 (Erlang OTP/R15B)
Location: http://localhost:5984/music/beatles/attachment
Etag: "2-2b22345fd492f31e3061e23a2b79fc08"
Date: Mon, 30 Jan 2012 09:41:51 GMT
Content-Type: text/plain;charset=utf-8
Content-Length: 70
Cache-Control: must-revalidate

{"ok":true,"id":"beatles","rev":"2-2b22345fd492f31e3061e23a2b79fc08"}
```

The document now has a new `_rev`.

To retrieve the attachment, just use its URL:

```
$ curl http://localhost:5984/music/beatles/lyrics
It was twenty years ago todaySgt. Pepper taught the band to play...
```

(the line breaks have been lost...)

Onward to Day 2!
