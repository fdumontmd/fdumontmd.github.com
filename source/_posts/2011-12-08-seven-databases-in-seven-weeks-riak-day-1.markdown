---
layout: post
title: "Seven Databases in Seven Weeks Riak Day 1"
date: 2011-12-08 10:22
comments: true
categories: [Books]
tags: [7databases7weeks, databases, riak]
series: "Seven Databases in Seven Weeks"
---
The second database is [Riak](http://basho.com/products/riak-overview/), a key-value distributed store. Key-value stores a not really new (many property or configuration files are really basic key-value stores, and [Berkeley DB](http://en.wikipedia.org/wiki/Berkeley_DB) has long been a good choice for those who needed something a little bit more robust than simple files but not as complex as a relational database).

Still, going from this simple concept to a distributed store that can execute data processing on each of its nodes requires more than simply scaling things up, and I hope that this book will give me some idea of how such a store (and the other NoSQL) might fit in the solution landscape.

<!--more--> 
But that is probably getting a bit ahead of myself; right now I'd be happy just to know how to use Riak.

The client can be the simple [`cURL`](http://curl.haxx.se/) command, as Riak's interface is based on HTTP. This simplifies the technical stack, but pushes some of the complexity on the client. It is clear that Riak will not provide anything as easy and convenient as PostgreSQL's `psql`.

Riak's basic API is a REST based CRUD (with Create being pretty much the same as Update). Additional attributes, such as meta-data or the more important links are passed as headers in the HTTP request.

It is simple, but somewhat inconvenient: there is no concept of partial update. When you want to update an object, you need to pass all the relevant data: meta-data, links, and content. Forget to mention one, and Riak will forget it too.

### When was this book written?

I had [noticed]() that the book refers to PostgreSQL 9.0 when [9.1](http://www.postgresql.org/docs/9.1/static/release-9-1-2.html) has been out for a while. In this chapter on Riak, the author uses an apparently old format for the URLs, `/riak/bucket/key`, whereas the [official documentation](http://wiki.basho.com/HTTP-Store-Object.html) recommends `/buckets/bucket/keys/key` (for instance, `/buckets/animals/keys/polly`). Both formats can be used and are interoperable, but there is no need to teach already deprecated formats.

Of course, I found out about this new format after I completed all the exercises for today. So I will still use the old format for today.

Presumably this will be fixed by the time the book gets published.

### Simple but useful trick

Reading unformatted JSON data can be difficult. I found that Python provides a simple way to pretty print JSON output:

```
python -mjson.tool
```

(there are certainly other tools. Python is just the first one I came upon). To turn this into a simple, easy to use command, I added this to my `.profile` file:

```
alias ppjs='python -mjson.tool'
alias ppjsless='python -mjson.tool | less'
```

That way, I can just pipe the output of `curl` into `ppjsless`:

```
curl http://localhost:8091/stats | ppjsless
``` 

This unfortunately does not work with `curl` additional output (such as HTTP headers).

Exercises
---------

### Online documentation

The root of the wiki is [here](http://wiki.basho.com/), and the HTTP (not just REST) API is [here](http://wiki.basho.com/HTTP-API.html).

### Mime types

As always, [Wikipedia](http://en.wikipedia.org/wiki/Internet_media_type) is very useful.

### Differences between the dev1, dev2 and dev3 servers

The only difference is the port number. But there is some intelligence in the startup script to map each server to its own directory for permanent storage.

### Link from Polly to her picture

This creates the link. Note, as I mentioned above, that the content needs to be repeated. Putting no body would cause a `curl` error (as an HTTP `PUT` request must have a body):

```
curl -X PUT http://localhost:8091/riak/animals/polly \
-H "Content-type: application/json" \
-H "Link: </riak/photo/polly.jpg>; riaktag=\"photo\"" \
-d '{"nickname" : "Sweet Polly Purebred", "breed" : "Purebred"}'
```

The image can be retrived from Polly by following the link:
```
http://localhost:8091/riak/animals/polly/_,photo,_
```
or, using the new format:
```
http://localhost:8091/buckets/animals/keys/polly/_,photo,_
```

### `POST` a new type of document

Here I upload the Seven Databases in Seven Weeks (legal) PDF:
```
curl -i -X POST http://localhost:8091/riak/documents \
-H "Content-type: application/pdf" \
--data-binary @seven-databases-in-seven-weeks_b1_0.pdf 
```

I use the `-i` option to retrieve the HTTP headers of the response and get the generated key. The command above has this output:
```
HTTP/1.1 100 Continue

HTTP/1.1 201 Created
Vary: Accept-Encoding
Server: MochiWeb/1.1 WebMachine/1.9.0 (someone had painted it blue)
Location: /riak/documents/6WCpgTjMpvHiojiHeH7vsGBXdHC
Date: Thu, 08 Dec 2011 09:16:18 GMT
Content-Type: application/pdf
Content-Length: 0
```

Otherwise, I could list the keys for this bucket:
```
curl http://localhost:8091/riak/documents?keys=true | ppjs

{
    "keys": [
        "6WCpgTjMpvHiojiHeH7vsGBXdHC"
    ], 
    "props": {
        "allow_mult": false, 
        "basic_quorum": false, 
        "big_vclock": 50, 
        "chash_keyfun": {
            "fun": "chash_std_keyfun", 
            "mod": "riak_core_util"
        }, 
        "dw": "quorum", 
        "last_write_wins": false, 
        "linkfun": {
            "fun": "mapreduce_linkfun", 
            "mod": "riak_kv_wm_link_walker"
        }, 
        "n_val": 3, 
        "name": "documents", 
        "notfound_ok": true, 
        "old_vclock": 86400, 
        "postcommit": [], 
        "pr": 0, 
        "precommit": [], 
        "pw": 0, 
        "r": "quorum", 
        "rw": "quorum", 
        "small_vclock": 10, 
        "w": "quorum", 
        "young_vclock": 20
    }
}
```

I can use the URL below to retrieve the document in a browser:
```
http://localhost:8091/riak/documents/6WCpgTjMpvHiojiHeH7vsGBXdHC
```

### `PUT` a medecine image and link to Ace

Once again, nothing too complex, but everything has to be done at the same time, as partial updates are not possible:

```
curl -X PUT http://localhost:8091/riak/medecine/antibiotics \
-H "Content-type: image/jpeg" -H "Link: </riak/animals/ace>; riaktag=\"for\"" \
--data-binary @medecine.jpg 
```

Then the image itself can be retrieve at:
```
http://localhost:8091/riak/medecine/antibiotics
```

Finally, I can get the poor patient by following links:
```
$ curl http://localhost:8091/riak/medecine/antibiotics/animals,for,_

--ZhESIca7K0r54xNT0w737ZZDbvl
Content-Type: multipart/mixed; boundary=WZgHw2YUOMNtda2iM46bNdvXxt7

--WZgHw2YUOMNtda2iM46bNdvXxt7
X-Riak-Vclock: a85hYGBgzGDKBVIcR4M2cvvdjT+YwZTImsfKUDcx5ARfFgA=
Location: /riak/animals/ace
Content-Type: application/json
Link: </riak/animals>; rel="up"
Etag: 6egZ1heUAPW7DEy0HjO7K0
Last-Modified: Thu, 08 Dec 2011 01:40:14 GMT

{"nickname" : "The Wonder Dog", "breed" : "German Shepherd"}
--WZgHw2YUOMNtda2iM46bNdvXxt7--

--ZhESIca7K0r54xNT0w737ZZDbvl--
```

And this completes Day 1. The basic REST API is not complex, but its simplicity cuts both ways. There is a lot of typing required; I expect client libraries to be much easier to use, at the cost of having to write an application or script to do anything.

Tomorrow will cover MapReduce in the context of Riak.