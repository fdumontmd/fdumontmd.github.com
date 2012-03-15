/***
 * Excerpted from "Seven Databases in Seven Weeks",
 * published by The Pragmatic Bookshelf.
 * Copyrights apply to this code. It may not be used to create training material, 
 * courses, books, articles, and the like. Contact us if you are in doubt.
 * We make no guarantees that this code is fit for any purpose. 
 * Visit http://www.pragmaticprogrammer.com/titles/rwdata for more book information.
***/

var
  port = 8080,
  streamPort = 8089,
  host = 'localhost',
  jsonHeader = {'Content-Type':'application/json'},

  // standard libraries
  http = require('http'),
  redis = require('redis'),
  bricks = require('bricks'),
  mustache = require('mustache'),
  fs = require('fs'),
  url = require('url'),

  // mongodb
  mongodb = require('mongodb'),

  // custom libraries
  couchUtil = require('./populate_couch.js'),
  neo4j = require('./neo4j_caching_client.js'),

  // database clients
  couchClient = http.createClient(5984, 'localhost'),
  neo4jClient = neo4j.createClient(),
  redisClient = redis.createClient(6379),

  mongoClient = 
    new mongodb.Db('music', 
                   new mongodb.Server('localhost', 
                                      mongodb.Connection.DEFAULT_PORT, {}),
                   {native_parser:true});

var
  gremlin = neo4jClient.runGremlin;

/**
 * A convenience function for wrapping the
 * reading of JSON reponse data chunks.
 * @param response A Node HTTP response object.
 * @param callback the function to populate and call on completion.
 */
function processBuffer( response, callback )
{
  var buffer = '';
  response.on('data', function(chunk) {
    buffer += chunk;
  });
  response.on('end', function() {
    if(buffer === '') buffer = 'null';
    callback( JSON.parse(buffer) );
  });
};

/*
 * Post one or more documents into CouchDB.
 * @param url is where we POST to.
 * @param docString a stringified JSON document.
 * @param count the number of documents being inserted.
 */
function getCouchDoc( path, httpResponse, callback )
{
  var request = couchClient.request( 'GET', path, jsonHeader );
  request.end();
  request.on('response', function( response ) {
    if( response.statusCode != 200 ) {
      writeTemplate( httpResponse, '', { message: "Value not found" } );
    } else {
      processBuffer( response, function( couchObj ) {
        callback( couchObj );
      });
    }
  }).
  on('error', function(e) {
    console.log('postDoc Got error: ' + e.message);
  });
};

/**
 * Wraps a block of HTML with a standard template. HTML lives in template.html.
 * @innerHtml populates the body of the template
 */
function htmlTemplate( innerHtml )
{
  var file_data = fs.readFileSync( 'template.html', 'utf8' );
  return file_data.replace("[[YIELD]]", innerHtml);
};

function writeTemplate( response, innerHtml, values )
{
  response.write( mustache.to_html( htmlTemplate( innerHtml ), values ));
  response.end();
}

function convertGremlinTable(table) {
    return fromTableToObject(table[0][0].columns, table[0][0].data);
}

function fromTableToObject(columns, data) {
    var res = [];
    for (var i = 0; i < data.length; i++) {
        var obj = {};
        for (var j = 0; j < columns.length; j++) {
            if (data[i][j] != 'null')
                obj[columns[j]] = data[i][j];
        }
        res.push(obj);
    }

    return res;
}


// A Nodejs web app utility setup
appServer = new bricks.appserver();

// attach request plugin to easily extract params
appServer.addRoute("^/", appServer.plugins.request);

/*
 * Just display a blank form if no band is given.
 */
appServer.addRoute("^/$", function(req, res) {
  writeTemplate( res, '', { message: "Find a band" } );
});

/*
 * Accepts a band name and displays all artists in the band.
 * Also displays a list of suggested bands where at least
 * one artist has played at one time.
 */

/*
 * Gremlin: aliases introduced for the same node and/or
 * relationship appear to be used in reverse order in
 * Tables.
 */
appServer.addRoute("^/band$", function(req, res) {
    var bandName = req.param('name'),
      bandKey = couchUtil.couchKeyify( bandName ),
      bandNodePath = '/bands/' + bandKey,
      queryPrefix = 'g.idx("bands")[["name":"'+bandName+'"]]',
      otherBandsQuery = queryPrefix
            + '.out("member").in("member").dedup.name',
      currentMembersQuery = queryPrefix
            + '.outE("member").as("from")'
            + '.filter{it.to == null}.inV.as("name")'
            + '.table(new Table()){it.from}{it.name}.cap()',
      otherMembersQuery = queryPrefix
            + '.outE("member").as("from").as("to")'
            + '.filter{it.to != null}.inV.as("name")'
            + '.table(new Table())'
            + '{it.to}{it.from}{it.name}.cap()';

    gremlin(otherBandsQuery, function(graphData) {
        gremlin(currentMembersQuery, function(currentMembers) {
            gremlin(otherMembersQuery, function(otherMembers) {
                var values = {band: bandName, bands: graphData,
                             currents: convertGremlinTable(currentMembers),
                             others: convertGremlinTable(otherMembers),
                             bandK: bandKey};
                var body = '<h2>Current {{band}} Band Members</h2>';
                if (currentMembers[0][0].data.length > 0) {
                    body += '<ul>{{#currents}}';
                    body += '<li><a href="/artist?name={{name}}">{{name}}';
                    body += '{{#from}} from {{from}}{{/from}}</a></li>';
                    body += '{{/currents}}';
                } else {
                    body += "<p>No current member (dead band?)</p>";
                }

                mongoClient.open(function(err, db) {
                    mongodb.GridStore.exist(db, bandKey + '.mp3', function(err, exist) {
                        if (exist)
                            body += '<a href="http://'+host+':'+streamPort+'?band={{bandK}}">Sample</a>';
                        if (otherMembers[0][0].data.length > 0) {
                            body += '<h3>Other members</h3>';
                            body += '<ul>{{#others}}';
                            body += '<li><a href="/artist?name={{name}}">{{name}}';
                            body += '{{#from}} from {{from}}{{/from}}';
                            body += '{{#to}} to {{to}}{{/to}}</a></li>';
                            body += '{{/others}}</ul>';
                        }
                        body += '<h3>You may also like</h3>';
                        body += '<ul>{{#bands}}';
                        body += '<li><a href="/band?name={{.}}">{{.}}</a></li>';
                        body += '{{/bands}}</ul>';
                
                        writeTemplate( res, body, values );
                        db.close();
                    });
                });
            });
        });
    });
});

/*
 * Accepts an artist name and displays band and role information
 */
appServer.addRoute("^/artist$", function(req, res) {
    var artistName = req.param('name'),
        queryPrefix = 'g.idx("artists")[["name":"'+artistName+'"]]',
        rolesQuery = queryPrefix +'.out("plays").role.dedup',
        bandsQuery = 'g.idx("artists")[["name":"'+artistName+'"]]'
            + '.inE("member").as("to").as("from").outV.as("name")'
            + '.table(new Table()){it.from}{it.to}{it.name}.cap()';

    gremlin( rolesQuery, function(roles) {
        gremlin( bandsQuery, function(bands) {
            var values = { artist: artistName, roles: roles, 
                           bands: convertGremlinTable(bands) };

            var body = '<h3>{{artist}} Performs these Roles</h3>';
            body += '<ul>{{#roles}}';
            body += '<li>{{.}}</li>';
            body += '{{/roles}}</ul>';
            body += '<h3>Play in Bands</h3>';
            body += '<ul>{{#bands}}';
            body += '<li><a href="/band?name={{name}}">{{name}}';
            body += '{{#from}} from {{from}}{{/from}}';
            body += '{{#to}} to {{to}}{{/to}}</a></li>';
            body += '</a></li>';
            body += '{{/bands}}</ul>';
      
      writeTemplate( res, body, values );
    });
  });
});

/*
 * A band name search. Used for autocompletion.
 */
appServer.addRoute("^/search$", function(req, res) {
  var query = req.param('term');
  
  redisClient.keys("band-name:"+query+"*", function(error, keys) {
    var bands = [];
    keys.forEach(function(key){
      bands.push(key.replace("band-name:", ''));
    });
    res.write( JSON.stringify(bands) );
    res.end();
  });
});

// cannot seem to get bricks to stream data back
// use simple default http server
http.createServer(function(request, response) {
    var band = url.parse(request.url, true).query.band;   
    mongoClient.open(function(err, db) {
        var gs = new mongodb.GridStore(db, band+'.mp3', "r");
        gs.open(function(err, gs) {
            console.log("streaming...");
            response.writeHeader(200, {
                'Content-type': 'audio/mpeg, audio/x-mpeg, audio/x-mpeg-3, audio/mpeg3',
                // magic headers to stream mp3...
                'X-Pad': 'avoid browser bug',
                'Cache-Control': 'no-cache',
                'Content-Length': gs.length});
// cannot use gridstore streams; somehow file always
// truncated - load in memory instead
//            gs.stream(true).pipe(response);
            gs.read(gs.length, function(err, data) {
                response.write(data);
                response.end();
                db.close();
            });  
        });
    });
}).listen(streamPort);

// catch all unknown routes with a 404
appServer.addRoute(".+", appServer.plugins.fourohfour);
appServer.addRoute(".+", appServer.plugins.loghandler, { section: "final" });

// start up the server
console.log("Starting Server on port " + port);
appServer.createServer().listen(port);
