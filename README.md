CFGrep
==========

Very simple "grep" for Cold Fusion or other tag based language

Usage:
```
$ cfgrep cfquery mycode.cfm
```

Sample output
```
<cfquery name="content_fetch" datasource="#dave.datasource#" username="#dave.username#" password="#dave.password#">
SELECT * FROM DAVE_CONTENT
WHERE PAGE_ID = '#url.page#' AND URL = '#NAME#'
</cfquery>
<cfquery name="checkEvent" datasource="#dave.datasource#" username="#dave.username#" password="#dave.password#" timeout="5" maxrows="1">
select * from DAVE_STREAMS where eventID =
<cfqueryparam cfsqltype="cf_sql_integer" value="#url.event#">
and public = 1
and (eventID = 0 or bambuserID is not null)
</cfqueryparam></cfquery>
```

The pattern is a full jQuery CSS selector. Some useful queries:
```
cfgrep "cfquery[username]" *.cfm
```
(find all cfquery with username parameters)
```
cfgrep "cfquery cfqueryparam" *.cfm
```
(find all cfqueryparam tags that are descendents of cfquery tags)

Building
========


```
$ cabal configure
$ cabal install --only-dependencies
$ cabal install
```
You'll get a binary cfgrep in ~/.cabal/bin (Linux) or ~/Library/Haskell/bin (OS X)

Binary Version
==============

Binary for Ubuntu 12.04 LTS 64 bit

http://files.clune.org/cfgrep.gz   (2.8Mb gzip compressed binary, SHA256 e82d7cf5a173989a3b370dcbe630d8db447353155fcf7eaa23f06b76c4fa5f73)

Bugs/Limitations
================

Currently the file extensions searched for when using "-r" are limited to .cfc and .cfm



