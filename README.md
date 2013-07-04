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

Useful for spotting SQLi

Building
========


```
$ cabal configure --enable-tests
$ cabal install
```
You'll get a binary pastewatch in ~/.cabal/bin (Linux) or ~/Library/Haskell/bin (OS X)

Binary Version
==============

Binary for Ubuntu 12.04 LTS 64 bit

http://clune.org/cfgrep   (14Mb binary)



