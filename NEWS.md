enit
====

Changes with 0.2.7 - 03 Feb 2015
--------------------------------

* extend Documentation

Changes with 0.2.7 - 25 Nov 2014
--------------------------------

* make started applications as permanent, as they should stop the vm, when application is stoped

Changes with 0.2.6 - 16 Jul 2014
--------------------------------

* change application start-up, before the list of applications was sorted
  before resolving the dependencies. This could lead to applications that
  had no other dependencies to be started last. Now, the order as specified
  in the release.enit is kept.

Changes with 0.2.5 - 15 May 2014
--------------------------------

* document the enit:apply_config and enit:get_config functions
* setting cookie not in cmd (for security reason)
* use native erlang resolver, instead of dns

Changes with 0.2.4 - 21 Feb 2014
--------------------------------

* add enit:call for rpc on an enit nodes

Changes with 0.2.3 - 20 Dec 2013
--------------------------------

* add -v option to see the enit version

Changes with 0.2.2 - 20 Nov 2013
--------------------------------

* add possibility to debug with redbug the application or enit themself

Changes with 0.2.1 - 11 Nov 2013
--------------------------------

* use precompiled escript

Changes with 0.2.0 - 19 Sep 2013
--------------------------------

* reset the versioning to "Semantic Versioning"
* adding a NEWS.md
