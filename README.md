Enit provides a dynamic alternative to OTP releases.
It is meant to be the successor to [erlrc](http://code.google.com/p/erlrc/).

Enit is provided under the MIT License.
In order to build enit using [tetrapak](https://github.com/fjl/tetrapak), simply execute

	tetrapak build

in your working copy.

Dynamic, you say?
-----------------

In the OTP release model, you write a release specification
and then generate a release using reltool. OTP releases contain
all applications needed to run the release and a copy of the Erlang
emulator. When you want to update the release, you generate a new
version of it, copy the tarball to the target system, and then upgrade
it using reltool.

With enit, things are different. Enit releases do not bundle any
applications nor the Erlang VM. Instead, you're responsible to
install your applications to the target using any mechanism of
your choice (e.g. Debian packages). You define releases by simply
dropping a release specification into the definition directory,
`/var/lib`. You start and stop your release using the `enit`
command line tool.

This model allows you to upgrade applications separately.
If you run multiple releases on a single host, they can share
the emulator installation and all applications.

Enit also handles bootstrap configuration through the OTP application
environment. If your application supports the
[config_change/3 application callback](http://www.erlang.org/doc/apps/kernel/application.html#Module:config_change-3),
you'll be able to react to configuration changes while the application is running.

Current Limitations
-------------------

* It only works on Linux right now.
* Only shortnames are supported. Long nodenames will not work.
* Hot Upgrades are not supported. You'll need to
  restart the VM to upgrade applications.

Release Specification Example
-----------------------------

We're going to define a release called `relex`.

First we need to create a subdirectory of `/var/lib` with
the same name as the release:

	$ mkdir -p /var/lib/relex

We now add the release definition in `/var/lib/relex/release.enit`:

	{release, relex, [
		{vsn, "1.0"},
		{applications, [
			sasl,
			runtime_tools,
			relex_application
		]}
	]}.

and some configuration defaults in `/var/lib/relex/defaults.config`:

	{node, [
		{run_as_user, "relex"},
		{run_as_group, "relex"},
		{smp, enabled}
	]}.

	{kernel, [
		{start_timer, true},
		{start_disk_log, true}
	]}.

	{sasl, [
		{sasl_error_logger, false},
		{utc_log, true}
	]}.

We also set some host-specific configuration
parameters in the *user configuration file*, `/etc/relex/user.config`:

	{node, [
		{cookie, "monster"}
	]}.

	{kernel, [
		{inet_dist_use_interface, {98,77,23,2}}
	]}.

	{relex_application, [
		{frozzle_timeout, 6000},
		{kabozzle_domain, "relex.com"}
	]}.

That's it. We can now start the release. The *correct* way to do so
is through an init script. Here's a working script for
[Upstart](http://upstart.ubuntu.com), the init daemon used
by Ubuntu. Put this into `/etc/init/relex.conf`:

	start on started
	stop on stopping bigcouch

	env HOME=/root
	exec /usr/bin/enit startfg relex --syslog

	respawn
	respawn limit 5 10

Let's boot the release using upstart:

	$ sudo initctl start relex
	$ enit status relex
	Release: relex
	Version: 1.0
	Node:    relex@cluster02
	Cookie:  monster

	This node is currently online.

	OTP: R14B03
	Pid: 24554
	Uptime: 22sec
	Connected Nodes:
	Running Applications:
	 cowboy        0.4.0.0
	 crypto        2.0.3
	 kernel        2.14.4
	 public_key    0.12
	 relex         1.0.1
	 runtime_tools 1.8.5
	 sasl          2.1.9.4
	 stdlib        1.17.4
	 ssl           4.1.5
	Memory Usage (erlang:memory/0):
	 Total:            17 MiB
	 Processes:        2 MiB
	 Processes (used): 2 MiB
	 System:           14 MiB
	 Atom:             1 MiB
	 Atom (used):      1 MiB
	 Binary:           79 KiB
	 Code:             10 MiB
	 ETS:              1 MiB

Dynamic Configuration Example
-----------------------------

Suppose we want to change the value of `kabozzle_domain` in
`relex_application` to some other value.

Edit `/etc/relex/user.config`:

	...

	{relex_application, [
		{frozzle_timeout, 6000},
		{kabozzle_domain, "relex.changed.com"}
	]}.

	...

We now notify the VM that configuration has changed:

	$ enit reconfigure relex
	reconfigure: config changes applied

Inside the VM, enit calculates the differences between the running
configuration and the config files. It then sets all the application
parameters and calls the `config_change` function in all applications
whose configuration has changed (if they define the callback).

Use enit configuration from another application
---------------------------------------------------

You can use configuration values from enit releases without manually having to extract them from the release's config files.

    enit:apply_config(RELEASE, APPLICATION, [{match, true}])

A release usually configures several applications (node, kernel, ...).
Using enit:configure/3, you choose which application's configuration you want to extract from a release's configuration.

The example above adds all configuration that is done for APPLICATION in the configuration files of RELEASE.

You can then access that configuration using

    ``application:get_env(APPLICATION, KEY).``

where KEY is a parameter that is set in RELEASE's configuration files.

You can also add the configuration of all applications that are configured in a release's configuration by using enit:configure/2:

    enit:apply_config(RELEASE, [{match, true}]).

Read the configuration:

    enit:get_config(RELEASE, [{match, true}]).
    enit:get_config(RELEASE, APPLICATION, [{match, true}]).
