# Zotonic module to create backups with Tarsnap

Manage backups of database and files to the Tarsnap online backup service ("Online backups for the truly paranoid"). Because backups stored on your own webserver are not safe enough.

* Automatically creates backups and stores them - outside of your own server - with Tarsnap.
* Manages a grandfather-father-child backup scheme.
* Uses configurable backup schema, for example "6h 1d 1w 1m 1y".
* Creates separate backups for database and files, optionally with different backup schemas.
* Automatically removes expired archives.

Inspired by [Tarsnapper](https://github.com/miracle2k/tarsnapper).


## Archive creation

* Backup names follow the scheme: identifier-job-date-time. For example: `mysite-database-20141231-065959`.
* The backup name does not contain information about the cycle it belongs to (f.i. "WEEKLY");  the date in the name is used to infer that information. The date is 'universal time', written as `dddddd-tttttt`.
* Jobs for Zotonic backups are: `database` and `files`.
* A new backup is created as soon as the most recent backup is older than the first interval.
* Backups will not be skipped if you activate the module later in the day: when it detects that the most recent backup is older, a new backup is created.
* Interval settings can be changed at any time.


## Intervals

* The backup schema is defined by freeform time interval ranges (default: "1d 1w 1m 1y").
* Each interval value defines a cycle that ends at the next interval; the first cycle runs from 1 day to 1 week old; the second from 1 week to 1 month.
* The default values will maintain 7 daily backups, 4 weekly backups, 12 montly backups, and after that one backup for each year.
* You can use  both `120` and `2h` for 2 hours; `3d` for 3 days; `6m` for six months; and so on. The minimum interval is `10` (minutes) to reduce the load on the server and to prevent overlapping backup tasks.
* The smallest interval defines when new backups should be created: as soon as the most recent backup is older than this value (default: 1 day).


## Archive expiration

* Only archives with the same identifier are considered; archives created for other sites or using different naming schemes are ignored.
* The date in the archive name is used to infer expiration dates. 
* Calculation starts at the longest interval value (default 1 year). The archive that is closest to that date (the current date minus the interval) is marked as "to keep". Proximity is calculated with a range of plus/minus half an interval (in the example plus or minus half a year).
  * If older archives exist, we go further back in time (the interval value); this process continues until no older archives are found.
  * Archives older than that first interval value that are not marked as "to keep" are marked as "to expire".
* Then the second longest interval is used, until all intervals have been processed.
* The most recent archive is always kept.


## Configuration

### Intervals

* The interval range is set with config key `interval` for module `mod_backup_tarsnap`.
* If not set, the default value will be used: `1d 1w 1m 1y`.
* Intervals are default set for all jobs, or can be further specified for each job: `interval_files` and `interval_database`.

These are the default values in /admin/config:

| Module | Key | Default value |
|--------|-----|-------|
| mod_backup_tarsnap | interval          | 1d 1w 1m 1y  |
| mod_backup_tarsnap | interval_files    | 1d 1w 1m 1y  |
| mod_backup_tarsnap | interval_database | 1d 1w 1m 1y  |


### Archive identifier

The default identifier is the site's host name. You can change that with key `identifier`:

| Module | Key | Default value |
|--------|-----|-------|
| mod_backup_tarsnap | identifier          | [host name]  |


### Debug info

If you are running Zotonic in debug mode, let the module write debug info to the console when set to `true`:

| Module | Key | Default value |
|--------|-----|-------|
| mod_backup_tarsnap | debug          | -  |



## Installation

### Requirements

* [Tarsnap](https://www.tarsnap.com) account
* Working Tarsnap configuration (tarsnap.conf or ~/.tarsnaprc) that defines `cachedir` and `keyfile`.

Note that the Zotonic user needs to access configuration files, so you might need to place those locations in the Zotonic user home directory.

#### Example setup

As zotonic user:

    $ mkdir ~/.tarsnap
    $ mkdir ~/.tarsnap/tarsnap-cache
    $ mv /root/tarsnap.key ~/.tarsnap/
    $ vim ~/.tarsnaprc
    
    # Tarsnap cache directory
    cachedir ~/.tarsnap/tarsnap-cache

    # Tarsnap key file
    keyfile ~/.tarsnap/tarsnap.key

As root:

    $sudo chown -R zotonic:zotonic /home/zotonic/.tarsnap/


### Install

Zotonic >= 0.10::

    zotonic modules install mod_bulk_commands
    
Zotonic <= 0.9::

    zotonic installmodule mod_bulk_commands
    
Zotonic <= 0.6::

    git clone https://github.com/ArthurClemens/mod_bulk_commands.git mod_bulk_commands

### Install dependencies

[qdate](https://github.com/choptastic/qdate) is installed with Zotonic 0.11 and 0.12, but for older installations of Zotonic it must be installed separately.

#### Install qdate for Zotonic 0.10

You many need to install `rebar` first if it isn't installed yet. 

From the zotonic root directory:

    rebar get-deps --config priv/modules/mod_backup_tarsnap/rebar.config

This will install the dependencies into Zotonic's `deps` directory.


### Activate

Activate this module in Admin > System > Modules. 


## Known problems

* Tarsnap with multiple sites may raise errors sometimes because of concurrency problems: "You can't run two {create, delete} operations at the same time using the same cache directory". This will be fixed.
