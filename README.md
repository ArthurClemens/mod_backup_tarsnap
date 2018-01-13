# Zotonic module to create backups with Tarsnap

**Update: this module is obsolete (the rotation scheme causes a lot of mutations, driving up costs when storing data via the Tarsnap service and Amazon S3). Use [mod_rotation_backup](https://github.com/ArthurClemens/mod_rotation_backup) instead.**


Manage backups of database and files to the Tarsnap online backup service ("Online backups for the truly paranoid"). Because backups stored on your own webserver are not safe enough.

* Automatically creates backups and stores them - outside of your own server - with Tarsnap.
* Manages a gfc (grandfather-father-child) backup scheme.
* Uses configurable backup schema, for example "6h 1d 1w 1m 1y".
* Creates separate backups for database and files, optionally with different backup schemas.
* Automatically removes expired archives.

Inspired by [Tarsnapper](https://github.com/miracle2k/tarsnapper), with improvements to interval handling.


## The backup schema

tldr; the default values will maintain 7 daily backups, 4 weekly backups, 12 monthly backups, and after that one backup for each year.

The backup schema is defined by time interval ranges (default: "1d 1w 1m 1y" - but they can be set to any other time values). The first value defines the frequency of the backups; the other values define how many backups should be kept.

For the default setting, one backup will be made every day.

The interval `1d - 1w` means: keep 1 daily backup up to 1 week, so 7 backups of the past week.

Backups older than the first interval are handles according to the next interval. The interval `1w - 1m` means: keep 1 weekly backup up to 1 month, so 4 backups of the past month.

The same goes for the next interval `1m - 1y`: keep 1 monthly backup up to 1 year, so 12 backups of the past month.

The final interval can be read as "1y until the end of time": keep 1 yearly backup.


* The smallest interval defines when new backups should be created: as soon as the most recent backup is older than this value (default: 1 day).
* You can use  both `120` and `2h` for 2 hours; `3d` for 3 days; `6m` for six months; and so on. The minimum interval is `10` (minutes) to reduce the load on the server and to prevent overlapping backup tasks.


## Archive creation

* Backup names follow the scheme: identifier-job-date-time. For example: `mysite-database-20141231-065959`.
* The backup name does not contain information about the interval it belongs to (f.i. "WEEKLY");  the date in the name is used to infer that information. The date is 'universal time', written as `dddddd-tttttt`.
* Jobs for Zotonic backups are: `database` and `files`.
* A new backup is created as soon as the most recent backup is older than the first interval.
* Backups will not be skipped if you activate the module later in the day: when it detects that the most recent backup is older, a new backup is created.
* Interval settings can be changed at any time.


## Archive expiration
 
* Only archives with the same identifier are considered; archives created for other sites or using different naming schemes are ignored.
* The date in the archive name is used to infer expiration dates. 
* Calculation starts at the longest interval value (default 1 year). The archive that is closest to that date (the current date minus the interval) is marked as "to keep". Proximity is calculated with a range of plus/minus half an interval (in the example plus or minus half a year).
  * If older archives exist, we go further back in time (the interval value); this process continues until no older archives are found.
  * Archives older than that first interval value that are not marked as "to keep" are marked as "to expire".
* Then the second longest interval is used, until all intervals have been processed.
* The most recent archive is always kept.


## Some questions you might have

### I am seeing more archives than I was expecting

tl;dr: These are extra items to preserve archives when they gradually migrate from new to old.

Internally, archives are grouped into time buckets. When an item expires from a time bucket, it will move to the next bucket (for instance from "days" to "weeks"). This newer archive ("new" from the older bucket point of view) will be kept as a next generation item, until a next item comes along. This mechanism preserves a fresh flow from new to old. 


## Configuration

### Intervals

* The interval range is set with config key `interval` for module `mod_backup_tarsnap`.
* If not set, the default value will be used: `1d 1w 1m 1y`.
* Intervals are default set for all jobs, or can be further specified for each job: `interval_files` and `interval_database`.

These are the default values in /admin/config:

| Module | Key | Default value |
|--------|-----|-------|
| `mod_backup_tarsnap` | `interval`          | `1d 1w 1m 1y`  |
| `mod_backup_tarsnap` | `interval_files`    | `1d 1w 1m 1y`  |
| `mod_backup_tarsnap` | `interval_database` | `1d 1w 1m 1y`  |


### Archive identifier

The default identifier is the site's host name. You can change that with key `identifier`:

| Module | Key | Default value |
|--------|-----|-------|
| `mod_backup_tarsnap` | identifier          | [host name]  |


### Debug info

If you are running Zotonic in debug mode, let the module write debug info to the console when set to `true`:

| Module | Key | Default value |
|--------|-----|-------|
| `mod_backup_tarsnap` | `debug`          | -  |


### Testing backups

If you are running backups on your local machine:

* Use a different identifier, for instance "test-[your host name]"
* Deactivate your local mod_backup_tarsnap before running it on the live server. See "Known problems". Or use a different key for testing.


## Installation

### Requirements

* Zotonic 0.11 or higher
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

    zotonic modules install mod_backup_tarsnap


### Activate

Activate this module in Admin > System > Modules. 


## Known problems

* It is a bad idea to run backups on different machines using the same key. Tarsnap's cache directory gets easily mixed up and backups will no longer being made. Steps to fix:
  * Deactivate mod_backup_tarsnap on your local machine.
  * On the live server, empty the cache dir and rebuild using ``tarsnap --fsck``
* Tarsnap with multiple sites may raise errors sometimes because of concurrency problems: "You can't run two {create, delete} operations at the same time using the same cache directory".
