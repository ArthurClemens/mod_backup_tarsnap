# Zotonic module to create backups with Tarsnap

* Manages a grandfather-father-child backup scheme
* Uses configurable time delta ranges, for example "6h 1d 1w 1m 1y"
* Creates backups for database and files
* Automatically removes expired archives

Inspired by [Tarsnapper](https://github.com/miracle2k/tarsnapper).


## Archive creation

* Backup names follow the scheme: identifier-job-date-time. For example: `mysite-database-20141231-065959`.
* The backup name does not contain information about the cycle it belongs to;  the date in the name is used to infer that information. The date always is written as `dddddd-tttttt`.
* Jobs for Zotonic backups are: `database` and `files`.
* A new backup is created as soon as the most recent backup is older than the first delta range.
* Backups will not be skipped if you activate the module later in the day: when it detects that the most recent backup is older, a new backup is created.
* Delta ranges can be changed at any time.


## Cycles

* Cycles are defined by freeform time delta ranges.
* The default delta ranges are "1d 1w 1m 1y".
* Each cycle ends at the next delta range; the first cycle runs from 1 day to 1 week old; the second from 1 week to 1 month.
* The default values will maintain 7 daily backups, 4 weekly backups, 12 montly backups, and after that one backup for each year.
* You can use  both `120` and `2h` for 2 hours; `3d` for 3 days; `6m` for six months; and so on.
* The smallest delta range defines when new backups should be created: as soon as the most recent backup is older than this value (default: 1 day).


## Archive expiration

* The archive names do not include any cycle information. Instead, the date in the archive name is used to infer expiration dates. 
* Calculation starts at the highest delta range (default 1 year). The archive that is closest to that date (the current date minus the delta) is marked as "to keep". Proximity is calculated with a range of plus/minus half a delta (in the example plus or minus half a year).
  * If older archives exist, the next delta is used (the current date minus 2 deltas) until no older archives are found.
  * Archives older than that first delta range that are not marked as "to keep" are marked as "to expire".
* This mechanism moves up the delta ranges until all have been passed.
* The most recent archive is always kept.


## Configuration

### Deltas

* The delta range is set with config key `deltas` for module `mod_backup_tarsnap`
* If not set, the default value will be used: `1d 1w 1m 1y`
* Deltas can be set for the module, or further specified for each job: `deltas_files` and `deltas_database`

These are the default values in /admin/config:

| Module | Key | Value |
|--------|-----|-------|
| mod_backup_tarsnap | deltas          | 1d 1w 1m 1y  |
| mod_backup_tarsnap | deltas_files    | 1d 1w 1m 1y  |
| mod_backup_tarsnap | deltas_database | 1d 1w 1m 1y  |

### Archive identifier

The default identifier is the site's host name. You can change that with key `identifier`:

| Module | Key | Value |
|--------|-----|-------|
| mod_backup_tarsnap | identifier          | [host name]  |


## Installation

### Requirements

* [Tarsnap](https://www.tarsnap.com) account
* Working Tarsnap configuration (tarsnap.conf or ~/.tarsnaprc) that defines `cachedir` and `keyfile`. Note that the Zotonic user needs to access those files, so you might need to place those locations in the Zotonic user home directory.

### Install

Zotonic >= 0.10::

    zotonic modules install mod_bulk_commands
    
Zotonic <= 0.9::

    zotonic installmodule mod_bulk_commands
    
Zotonic <= 0.6::

    git clone https://github.com/ArthurClemens/mod_bulk_commands.git mod_bulk_commands

### Activate

Activate this module in Admin > System > Modules. 