-module(controller_admin_backup_tarsnap).

-export([
    is_authorized/2
]).

-include_lib("controller_html_helper.hrl").

is_authorized(ReqData, Context) ->
    z_admin_controller_helper:is_authorized(mod_backup_tarsnap, ReqData, Context).


html(Context) ->
    Vars = [
        {page_admin_backup_tarsnap, true},
        {backups, mod_backup_tarsnap:list_backups(Context)},
        {backup_config, mod_backup_tarsnap:check_configuration(Context)},
        {backup_in_progress, mod_backup_tarsnap:backup_in_progress(Context)}
    ],
	Html = z_template:render("admin_backup_tarsnap.tpl", Vars, Context),
	z_context:output(Html, Context).

