-module(action_backup_tarsnap_backup).
-include("zotonic.hrl").
-export([
    render_action/4, 
    event/2
]).

render_action(TriggerId, TargetId, _Args, Context) ->
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(backup, undefined, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.


%% @spec event(Event, Context1) -> Context2
event(#postback{message=backup}, Context) ->
    case z_acl:is_allowed(use, mod_backup_tarsnap, Context) of
        true ->
            case mod_backup_tarsnap:backup(Context) of
                ok ->
        	        z_render:growl("Started the backup. You can keep this page open or continue working.", Context);
        	    {error, in_progress} ->
        	        z_render:growl_error("Could not start the backup because a backup is already in progress.", Context)
        	end;
        false ->
            z_render:growl_error("Only administrators can start a backup.", Context)
    end.

