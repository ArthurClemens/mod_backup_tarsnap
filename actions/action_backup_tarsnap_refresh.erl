-module(action_backup_tarsnap_refresh).
-include("zotonic.hrl").
-export([
    render_action/4, 
    event/2
]).

render_action(TriggerId, TargetId, _Args, Context) ->
	{PostbackMsgJS, _PickledPostback} = z_render:make_postback(refresh, undefined, TriggerId, TargetId, ?MODULE, Context),
	{PostbackMsgJS, Context}.
	
	
%% @spec event(Event, Context1) -> Context2
event(#postback{message=refresh}, Context) ->
    case z_acl:is_allowed(use, mod_backup_tarsnap, Context) of
        true ->
            Context1 = z_render:growl("Requested a new list. This can take a while.", Context),
            case mod_backup_tarsnap:refresh(Context1) of
                {ok, Archives} ->
                    Html = z_template:render("_backup_tarsnap_archives.tpl", [{archives, Archives}], Context1),
                    z_render:update("backup_tarsnap_archives", Html, Context1);
        	    {error, Reason} ->
        	        z_render:growl_error(Reason, Context1)
        	end;
        false ->
            z_render:growl_error("Only administrators can refresh this list.", Context)
    end.

