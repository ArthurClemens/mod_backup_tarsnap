{% extends "admin_base.tpl" %}

{% block title %} {_ Tarsnap Backups _} {% endblock %}

{% block content %}
    {% with m.acl.is_admin as is_editable %}
        <div class="admin-header">
            <h2>{_ Tarsnap Backups _}</h2>
        </div>
        <div class="row">
            <div class="col-lg-8 col-md-6">
                <div id="backup_tarsnap_archives">
                    {% include "_backup_tarsnap_archives.tpl"
                        archives=archives
                    %}
                </div>                
            </div>
            <div class="col-lg-4 col-md-6">
                {% if not backup_config.ok %}
                    <div class="well">
                        {% if backup_config.ok and is_editable %}
                            {#
                            {% button class="btn btn-primary" text=_"Create extra backup" action={backup} %}
                            {% button class="btn btn-default" text=_"Refresh list" action={refresh} %}
                            #}
                        {% elseif not backup_config.ok %}
                            <div class="alert alert-danger">
                                <strong>{_ Warning: _}</strong> {_ Your backup is not correctly configured. The backup module will not work until the problem(s) below have been resolved: _}
                                <ul>
                                    {% if not backup_config.db_dump %}<li>{_ The "pg_dump" command was not found in the path. Set the "pg_dump" config key to the path to pg_dump and return to this page. _}</li>{% endif %}
                                    {% if not backup_config.archive %}<li>{_ The "tar" command was not found in the path. Set the "tar" config key to the path to tar and return to this page. _}</li>{% endif %}
                                    {% if not backup_config.tarsnap %}<li>{_ The "tarsnap" command was not found in the path. Set the "tarsnap" config key to the path to tarsnap and return to this page. _}</li>{% endif %}
                                    {% if not backup_config.tarsnap_cfg %}<li>{_ Tarsnap is not configured properly. Make sure you have set "tarsnap.conf" or "~/.tarsnaprc" that defines "cachedir" and "keyfile", then return to this page. _}</li>{% endif %}
                                </ul>
                            </div>
                        {% endif %}
                    </div>
                {% endif %}
            </div>
        </div>
    {% endwith %}
{% endblock %}
