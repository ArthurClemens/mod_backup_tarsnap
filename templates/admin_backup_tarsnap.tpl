{% extends "admin_base.tpl" %}

{% block title %} {_ Tarsnap Backup _} {% endblock %}

{% block content %}
    {% with m.acl.is_admin as is_editable %}
        <div class="admin-header">
            <h2>{_ Tarsnap Backup _}</h2> 
            <p>
                {_ Manage backups of database and files to the Tarsnap online backup service. _}
            </p>
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
                {% if backup_config.busy %}
                    <div class="alert alert-warning" role="alert">
                        {_ Backup is working. _}
                    </div>
                {% elseif backup_config.ok %}
                    <div class="alert alert-success" role="alert">
                        {_ Backup is standby. _}
                    </div>
                {% else %}
                    {% if backup_config.ok and is_editable %}
                        <div class="well">
                            {#
                            {% button class="btn btn-primary" text=_"Create extra backup" action={backup} %}
                            {% button class="btn btn-default" text=_"Refresh list" action={refresh} %}
                            #}
                        </div>
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
                {% endif %}
                <p>
                    {_ This page shows the list of automatically created archives. Read the <a href="https://github.com/ArthurClemens/mod_backup_tarsnap">module documentation</a> to change default settings. _}
                </p>
            </div>
        </div>
    {% endwith %}
{% endblock %}
