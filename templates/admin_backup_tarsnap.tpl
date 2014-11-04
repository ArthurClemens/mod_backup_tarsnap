{% extends "admin_base.tpl" %}

{% block title %} {_ Tarsnap Backups _} {% endblock %}

{% block content %}
    {% with m.acl.is_admin as is_editable %}
        <div class="admin-header">
            <h2>{_ Tarsnap Backups _}</h2>
        </div>
        <div class="row">
            <div class="col-lg-8 col-md-6">
                <table class="table">
                    <thead>
                        <tr>
                            <th>{_ Archive _}</th>
                            <th>{_ Job _}</th>
                            <th>{_ Date _}</th>
                        </tr>
                    </thead>
                    <tbody>
                        {% for backup in backups %}
                            <tr>
                                <td>
                                    {{ backup.archive }}
                                </td>
                                <td>
                                    {{ backup.job }}
                                </td>
                                <td>
                                    {{ backup.date|date:"M d Y, H:i:s" }}
                                </td>
                            </tr>
                        {% empty %}
                            <tr>
                                <td colspan="3">
                                    {_ No backups present. _}
                                </td>
                            </tr>
                        {% endfor %}
                    </tbody>
                </table>
            </div>
            <div class="col-lg-4 col-md-6">
                <div class="well">
                    {% if backup_config.ok and is_editable %}
                        {% button class="btn btn-primary" text=_"Start backup now" action={backup_start} %}

                    {% elseif not backup_config.ok %}
                        <div class="alert alert-danger">
                            <strong>{_ Warning: _}</strong> {_ Your backup is not correctly configured. The backup module will not work until the problem(s) below have been resolved: _}
                            <ul>
                                {% if not backup_config.db_dump %}<li>{_ The "pg_dump" command was not found in the path. Set the "pg_dump" config key to the path to pg_dump and return to this page. _}</li>{% endif %}
                                {% if not backup_config.archive %}<li>{_ The "tar" command was not found in the path. Set the "tar" config key to the path to pg_dump and return to this page. _}</li>{% endif %}
                            </ul>
                        </div>
                    {% endif %}
                </div>
            </div>
        </div>
    {% endwith %}
{% endblock %}
