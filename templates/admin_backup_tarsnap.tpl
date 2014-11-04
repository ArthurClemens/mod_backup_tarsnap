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
                        {% for archive, job, date, _status in backups %}
                            <tr>
                                <td>
                                    {{ archive }}
                                </td>
                                <td>
                                    {{ job }}
                                </td>
                                <td>
                                    {{ date }}
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
        </div>
    {% endwith %}
{% endblock %}
