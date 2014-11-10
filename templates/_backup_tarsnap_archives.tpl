{#
params:
archives
#}
<table class="table">
    <thead>
        <tr>
            <th>{_ Archive name _}</th>
            <th>{_ Job _}</th>
            <th colspan="2">{_ Backup date _}</th>
        </tr>
    </thead>
    <tbody>
        {% for archive in archives %}
            <tr>
                <td>
                    {{ archive.archive }}
                </td>
                <td>
                    {{ archive.job }}
                </td>
                <td>
                    {{ archive.date|timesince }}
                </td>
                <td>
                    <span class="text-muted">{{ archive.date|date:"M d Y, H:i:s" }}</span>
                </td>
            </tr>
        {% empty %}
            <tr>
                <td colspan="4">
                    {_ No archives present. _}
                </td>
            </tr>
        {% endfor %}
    </tbody>
</table>
